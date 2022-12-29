import           Control.Lens 
import           Control.Monad                         (void, forever) 
import qualified Data.ByteString.Char8                 as C 
import           Data.Aeson                            (FromJSON, ToJSON) 
import           Control.Monad.Error.Lens              (catching, throwing, throwing_) 
import           GHC.Generics                          (Generic) 
import qualified PlutusTx                              as PlutusTx 
import           PlutusTx.Prelude
import           Ledger                                hiding (to,initialise)
import qualified Ledger.Ada                            as Ada  
import qualified Ledger.Value                          as V
import           Ledger.Contexts                       as Cont
import           Ledger.Constraints                    (TxConstraints)  
import qualified Ledger.Constraints                    as Constraints 
import qualified Ledger.Typed.Scripts                  as Scripts  
import           Ledger.Typed.Tx                       (tyTxOutData,TypedScriptTxOut(..)) 
import           Plutus.Contract.StateMachine          (AsSMContractError (..), OnChainState, State (..), Void) 
import qualified Plutus.Contract.StateMachine          as SM 
import           Data.Text                             (Text) 
import qualified Data.Text                             as T 
import qualified Data.Map                              as Map
import           Data.String (fromString)
import           Plutus.Contract 
import           Playground.Contract
import           Ledger.AddressMap                    (UtxoMap) 
import           Plutus.Contract.Util                 (loopM) 
import qualified Prelude
import           Prelude                              (String, undefined, show)

-- | This contract was generated automatically using a scribble protocol as source:


-- | Declaration of the possible states for the State Machine:
data HelloBlockchainState =
    None [(Integer,(POSIXTime,Slot))]
    | InitState Integer Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))]
    | ListItemState Integer Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))]
    | BuyItemState Integer Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))]
    deriving stock (Show, Generic)

-- | Declaration of the inputs that will be used for the transitions of the State Machine
data HelloBlockchainInput =
    InitInput Integer Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    | ListItemInput Integer Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    | BuyItemInput Integer Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    deriving stock (Show, Generic)

-- | Make the types possible to use in the "on chain" part of Plutus (State Machine)

PlutusTx.makeLift ''HelloBlockchainState
PlutusTx.makeLift ''HelloBlockchainInput
PlutusTx.unstableMakeIsData ''HelloBlockchainState
PlutusTx.unstableMakeIsData ''HelloBlockchainInput
-- | Declaration of the errors that will be used throughout this contract
data HelloBlockchainError = Error Text
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''HelloBlockchainError


-- | Transforming errors into Text to comply with endpoint definitions
mapSMError' :: Contract w s SM.SMContractError a -> Contract w s Text a
mapSMError' = mapError $ T.pack . show

mapContractError' :: Contract w s ContractError a -> Contract w s Text a
mapContractError' = mapError $ T.pack . show

-- | Contract schema: Endpoint and the parameters they receive
type HelloBlockchainSchema =
        Endpoint "buyitem" ()
        .\/ Endpoint "init" InitParams
        .\/ Endpoint "listitem" ListItemParams


data InitParams = InitParams {
  aPrice :: Integer,
  bPrice :: Integer
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data ListItemParams = ListItemParams {
  name :: String,
  price :: Integer
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

stringToKey :: String -> PaymentPubKeyHash
stringToKey key = PaymentPubKeyHash{unPaymentPubKeyHash = fromString key}

{-# INLINABLE validateKeys #-}
validateKeys :: [PaymentPubKeyHash] -> TxConstraints Void Void
validateKeys keys = if null keys
    then mempty
    else Constraints.mustSatisfyAnyOf $ map (Constraints.mustBeSignedBy) keys

-- | State Machine transactions and client definition
{-# INLINABLE transition #-}
transition :: State HelloBlockchainState -> HelloBlockchainInput -> Maybe (TxConstraints Void Void, State HelloBlockchainState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (None _, InitInput aBalance bBalance itemPrice itemName triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitState aBalance bBalance itemPrice itemName triggerTimeStamps, stateValue = stateVal})
    (InitState _ _ _ _ _, ListItemInput aBalance bBalance itemPrice itemName triggerTimeStamps stateVal) -> Just(mempty, State{stateData = ListItemState aBalance bBalance itemPrice itemName triggerTimeStamps, stateValue = stateVal})
    (ListItemState _ _ _ _ _, BuyItemInput aBalance bBalance itemPrice itemName triggerTimeStamps stateVal) -> Just(mempty, State{stateData = BuyItemState aBalance bBalance itemPrice itemName triggerTimeStamps, stateValue = mempty})
    
    _ -> Nothing

{-# INLINABLE transitionCheck #-}
transitionCheck :: HelloBlockchainState -> HelloBlockchainInput -> ScriptContext -> Bool
transitionCheck state input context = case (state, input) of
    (None triggerTimeStamps, InitInput _ _ _ _ _ _) -> True
    (InitState aBalance bBalance itemPrice itemName triggerTimeStamps, ListItemInput _ _ _ _ _ _) -> True
    (ListItemState aBalance bBalance itemPrice itemName triggerTimeStamps, BuyItemInput _ _ _ _ _ _) -> True
    
    _ -> False
    where
        checkKeys keys = any (txSignedBy $ (scriptContextTxInfo context)) $ map unPaymentPubKeyHash keys

{-# INLINABLE machine #-}
machine :: SM.StateMachine HelloBlockchainState HelloBlockchainInput
machine = SM.StateMachine
        { SM.smTransition = transition
        , SM.smFinal = isFinal
        , SM.smCheck = transitionCheck
        , SM.smThreadToken = Nothing
        }
    where
        isFinal (BuyItemState _ _ _ _ _) = True
        isFinal _       = False

{-# INLINABLE mkValidator #-} 
mkValidator :: Scripts.ValidatorType (SM.StateMachine HelloBlockchainState HelloBlockchainInput)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.TypedValidator (SM.StateMachine HelloBlockchainState HelloBlockchainInput)
scriptInstance = Scripts.mkTypedValidator @(SM.StateMachine HelloBlockchainState HelloBlockchainInput)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @HelloBlockchainState @HelloBlockchainInput

machineInstance :: SM.StateMachineInstance HelloBlockchainState HelloBlockchainInput
machineInstance = SM.StateMachineInstance machine scriptInstance

client :: SM.StateMachineClient HelloBlockchainState HelloBlockchainInput
client = SM.mkStateMachineClient machineInstance

-- | Methods to interact with the state machine

runContractStep :: SM.StateMachineClient HelloBlockchainState HelloBlockchainInput -> HelloBlockchainInput -> TxConstraints HelloBlockchainInput HelloBlockchainState -> Contract () HelloBlockchainSchema SM.SMContractError ( Maybe HelloBlockchainState )
runContractStep client input constraint = 
    do 
    pkh <- ownPaymentPubKeyHash
    r <- SM.runStepWith (Constraints.ownPaymentPubKeyHash pkh) constraint client input 
    case r of 
      SM.TransitionFailure i -> pure Nothing
      SM.TransitionSuccess s -> pure (Just s)

initialiseSM :: Contract () HelloBlockchainSchema SM.SMContractError (Maybe HelloBlockchainState)
initialiseSM = do
    currentState <- getCurrentStateSM client
    case currentState of
        Nothing -> do
          let triggerTimeStamps = []
          SM.runInitialise client (None  triggerTimeStamps) mempty
          pure Nothing
        x -> pure x

getCurrentStateSM :: SM.StateMachineClient HelloBlockchainState HelloBlockchainInput -> Contract () HelloBlockchainSchema SM.SMContractError (Maybe HelloBlockchainState)
getCurrentStateSM client = do
  currentState <- SM.getOnChainState client
  case currentState of
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=state}}, _) -> pure (Just (state))
    _ -> pure Nothing

isValidCallInState :: Maybe HelloBlockchainState -> HelloBlockchainInput -> Bool
isValidCallInState Nothing input = canInitialiseSM input
isValidCallInState (Just state) input = validTransitions state input

canInitialiseSM :: HelloBlockchainInput -> Bool
canInitialiseSM (InitInput _ _ _ _ _ _) = True
canInitialiseSM _ = False

validTransitions :: HelloBlockchainState -> HelloBlockchainInput -> Bool
validTransitions (ListItemState _ _ _ _ _) (BuyItemInput _ _ _ _ _ _) = True
validTransitions (InitState _ _ _ _ _) (ListItemInput _ _ _ _ _ _) = True
validTransitions (None _) (InitInput _ _ _ _ _ _) = True
validTransitions _ _ = False


-- | Some utility functions
fundsAtAddressCondition :: (AsContractError e) => (Value -> Bool)
    -> Address
    -> Contract () HelloBlockchainSchema e (Map.Map TxOutRef ChainIndexTxOut)
fundsAtAddressCondition condition addr = loopM go () where
    go () = do
        cur <- utxosAt addr
        sl <- currentSlot
        let presentVal = foldMap (\(_,a) -> view ciTxOutValue a) $ Map.toList cur
        if condition presentVal
            then pure (Right cur)
            else awaitSlot (sl + 1) >> pure (Left ())

contractAddress :: Ledger.Address
contractAddress = Ledger.scriptAddress (Scripts.validatorScript scriptInstance)

fundsInContract  :: AsContractError e => Contract () HelloBlockchainSchema e (Value)
fundsInContract = do
  utxos <-  utxosAt contractAddress
  return $ foldMap (view ciTxOutValue) $ map snd $ Map.toList utxos

zeroLovelace :: Value 
zeroLovelace = Ada.lovelaceValueOf 0

noKey :: PaymentPubKeyHash
noKey = stringToKey "00000000000000000000000000000000000000000000000000000000"

getContractInfo :: Contract () HelloBlockchainSchema T.Text ([(Integer,(POSIXTime,Slot))])
getContractInfo = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitState _ _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (ListItemState _ _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (BuyItemState _ _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (None triggerTimeStamps) -> pure $ (triggerTimeStamps)

getFields :: Contract () HelloBlockchainSchema T.Text (Integer, Integer, Integer, BuiltinByteString, [(Integer,(POSIXTime,Slot))])
getFields = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitState aBalance bBalance itemPrice itemName triggerTimeStamps) -> pure $ (aBalance, bBalance, itemPrice, itemName, triggerTimeStamps)
        Just (ListItemState aBalance bBalance itemPrice itemName triggerTimeStamps) -> pure $ (aBalance, bBalance, itemPrice, itemName, triggerTimeStamps)
        Just (BuyItemState aBalance bBalance itemPrice itemName triggerTimeStamps) -> pure $ (aBalance, bBalance, itemPrice, itemName, triggerTimeStamps)


-- | Beginning of Endpoint declarations
init :: Promise () HelloBlockchainSchema T.Text ()
init = endpoint @"init" @InitParams $ \(InitParams aPrice bPrice) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (InitInput undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      mapSMError' $ initialiseSM
      oldVal <- fundsInContract
      (triggerTimeStamps) <- getContractInfo
      logic <- initLogic aPrice bPrice 0 0 0 mempty triggerTimeStamps oldVal
      case logic of
        Left (aBalance, bBalance, itemPrice, itemName, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (InitInput aBalance bBalance itemPrice itemName triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint init"

 
listItem :: Promise () HelloBlockchainSchema T.Text ()
listItem = endpoint @"listitem" @ListItemParams $ \(ListItemParams name price) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (ListItemInput undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (aBalanceOld, bBalanceOld, itemPriceOld, itemNameOld, triggerTimeStamps) <- getFields
      logic <- listItemLogic name price aBalanceOld bBalanceOld itemPriceOld itemNameOld triggerTimeStamps oldVal
      case logic of
        Left (aBalance, bBalance, itemPrice, itemName, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (ListItemInput aBalance bBalance itemPrice itemName triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint listItem"

 
buyItem :: Promise () HelloBlockchainSchema T.Text ()
buyItem = endpoint @"buyitem" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (BuyItemInput undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (aBalanceOld, bBalanceOld, itemPriceOld, itemNameOld, triggerTimeStamps) <- getFields
      logic <- buyItemLogic aBalanceOld bBalanceOld itemPriceOld itemNameOld triggerTimeStamps oldVal
      case logic of
        Left (aBalance, bBalance, itemPrice, itemName, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (BuyItemInput aBalance bBalance itemPrice itemName triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint buyItem"

 

-- | Data structure used in the logic functions to track the change of the fields that may be altered from the execution of an endpoint.
data LogicOutput = LogicOutput{
    aBalance :: Integer,
    bBalance :: Integer,
    itemPrice :: Integer,
    itemName :: BuiltinByteString,
    stateVal :: Value,
    constraint :: TxConstraints HelloBlockchainInput HelloBlockchainState
}

-- | Changes the value of the aBalance field.
setABalance :: Integer -> LogicOutput -> LogicOutput
setABalance newABalance output = output{ aBalance = newABalance }

-- | Changes the value of the bBalance field.
setBBalance :: Integer -> LogicOutput -> LogicOutput
setBBalance newBBalance output = output{ bBalance = newBBalance }

-- | Changes the value of the itemPrice field.
setItemPrice :: Integer -> LogicOutput -> LogicOutput
setItemPrice newItemPrice output = output{ itemPrice = newItemPrice }

-- | Changes the value of the itemName field.
setItemName :: BuiltinByteString -> LogicOutput -> LogicOutput
setItemName newItemName output = output{ itemName = newItemName }

-- | Changes the value of the stateVal field.
setStateVal :: Value -> LogicOutput -> LogicOutput
setStateVal newStateVal output = output{ stateVal = newStateVal }

-- | Changes the value of the constraint field.
setConstraint :: TxConstraints HelloBlockchainInput HelloBlockchainState -> LogicOutput -> LogicOutput
setConstraint newConstraint output = output{ constraint = newConstraint }

getOutput :: LogicOutput -> (Integer,Integer,Integer,BuiltinByteString,Value,TxConstraints HelloBlockchainInput HelloBlockchainState)
getOutput out = (aBalance out,bBalance out,itemPrice out,itemName out,stateVal out,constraint out)

getSoloOutput :: LogicOutput -> (Integer,Integer,Integer,BuiltinByteString,Value,TxConstraints HelloBlockchainInput HelloBlockchainState)
getSoloOutput out = (aBalance out,bBalance out,itemPrice out,itemName out,stateVal out,constraint out)

-- | Logic functions (for one of the endpoints and triggers declared in the protocol
-- | TODO: customize this section of the contract!

initLogic :: Integer ->
             Integer ->
             Integer ->
             Integer ->
             Integer ->
             BuiltinByteString ->
             [(Integer, (POSIXTime, Slot))] ->
             Value ->
             Contract ()
                      HelloBlockchainSchema
                      T.Text
                      (Either (Integer,
                               Integer,
                               Integer,
                               BuiltinByteString,
                               Value,
                               TxConstraints HelloBlockchainInput HelloBlockchainState)
                              HelloBlockchainError)
initLogic aPrice bPrice aBalance bBalance itemPrice itemName triggerTimeStamps stateVal = do returnOutputOk $ setABalance aPrice $ setBBalance bPrice $ setStateVal stateVal output
              where output :: LogicOutput
                    output = LogicOutput aBalance bBalance itemPrice itemName stateVal mempty
                    printMsg :: String -> Contract () HelloBlockchainSchema T.Text ()
                    printMsg msg = logInfo @String msg
                    printError :: String -> Contract () HelloBlockchainSchema T.Text ()
                    printError err = logError @String err
                    returnError :: String ->
                                   Contract ()
                                            HelloBlockchainSchema
                                            T.Text
                                            (Either (Integer,
                                                     Integer,
                                                     Integer,
                                                     BuiltinByteString,
                                                     Value,
                                                     TxConstraints HelloBlockchainInput
                                                                   HelloBlockchainState)
                                                    HelloBlockchainError)
                    returnError err = pure $ Right $ Error $ T.pack err
                    returnOk :: Contract ()
                                         HelloBlockchainSchema
                                         T.Text
                                         (Either (Integer,
                                                  Integer,
                                                  Integer,
                                                  BuiltinByteString,
                                                  Value,
                                                  TxConstraints HelloBlockchainInput
                                                                HelloBlockchainState)
                                                 HelloBlockchainError)
                    returnOk = pure $ Left (aBalance,
                                            bBalance,
                                            itemPrice,
                                            itemName,
                                            stateVal,
                                            mempty)
                    returnOutputOk :: LogicOutput ->
                                      Contract ()
                                               HelloBlockchainSchema
                                               T.Text
                                               (Either (Integer,
                                                        Integer,
                                                        Integer,
                                                        BuiltinByteString,
                                                        Value,
                                                        TxConstraints HelloBlockchainInput
                                                                      HelloBlockchainState)
                                                       HelloBlockchainError)
                    returnOutputOk out = pure $ Left $ getOutput out
                    returnOkWith :: Integer ->
                                    Integer ->
                                    Integer ->
                                    BuiltinByteString ->
                                    Value ->
                                    TxConstraints HelloBlockchainInput HelloBlockchainState ->
                                    Contract ()
                                             HelloBlockchainSchema
                                             T.Text
                                             (Either (Integer,
                                                      Integer,
                                                      Integer,
                                                      BuiltinByteString,
                                                      Value,
                                                      TxConstraints HelloBlockchainInput
                                                                    HelloBlockchainState)
                                                     HelloBlockchainError)
                    returnOkWith aBalanceRet bBalanceRet itemPriceRet itemNameRet stateValRet constraintRet = pure $ Left (aBalanceRet,
                                                                                                                           bBalanceRet,
                                                                                                                           itemPriceRet,
                                                                                                                           itemNameRet,
                                                                                                                           stateValRet,
                                                                                                                           constraintRet)

listItemLogic :: String ->
                 Integer ->
                 Integer ->
                 Integer ->
                 Integer ->
                 BuiltinByteString ->
                 [(Integer, (POSIXTime, Slot))] ->
                 Value ->
                 Contract ()
                          HelloBlockchainSchema
                          T.Text
                          (Either (Integer,
                                   Integer,
                                   Integer,
                                   BuiltinByteString,
                                   Value,
                                   TxConstraints HelloBlockchainInput HelloBlockchainState)
                                  HelloBlockchainError)
listItemLogic name price aBalance bBalance itemPrice itemName triggerTimeStamps stateVal = do returnOutputOk $ setABalance aPrice $ setBBalance bPrice $ setStateVal stateVal output
                  where output :: LogicOutput
                        output = LogicOutput aBalance bBalance itemPrice itemName stateVal mempty
                        printMsg :: String -> Contract () HelloBlockchainSchema T.Text ()
                        printMsg msg = logInfo @String msg
                        printError :: String -> Contract () HelloBlockchainSchema T.Text ()
                        printError err = logError @String err
                        returnError :: String ->
                                       Contract ()
                                                HelloBlockchainSchema
                                                T.Text
                                                (Either (Integer,
                                                         Integer,
                                                         Integer,
                                                         BuiltinByteString,
                                                         Value,
                                                         TxConstraints HelloBlockchainInput
                                                                       HelloBlockchainState)
                                                        HelloBlockchainError)
                        returnError err = pure $ Right $ Error $ T.pack err
                        returnOk :: Contract ()
                                             HelloBlockchainSchema
                                             T.Text
                                             (Either (Integer,
                                                      Integer,
                                                      Integer,
                                                      BuiltinByteString,
                                                      Value,
                                                      TxConstraints HelloBlockchainInput
                                                                    HelloBlockchainState)
                                                     HelloBlockchainError)
                        returnOk = pure $ Left (aBalance,
                                                bBalance,
                                                itemPrice,
                                                itemName,
                                                stateVal,
                                                mempty)
                        returnOutputOk :: LogicOutput ->
                                          Contract ()
                                                   HelloBlockchainSchema
                                                   T.Text
                                                   (Either (Integer,
                                                            Integer,
                                                            Integer,
                                                            BuiltinByteString,
                                                            Value,
                                                            TxConstraints HelloBlockchainInput
                                                                          HelloBlockchainState)
                                                           HelloBlockchainError)
                        returnOutputOk out = pure $ Left $ getOutput out
                        returnOkWith :: Integer ->
                                        Integer ->
                                        Integer ->
                                        BuiltinByteString ->
                                        Value ->
                                        TxConstraints HelloBlockchainInput HelloBlockchainState ->
                                        Contract ()
                                                 HelloBlockchainSchema
                                                 T.Text
                                                 (Either (Integer,
                                                          Integer,
                                                          Integer,
                                                          BuiltinByteString,
                                                          Value,
                                                          TxConstraints HelloBlockchainInput
                                                                        HelloBlockchainState)
                                                         HelloBlockchainError)
                        returnOkWith aBalanceRet bBalanceRet itemPriceRet itemNameRet stateValRet constraintRet = pure $ Left (aBalanceRet,
                                                                                                                               bBalanceRet,
                                                                                                                               itemPriceRet,
                                                                                                                               itemNameRet,
                                                                                                                               stateValRet,
                                                                                                                               constraintRet)

buyItemLogic :: Integer ->
                Integer ->
                Integer ->
                BuiltinByteString ->
                [(Integer, (POSIXTime, Slot))] ->
                Value ->
                Contract ()
                         HelloBlockchainSchema
                         T.Text
                         (Either (Integer,
                                  Integer,
                                  Integer,
                                  BuiltinByteString,
                                  Value,
                                  TxConstraints HelloBlockchainInput HelloBlockchainState)
                                 HelloBlockchainError)
buyItemLogic aBalance bBalance itemPrice itemName triggerTimeStamps stateVal = do if bBalance <= itemPrice
                                                                                   then returnError "Party B does not have enough funds."
                                                                                   else returnOutputOk $ setBBalance (bBalance - itemPrice) $ setABalance (aBalance + itemPrice) $ setStateVal stateVal output
                 where output :: LogicOutput
                       output = LogicOutput aBalance bBalance itemPrice itemName stateVal mempty
                       printMsg :: String -> Contract () HelloBlockchainSchema T.Text ()
                       printMsg msg = logInfo @String msg
                       printError :: String -> Contract () HelloBlockchainSchema T.Text ()
                       printError err = logError @String err
                       returnError :: String ->
                                      Contract ()
                                               HelloBlockchainSchema
                                               T.Text
                                               (Either (Integer,
                                                        Integer,
                                                        Integer,
                                                        BuiltinByteString,
                                                        Value,
                                                        TxConstraints HelloBlockchainInput
                                                                      HelloBlockchainState)
                                                       HelloBlockchainError)
                       returnError err = pure $ Right $ Error $ T.pack err
                       returnOk :: Contract ()
                                            HelloBlockchainSchema
                                            T.Text
                                            (Either (Integer,
                                                     Integer,
                                                     Integer,
                                                     BuiltinByteString,
                                                     Value,
                                                     TxConstraints HelloBlockchainInput
                                                                   HelloBlockchainState)
                                                    HelloBlockchainError)
                       returnOk = pure $ Left (aBalance,
                                               bBalance,
                                               itemPrice,
                                               itemName,
                                               stateVal,
                                               mempty)
                       returnOutputOk :: LogicOutput ->
                                         Contract ()
                                                  HelloBlockchainSchema
                                                  T.Text
                                                  (Either (Integer,
                                                           Integer,
                                                           Integer,
                                                           BuiltinByteString,
                                                           Value,
                                                           TxConstraints HelloBlockchainInput
                                                                         HelloBlockchainState)
                                                          HelloBlockchainError)
                       returnOutputOk out = pure $ Left $ getOutput out
                       returnOkWith :: Integer ->
                                       Integer ->
                                       Integer ->
                                       BuiltinByteString ->
                                       Value ->
                                       TxConstraints HelloBlockchainInput HelloBlockchainState ->
                                       Contract ()
                                                HelloBlockchainSchema
                                                T.Text
                                                (Either (Integer,
                                                         Integer,
                                                         Integer,
                                                         BuiltinByteString,
                                                         Value,
                                                         TxConstraints HelloBlockchainInput
                                                                       HelloBlockchainState)
                                                        HelloBlockchainError)
                       returnOkWith aBalanceRet bBalanceRet itemPriceRet itemNameRet stateValRet constraintRet = pure $ Left (aBalanceRet,
                                                                                                                              bBalanceRet,
                                                                                                                              itemPriceRet,
                                                                                                                              itemNameRet,
                                                                                                                              stateValRet,
                                                                                                                              constraintRet)





-- | Defining the contract to run on the playground
contract :: Contract () HelloBlockchainSchema T.Text ()
contract = selectList[init, listItem, buyItem]

endpoints :: Contract () HelloBlockchainSchema T.Text ()
endpoints = forever contract

mkSchemaDefinitions ''HelloBlockchainSchema