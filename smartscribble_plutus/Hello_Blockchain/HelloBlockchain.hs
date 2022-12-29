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
    | InitState BuiltinByteString [(Integer,(POSIXTime,Slot))]
    | SendRespondState BuiltinByteString [(Integer,(POSIXTime,Slot))]
    deriving stock (Show, Generic)

-- | Declaration of the inputs that will be used for the transitions of the State Machine
data HelloBlockchainInput =
    InitInput BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    | SendRespondInput BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    | SendRequestInput BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
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
        Endpoint "init" InitParams
        .\/ Endpoint "sendrequest" SendRequestParams
        .\/ Endpoint "sendrespond" SendRespondParams


data InitParams = InitParams {
  msg :: String
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data SendRequestParams = SendRequestParams {
  msgRequest :: String
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data SendRespondParams = SendRespondParams {
  msgRespond :: String
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
    (None _, InitInput message triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitState message triggerTimeStamps, stateValue = stateVal})
    (InitState _ _, SendRespondInput message triggerTimeStamps stateVal) -> Just(mempty, State{stateData = SendRespondState message triggerTimeStamps, stateValue = stateVal})
    (SendRespondState _ _, SendRequestInput message triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitState message triggerTimeStamps, stateValue = stateVal})
    
    
    _ -> Nothing

{-# INLINABLE transitionCheck #-}
transitionCheck :: HelloBlockchainState -> HelloBlockchainInput -> ScriptContext -> Bool
transitionCheck state input context = case (state, input) of
    (None triggerTimeStamps, InitInput _ _ _) -> True
    (InitState message triggerTimeStamps, SendRespondInput _ _ _) -> True
    (SendRespondState message triggerTimeStamps, SendRequestInput _ _ _) -> True
    
    
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
canInitialiseSM (InitInput _ _ _) = True
canInitialiseSM _ = False

validTransitions :: HelloBlockchainState -> HelloBlockchainInput -> Bool
validTransitions (SendRespondState _ _) (SendRequestInput _ _ _) = True
validTransitions (InitState _ _) (SendRespondInput _ _ _) = True
validTransitions (None _) (InitInput _ _ _) = True
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
        Just (InitState _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (SendRespondState _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (None triggerTimeStamps) -> pure $ (triggerTimeStamps)

getFields :: Contract () HelloBlockchainSchema T.Text (BuiltinByteString, [(Integer,(POSIXTime,Slot))])
getFields = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitState message triggerTimeStamps) -> pure $ (message, triggerTimeStamps)
        Just (SendRespondState message triggerTimeStamps) -> pure $ (message, triggerTimeStamps)


-- | Beginning of Endpoint declarations
init :: Promise () HelloBlockchainSchema T.Text ()
init = endpoint @"init" @InitParams $ \(InitParams msg) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (InitInput undefined undefined undefined) then do
    -- | Calling business logic function...
      mapSMError' $ initialiseSM
      oldVal <- fundsInContract
      (triggerTimeStamps) <- getContractInfo
      logic <- initLogic msg mempty triggerTimeStamps oldVal
      case logic of
        Left (message, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (InitInput message triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint init"

 
sendRespond :: Promise () HelloBlockchainSchema T.Text ()
sendRespond = endpoint @"sendrespond" @SendRespondParams $ \(SendRespondParams msgRespond) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (SendRespondInput undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (messageOld, triggerTimeStamps) <- getFields
      logic <- sendRespondLogic msgRespond messageOld triggerTimeStamps oldVal
      case logic of
        Left (message, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (SendRespondInput message triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint sendRespond"

 
sendRequest :: Promise () HelloBlockchainSchema T.Text ()
sendRequest = endpoint @"sendrequest" @SendRequestParams $ \(SendRequestParams msgRequest) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (SendRequestInput undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (messageOld, triggerTimeStamps) <- getFields
      logic <- sendRequestLogic msgRequest messageOld triggerTimeStamps oldVal
      case logic of
        Left (message, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (SendRequestInput message triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint sendRequest"

 

-- | Data structure used in the logic functions to track the change of the fields that may be altered from the execution of an endpoint.
data LogicOutput = LogicOutput{
    message :: BuiltinByteString,
    stateVal :: Value,
    constraint :: TxConstraints HelloBlockchainInput HelloBlockchainState
}

-- | Changes the value of the message field.
setMessage :: BuiltinByteString -> LogicOutput -> LogicOutput
setMessage newMessage output = output{ message = newMessage }

-- | Changes the value of the stateVal field.
setStateVal :: Value -> LogicOutput -> LogicOutput
setStateVal newStateVal output = output{ stateVal = newStateVal }

-- | Changes the value of the constraint field.
setConstraint :: TxConstraints HelloBlockchainInput HelloBlockchainState -> LogicOutput -> LogicOutput
setConstraint newConstraint output = output{ constraint = newConstraint }

getOutput :: LogicOutput -> (BuiltinByteString,Value,TxConstraints HelloBlockchainInput HelloBlockchainState)
getOutput out = (message out,stateVal out,constraint out)

getSoloOutput :: LogicOutput -> (BuiltinByteString,Value,TxConstraints HelloBlockchainInput HelloBlockchainState)
getSoloOutput out = (message out,stateVal out,constraint out)

-- | Logic functions (for one of the endpoints and triggers declared in the protocol
-- | TODO: customize this section of the contract!

initLogic :: String ->
             BuiltinByteString ->
             [(Integer, (POSIXTime, Slot))] ->
             Value ->
             Contract ()
                      HelloBlockchainSchema
                      T.Text
                      (Either (BuiltinByteString,
                               Value,
                               TxConstraints HelloBlockchainInput HelloBlockchainState)
                              HelloBlockchainError)
initLogic msg message triggerTimeStamps stateVal = do returnOutputOk $ setMessage (toBuiltin $ C.pack msg) $ setStateVal stateVal output
              where output :: LogicOutput
                    output = LogicOutput message stateVal mempty
                    printMsg :: String -> Contract () HelloBlockchainSchema T.Text ()
                    printMsg msg = logInfo @String msg
                    printError :: String -> Contract () HelloBlockchainSchema T.Text ()
                    printError err = logError @String err
                    returnError :: String ->
                                   Contract ()
                                            HelloBlockchainSchema
                                            T.Text
                                            (Either (BuiltinByteString,
                                                     Value,
                                                     TxConstraints HelloBlockchainInput
                                                                   HelloBlockchainState)
                                                    HelloBlockchainError)
                    returnError err = pure $ Right $ Error $ T.pack err
                    returnOk :: Contract ()
                                         HelloBlockchainSchema
                                         T.Text
                                         (Either (BuiltinByteString,
                                                  Value,
                                                  TxConstraints HelloBlockchainInput
                                                                HelloBlockchainState)
                                                 HelloBlockchainError)
                    returnOk = pure $ Left (message, stateVal, mempty)
                    returnOutputOk :: LogicOutput ->
                                      Contract ()
                                               HelloBlockchainSchema
                                               T.Text
                                               (Either (BuiltinByteString,
                                                        Value,
                                                        TxConstraints HelloBlockchainInput
                                                                      HelloBlockchainState)
                                                       HelloBlockchainError)
                    returnOutputOk out = pure $ Left $ getOutput out
                    returnOkWith :: BuiltinByteString ->
                                    Value ->
                                    TxConstraints HelloBlockchainInput HelloBlockchainState ->
                                    Contract ()
                                             HelloBlockchainSchema
                                             T.Text
                                             (Either (BuiltinByteString,
                                                      Value,
                                                      TxConstraints HelloBlockchainInput
                                                                    HelloBlockchainState)
                                                     HelloBlockchainError)
                    returnOkWith messageRet stateValRet constraintRet = pure $ Left (messageRet,
                                                                                     stateValRet,
                                                                                     constraintRet)

sendRespondLogic :: String ->
                    BuiltinByteString ->
                    [(Integer, (POSIXTime, Slot))] ->
                    Value ->
                    Contract ()
                             HelloBlockchainSchema
                             T.Text
                             (Either (BuiltinByteString,
                                      Value,
                                      TxConstraints HelloBlockchainInput HelloBlockchainState)
                                     HelloBlockchainError)
sendRespondLogic msgRespond message triggerTimeStamps stateVal = do returnOutputOk $ setMessage (toBuiltin $ C.pack msgRespond) $ setStateVal stateVal output
                     where output :: LogicOutput
                           output = LogicOutput message stateVal mempty
                           printMsg :: String -> Contract () HelloBlockchainSchema T.Text ()
                           printMsg msg = logInfo @String msg
                           printError :: String -> Contract () HelloBlockchainSchema T.Text ()
                           printError err = logError @String err
                           returnError :: String ->
                                          Contract ()
                                                   HelloBlockchainSchema
                                                   T.Text
                                                   (Either (BuiltinByteString,
                                                            Value,
                                                            TxConstraints HelloBlockchainInput
                                                                          HelloBlockchainState)
                                                           HelloBlockchainError)
                           returnError err = pure $ Right $ Error $ T.pack err
                           returnOk :: Contract ()
                                                HelloBlockchainSchema
                                                T.Text
                                                (Either (BuiltinByteString,
                                                         Value,
                                                         TxConstraints HelloBlockchainInput
                                                                       HelloBlockchainState)
                                                        HelloBlockchainError)
                           returnOk = pure $ Left (message, stateVal, mempty)
                           returnOutputOk :: LogicOutput ->
                                             Contract ()
                                                      HelloBlockchainSchema
                                                      T.Text
                                                      (Either (BuiltinByteString,
                                                               Value,
                                                               TxConstraints HelloBlockchainInput
                                                                             HelloBlockchainState)
                                                              HelloBlockchainError)
                           returnOutputOk out = pure $ Left $ getOutput out
                           returnOkWith :: BuiltinByteString ->
                                           Value ->
                                           TxConstraints HelloBlockchainInput
                                                         HelloBlockchainState ->
                                           Contract ()
                                                    HelloBlockchainSchema
                                                    T.Text
                                                    (Either (BuiltinByteString,
                                                             Value,
                                                             TxConstraints HelloBlockchainInput
                                                                           HelloBlockchainState)
                                                            HelloBlockchainError)
                           returnOkWith messageRet stateValRet constraintRet = pure $ Left (messageRet,
                                                                                            stateValRet,
                                                                                            constraintRet)

sendRequestLogic :: String ->
                    BuiltinByteString ->
                    [(Integer, (POSIXTime, Slot))] ->
                    Value ->
                    Contract ()
                             HelloBlockchainSchema
                             T.Text
                             (Either (BuiltinByteString,
                                      Value,
                                      TxConstraints HelloBlockchainInput HelloBlockchainState)
                                     HelloBlockchainError)
sendRequestLogic msgRequest message triggerTimeStamps stateVal = do returnOutputOk $ setMessage (toBuiltin $ C.pack msgRequest) $ setStateVal stateVal output
                     where output :: LogicOutput
                           output = LogicOutput message stateVal mempty
                           printMsg :: String -> Contract () HelloBlockchainSchema T.Text ()
                           printMsg msg = logInfo @String msg
                           printError :: String -> Contract () HelloBlockchainSchema T.Text ()
                           printError err = logError @String err
                           returnError :: String ->
                                          Contract ()
                                                   HelloBlockchainSchema
                                                   T.Text
                                                   (Either (BuiltinByteString,
                                                            Value,
                                                            TxConstraints HelloBlockchainInput
                                                                          HelloBlockchainState)
                                                           HelloBlockchainError)
                           returnError err = pure $ Right $ Error $ T.pack err
                           returnOk :: Contract ()
                                                HelloBlockchainSchema
                                                T.Text
                                                (Either (BuiltinByteString,
                                                         Value,
                                                         TxConstraints HelloBlockchainInput
                                                                       HelloBlockchainState)
                                                        HelloBlockchainError)
                           returnOk = pure $ Left (message, stateVal, mempty)
                           returnOutputOk :: LogicOutput ->
                                             Contract ()
                                                      HelloBlockchainSchema
                                                      T.Text
                                                      (Either (BuiltinByteString,
                                                               Value,
                                                               TxConstraints HelloBlockchainInput
                                                                             HelloBlockchainState)
                                                              HelloBlockchainError)
                           returnOutputOk out = pure $ Left $ getOutput out
                           returnOkWith :: BuiltinByteString ->
                                           Value ->
                                           TxConstraints HelloBlockchainInput
                                                         HelloBlockchainState ->
                                           Contract ()
                                                    HelloBlockchainSchema
                                                    T.Text
                                                    (Either (BuiltinByteString,
                                                             Value,
                                                             TxConstraints HelloBlockchainInput
                                                                           HelloBlockchainState)
                                                            HelloBlockchainError)
                           returnOkWith messageRet stateValRet constraintRet = pure $ Left (messageRet,
                                                                                            stateValRet,
                                                                                            constraintRet)





-- | Defining the contract to run on the playground
contract :: Contract () HelloBlockchainSchema T.Text ()
contract = selectList[init, sendRespond, sendRequest]

endpoints :: Contract () HelloBlockchainSchema T.Text ()
endpoints = forever contract

mkSchemaDefinitions ''HelloBlockchainSchema