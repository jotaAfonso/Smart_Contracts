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
data SimpleMarketPlaceState =
    None [(Integer,(POSIXTime,Slot))]
    | InitialiseState Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))]
    | MakeOfferState Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))]
    | RejectState Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))]
    | AcceptOfferState Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))]
    deriving stock (Show, Generic)

-- | Declaration of the inputs that will be used for the transitions of the State Machine
data SimpleMarketPlaceInput =
    InitialiseInput Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    | MakeOfferInput Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    | RejectInput Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    | AcceptOfferInput Integer Integer BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    deriving stock (Show, Generic)

-- | Make the types possible to use in the "on chain" part of Plutus (State Machine)

PlutusTx.makeLift ''SimpleMarketPlaceState
PlutusTx.makeLift ''SimpleMarketPlaceInput
PlutusTx.unstableMakeIsData ''SimpleMarketPlaceState
PlutusTx.unstableMakeIsData ''SimpleMarketPlaceInput
-- | Declaration of the errors that will be used throughout this contract
data SimpleMarketPlaceError = Error Text
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''SimpleMarketPlaceError


-- | Transforming errors into Text to comply with endpoint definitions
mapSMError' :: Contract w s SM.SMContractError a -> Contract w s Text a
mapSMError' = mapError $ T.pack . show

mapContractError' :: Contract w s ContractError a -> Contract w s Text a
mapContractError' = mapError $ T.pack . show

-- | Contract schema: Endpoint and the parameters they receive
type SimpleMarketPlaceSchema =
        Endpoint "acceptoffer" ()
        .\/ Endpoint "initialise" InitialiseParams
        .\/ Endpoint "makeoffer" MakeOfferParams
        .\/ Endpoint "reject" ()


data InitialiseParams = InitialiseParams {
  description :: String,
  price :: Integer
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data MakeOfferParams = MakeOfferParams {
  offerPriceValue :: Integer
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
transition :: State SimpleMarketPlaceState -> SimpleMarketPlaceInput -> Maybe (TxConstraints Void Void, State SimpleMarketPlaceState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (None _, InitialiseInput askingPrice offerPrice descriptionValue triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitialiseState askingPrice offerPrice descriptionValue triggerTimeStamps, stateValue = stateVal})
    (InitialiseState _ _ _ _, MakeOfferInput askingPrice offerPrice descriptionValue triggerTimeStamps stateVal) -> Just(mempty, State{stateData = MakeOfferState askingPrice offerPrice descriptionValue triggerTimeStamps, stateValue = stateVal})
    (MakeOfferState _ _ _ _, RejectInput askingPrice offerPrice descriptionValue triggerTimeStamps stateVal) -> Just(mempty, State{stateData = RejectState askingPrice offerPrice descriptionValue triggerTimeStamps, stateValue = stateVal})
    (RejectState _ _ _ _, AcceptOfferInput askingPrice offerPrice descriptionValue triggerTimeStamps stateVal) -> Just(mempty, State{stateData = AcceptOfferState askingPrice offerPrice descriptionValue triggerTimeStamps, stateValue = mempty})
    
    _ -> Nothing

{-# INLINABLE transitionCheck #-}
transitionCheck :: SimpleMarketPlaceState -> SimpleMarketPlaceInput -> ScriptContext -> Bool
transitionCheck state input context = case (state, input) of
    (None triggerTimeStamps, InitialiseInput _ _ _ _ _) -> True
    (InitialiseState askingPrice offerPrice descriptionValue triggerTimeStamps, MakeOfferInput _ _ _ _ _) -> True
    (MakeOfferState askingPrice offerPrice descriptionValue triggerTimeStamps, RejectInput _ _ _ _ _) -> True
    (RejectState askingPrice offerPrice descriptionValue triggerTimeStamps, AcceptOfferInput _ _ _ _ _) -> True
    
    _ -> False
    where
        checkKeys keys = any (txSignedBy $ (scriptContextTxInfo context)) $ map unPaymentPubKeyHash keys

{-# INLINABLE machine #-}
machine :: SM.StateMachine SimpleMarketPlaceState SimpleMarketPlaceInput
machine = SM.StateMachine
        { SM.smTransition = transition
        , SM.smFinal = isFinal
        , SM.smCheck = transitionCheck
        , SM.smThreadToken = Nothing
        }
    where
        isFinal (AcceptOfferState _ _ _ _) = True
        isFinal _       = False

{-# INLINABLE mkValidator #-} 
mkValidator :: Scripts.ValidatorType (SM.StateMachine SimpleMarketPlaceState SimpleMarketPlaceInput)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.TypedValidator (SM.StateMachine SimpleMarketPlaceState SimpleMarketPlaceInput)
scriptInstance = Scripts.mkTypedValidator @(SM.StateMachine SimpleMarketPlaceState SimpleMarketPlaceInput)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @SimpleMarketPlaceState @SimpleMarketPlaceInput

machineInstance :: SM.StateMachineInstance SimpleMarketPlaceState SimpleMarketPlaceInput
machineInstance = SM.StateMachineInstance machine scriptInstance

client :: SM.StateMachineClient SimpleMarketPlaceState SimpleMarketPlaceInput
client = SM.mkStateMachineClient machineInstance

-- | Methods to interact with the state machine

runContractStep :: SM.StateMachineClient SimpleMarketPlaceState SimpleMarketPlaceInput -> SimpleMarketPlaceInput -> TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState -> Contract () SimpleMarketPlaceSchema SM.SMContractError ( Maybe SimpleMarketPlaceState )
runContractStep client input constraint = 
    do 
    pkh <- ownPaymentPubKeyHash
    r <- SM.runStepWith (Constraints.ownPaymentPubKeyHash pkh) constraint client input 
    case r of 
      SM.TransitionFailure i -> pure Nothing
      SM.TransitionSuccess s -> pure (Just s)

initialiseSM :: Contract () SimpleMarketPlaceSchema SM.SMContractError (Maybe SimpleMarketPlaceState)
initialiseSM = do
    currentState <- getCurrentStateSM client
    case currentState of
        Nothing -> do
          let triggerTimeStamps = []
          SM.runInitialise client (None  triggerTimeStamps) mempty
          pure Nothing
        x -> pure x

getCurrentStateSM :: SM.StateMachineClient SimpleMarketPlaceState SimpleMarketPlaceInput -> Contract () SimpleMarketPlaceSchema SM.SMContractError (Maybe SimpleMarketPlaceState)
getCurrentStateSM client = do
  currentState <- SM.getOnChainState client
  case currentState of
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=state}}, _) -> pure (Just (state))
    _ -> pure Nothing

isValidCallInState :: Maybe SimpleMarketPlaceState -> SimpleMarketPlaceInput -> Bool
isValidCallInState Nothing input = canInitialiseSM input
isValidCallInState (Just state) input = validTransitions state input

canInitialiseSM :: SimpleMarketPlaceInput -> Bool
canInitialiseSM (InitialiseInput _ _ _ _ _) = True
canInitialiseSM _ = False

validTransitions :: SimpleMarketPlaceState -> SimpleMarketPlaceInput -> Bool
validTransitions (RejectState _ _ _ _) (AcceptOfferInput _ _ _ _ _) = True
validTransitions (MakeOfferState _ _ _ _) (RejectInput _ _ _ _ _) = True
validTransitions (InitialiseState _ _ _ _) (MakeOfferInput _ _ _ _ _) = True
validTransitions (None _) (InitialiseInput _ _ _ _ _) = True
validTransitions _ _ = False


-- | Some utility functions
fundsAtAddressCondition :: (AsContractError e) => (Value -> Bool)
    -> Address
    -> Contract () SimpleMarketPlaceSchema e (Map.Map TxOutRef ChainIndexTxOut)
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

fundsInContract  :: AsContractError e => Contract () SimpleMarketPlaceSchema e (Value)
fundsInContract = do
  utxos <-  utxosAt contractAddress
  return $ foldMap (view ciTxOutValue) $ map snd $ Map.toList utxos

zeroLovelace :: Value 
zeroLovelace = Ada.lovelaceValueOf 0

noKey :: PaymentPubKeyHash
noKey = stringToKey "00000000000000000000000000000000000000000000000000000000"

getContractInfo :: Contract () SimpleMarketPlaceSchema T.Text ([(Integer,(POSIXTime,Slot))])
getContractInfo = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (MakeOfferState _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (RejectState _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (AcceptOfferState _ _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (None triggerTimeStamps) -> pure $ (triggerTimeStamps)

getFields :: Contract () SimpleMarketPlaceSchema T.Text (Integer, Integer, BuiltinByteString, [(Integer,(POSIXTime,Slot))])
getFields = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState askingPrice offerPrice descriptionValue triggerTimeStamps) -> pure $ (askingPrice, offerPrice, descriptionValue, triggerTimeStamps)
        Just (MakeOfferState askingPrice offerPrice descriptionValue triggerTimeStamps) -> pure $ (askingPrice, offerPrice, descriptionValue, triggerTimeStamps)
        Just (RejectState askingPrice offerPrice descriptionValue triggerTimeStamps) -> pure $ (askingPrice, offerPrice, descriptionValue, triggerTimeStamps)
        Just (AcceptOfferState askingPrice offerPrice descriptionValue triggerTimeStamps) -> pure $ (askingPrice, offerPrice, descriptionValue, triggerTimeStamps)


-- | Beginning of Endpoint declarations
initialise :: Promise () SimpleMarketPlaceSchema T.Text ()
initialise = endpoint @"initialise" @InitialiseParams $ \(InitialiseParams description price) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (InitialiseInput undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      mapSMError' $ initialiseSM
      oldVal <- fundsInContract
      (triggerTimeStamps) <- getContractInfo
      logic <- initialiseLogic description price 0 0 mempty triggerTimeStamps oldVal
      case logic of
        Left (askingPrice, offerPrice, descriptionValue, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (InitialiseInput askingPrice offerPrice descriptionValue triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint initialise"

 
makeOffer :: Promise () SimpleMarketPlaceSchema T.Text ()
makeOffer = endpoint @"makeoffer" @MakeOfferParams $ \(MakeOfferParams offerPriceValue) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (MakeOfferInput undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (askingPriceOld, offerPriceOld, descriptionValueOld, triggerTimeStamps) <- getFields
      logic <- makeOfferLogic offerPriceValue askingPriceOld offerPriceOld descriptionValueOld triggerTimeStamps oldVal
      case logic of
        Left (askingPrice, offerPrice, descriptionValue, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (MakeOfferInput askingPrice offerPrice descriptionValue triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint makeOffer"

 
reject :: Promise () SimpleMarketPlaceSchema T.Text ()
reject = endpoint @"reject" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (RejectInput undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (askingPriceOld, offerPriceOld, descriptionValueOld, triggerTimeStamps) <- getFields
      logic <- rejectLogic askingPriceOld offerPriceOld descriptionValueOld triggerTimeStamps oldVal
      case logic of
        Left (askingPrice, offerPrice, descriptionValue, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (RejectInput askingPrice offerPrice descriptionValue triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint reject"

 
acceptOffer :: Promise () SimpleMarketPlaceSchema T.Text ()
acceptOffer = endpoint @"acceptoffer" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (AcceptOfferInput undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (askingPriceOld, offerPriceOld, descriptionValueOld, triggerTimeStamps) <- getFields
      logic <- acceptOfferLogic askingPriceOld offerPriceOld descriptionValueOld triggerTimeStamps oldVal
      case logic of
        Left (askingPrice, offerPrice, descriptionValue, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (AcceptOfferInput askingPrice offerPrice descriptionValue triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint acceptOffer"

 

-- | Data structure used in the logic functions to track the change of the fields that may be altered from the execution of an endpoint.
data LogicOutput = LogicOutput{
    askingPrice :: Integer,
    offerPrice :: Integer,
    descriptionValue :: BuiltinByteString,
    stateVal :: Value,
    constraint :: TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState
}

-- | Changes the value of the askingPrice field.
setAskingPrice :: Integer -> LogicOutput -> LogicOutput
setAskingPrice newAskingPrice output = output{ askingPrice = newAskingPrice }

-- | Changes the value of the offerPrice field.
setOfferPrice :: Integer -> LogicOutput -> LogicOutput
setOfferPrice newOfferPrice output = output{ offerPrice = newOfferPrice }

-- | Changes the value of the descriptionValue field.
setDescriptionValue :: BuiltinByteString -> LogicOutput -> LogicOutput
setDescriptionValue newDescriptionValue output = output{ descriptionValue = newDescriptionValue }

-- | Changes the value of the stateVal field.
setStateVal :: Value -> LogicOutput -> LogicOutput
setStateVal newStateVal output = output{ stateVal = newStateVal }

-- | Changes the value of the constraint field.
setConstraint :: TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState -> LogicOutput -> LogicOutput
setConstraint newConstraint output = output{ constraint = newConstraint }

getOutput :: LogicOutput -> (Integer,Integer,BuiltinByteString,Value,TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState)
getOutput out = (askingPrice out,offerPrice out,descriptionValue out,stateVal out,constraint out)

getSoloOutput :: LogicOutput -> (Integer,Integer,BuiltinByteString,Value,TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState)
getSoloOutput out = (askingPrice out,offerPrice out,descriptionValue out,stateVal out,constraint out)

-- | Logic functions (for one of the endpoints and triggers declared in the protocol
-- | TODO: customize this section of the contract!

initialiseLogic :: String ->
                   Integer ->
                   Integer ->
                   Integer ->
                   BuiltinByteString ->
                   [(Integer, (POSIXTime, Slot))] ->
                   Value ->
                   Contract ()
                            SimpleMarketPlaceSchema
                            T.Text
                            (Either (Integer,
                                     Integer,
                                     BuiltinByteString,
                                     Value,
                                     TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState)
                                    SimpleMarketPlaceError)
initialiseLogic description price askingPrice offerPrice descriptionValue triggerTimeStamps stateVal = do returnOutputOk $ setAskingPrice price $ setDescriptionValue (toBuiltin $ C.pack description) $ setStateVal stateVal output
                    where output :: LogicOutput
                          output = LogicOutput askingPrice offerPrice descriptionValue stateVal mempty
                          printMsg :: String -> Contract () SimpleMarketPlaceSchema T.Text ()
                          printMsg msg = logInfo @String msg
                          printError :: String ->
                                        Contract () SimpleMarketPlaceSchema T.Text ()
                          printError err = logError @String err
                          returnError :: String ->
                                         Contract ()
                                                  SimpleMarketPlaceSchema
                                                  T.Text
                                                  (Either (Integer,
                                                           Integer,
                                                           BuiltinByteString,
                                                           Value,
                                                           TxConstraints SimpleMarketPlaceInput
                                                                         SimpleMarketPlaceState)
                                                          SimpleMarketPlaceError)
                          returnError err = pure $ Right $ Error $ T.pack err
                          returnOk :: Contract ()
                                               SimpleMarketPlaceSchema
                                               T.Text
                                               (Either (Integer,
                                                        Integer,
                                                        BuiltinByteString,
                                                        Value,
                                                        TxConstraints SimpleMarketPlaceInput
                                                                      SimpleMarketPlaceState)
                                                       SimpleMarketPlaceError)
                          returnOk = pure $ Left (askingPrice,
                                                  offerPrice,
                                                  descriptionValue,
                                                  stateVal,
                                                  mempty)
                          returnOutputOk :: LogicOutput ->
                                            Contract ()
                                                     SimpleMarketPlaceSchema
                                                     T.Text
                                                     (Either (Integer,
                                                              Integer,
                                                              BuiltinByteString,
                                                              Value,
                                                              TxConstraints SimpleMarketPlaceInput
                                                                            SimpleMarketPlaceState)
                                                             SimpleMarketPlaceError)
                          returnOutputOk out = pure $ Left $ getOutput out
                          returnOkWith :: Integer ->
                                          Integer ->
                                          BuiltinByteString ->
                                          Value ->
                                          TxConstraints SimpleMarketPlaceInput
                                                        SimpleMarketPlaceState ->
                                          Contract ()
                                                   SimpleMarketPlaceSchema
                                                   T.Text
                                                   (Either (Integer,
                                                            Integer,
                                                            BuiltinByteString,
                                                            Value,
                                                            TxConstraints SimpleMarketPlaceInput
                                                                          SimpleMarketPlaceState)
                                                           SimpleMarketPlaceError)
                          returnOkWith askingPriceRet offerPriceRet descriptionValueRet stateValRet constraintRet = pure $ Left (askingPriceRet,
                                                                                                                                 offerPriceRet,
                                                                                                                                 descriptionValueRet,
                                                                                                                                 stateValRet,
                                                                                                                                 constraintRet)

makeOfferLogic :: Integer ->
                  Integer ->
                  Integer ->
                  BuiltinByteString ->
                  [(Integer, (POSIXTime, Slot))] ->
                  Value ->
                  Contract ()
                           SimpleMarketPlaceSchema
                           T.Text
                           (Either (Integer,
                                    Integer,
                                    BuiltinByteString,
                                    Value,
                                    TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState)
                                   SimpleMarketPlaceError)
makeOfferLogic offerPriceValue askingPrice offerPrice descriptionValue triggerTimeStamps stateVal = do if offerPriceValue <= 0
                                                                                                        then returnError "Offer price low."
                                                                                                        else returnOutputOk $ setOfferPrice offerPriceValue $ setStateVal stateVal output
                   where output :: LogicOutput
                         output = LogicOutput askingPrice offerPrice descriptionValue stateVal mempty
                         printMsg :: String -> Contract () SimpleMarketPlaceSchema T.Text ()
                         printMsg msg = logInfo @String msg
                         printError :: String ->
                                       Contract () SimpleMarketPlaceSchema T.Text ()
                         printError err = logError @String err
                         returnError :: String ->
                                        Contract ()
                                                 SimpleMarketPlaceSchema
                                                 T.Text
                                                 (Either (Integer,
                                                          Integer,
                                                          BuiltinByteString,
                                                          Value,
                                                          TxConstraints SimpleMarketPlaceInput
                                                                        SimpleMarketPlaceState)
                                                         SimpleMarketPlaceError)
                         returnError err = pure $ Right $ Error $ T.pack err
                         returnOk :: Contract ()
                                              SimpleMarketPlaceSchema
                                              T.Text
                                              (Either (Integer,
                                                       Integer,
                                                       BuiltinByteString,
                                                       Value,
                                                       TxConstraints SimpleMarketPlaceInput
                                                                     SimpleMarketPlaceState)
                                                      SimpleMarketPlaceError)
                         returnOk = pure $ Left (askingPrice,
                                                 offerPrice,
                                                 descriptionValue,
                                                 stateVal,
                                                 mempty)
                         returnOutputOk :: LogicOutput ->
                                           Contract ()
                                                    SimpleMarketPlaceSchema
                                                    T.Text
                                                    (Either (Integer,
                                                             Integer,
                                                             BuiltinByteString,
                                                             Value,
                                                             TxConstraints SimpleMarketPlaceInput
                                                                           SimpleMarketPlaceState)
                                                            SimpleMarketPlaceError)
                         returnOutputOk out = pure $ Left $ getOutput out
                         returnOkWith :: Integer ->
                                         Integer ->
                                         BuiltinByteString ->
                                         Value ->
                                         TxConstraints SimpleMarketPlaceInput
                                                       SimpleMarketPlaceState ->
                                         Contract ()
                                                  SimpleMarketPlaceSchema
                                                  T.Text
                                                  (Either (Integer,
                                                           Integer,
                                                           BuiltinByteString,
                                                           Value,
                                                           TxConstraints SimpleMarketPlaceInput
                                                                         SimpleMarketPlaceState)
                                                          SimpleMarketPlaceError)
                         returnOkWith askingPriceRet offerPriceRet descriptionValueRet stateValRet constraintRet = pure $ Left (askingPriceRet,
                                                                                                                                offerPriceRet,
                                                                                                                                descriptionValueRet,
                                                                                                                                stateValRet,
                                                                                                                                constraintRet)

rejectLogic :: Integer ->
               Integer ->
               BuiltinByteString ->
               [(Integer, (POSIXTime, Slot))] ->
               Value ->
               Contract ()
                        SimpleMarketPlaceSchema
                        T.Text
                        (Either (Integer,
                                 Integer,
                                 BuiltinByteString,
                                 Value,
                                 TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState)
                                SimpleMarketPlaceError)
rejectLogic askingPrice offerPrice descriptionValue triggerTimeStamps stateVal = do returnOk
                where output :: LogicOutput
                      output = LogicOutput askingPrice offerPrice descriptionValue stateVal mempty
                      printMsg :: String -> Contract () SimpleMarketPlaceSchema T.Text ()
                      printMsg msg = logInfo @String msg
                      printError :: String ->
                                    Contract () SimpleMarketPlaceSchema T.Text ()
                      printError err = logError @String err
                      returnError :: String ->
                                     Contract ()
                                              SimpleMarketPlaceSchema
                                              T.Text
                                              (Either (Integer,
                                                       Integer,
                                                       BuiltinByteString,
                                                       Value,
                                                       TxConstraints SimpleMarketPlaceInput
                                                                     SimpleMarketPlaceState)
                                                      SimpleMarketPlaceError)
                      returnError err = pure $ Right $ Error $ T.pack err
                      returnOk :: Contract ()
                                           SimpleMarketPlaceSchema
                                           T.Text
                                           (Either (Integer,
                                                    Integer,
                                                    BuiltinByteString,
                                                    Value,
                                                    TxConstraints SimpleMarketPlaceInput
                                                                  SimpleMarketPlaceState)
                                                   SimpleMarketPlaceError)
                      returnOk = pure $ Left (askingPrice,
                                              offerPrice,
                                              descriptionValue,
                                              stateVal,
                                              mempty)
                      returnOutputOk :: LogicOutput ->
                                        Contract ()
                                                 SimpleMarketPlaceSchema
                                                 T.Text
                                                 (Either (Integer,
                                                          Integer,
                                                          BuiltinByteString,
                                                          Value,
                                                          TxConstraints SimpleMarketPlaceInput
                                                                        SimpleMarketPlaceState)
                                                         SimpleMarketPlaceError)
                      returnOutputOk out = pure $ Left $ getOutput out
                      returnOkWith :: Integer ->
                                      Integer ->
                                      BuiltinByteString ->
                                      Value ->
                                      TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState ->
                                      Contract ()
                                               SimpleMarketPlaceSchema
                                               T.Text
                                               (Either (Integer,
                                                        Integer,
                                                        BuiltinByteString,
                                                        Value,
                                                        TxConstraints SimpleMarketPlaceInput
                                                                      SimpleMarketPlaceState)
                                                       SimpleMarketPlaceError)
                      returnOkWith askingPriceRet offerPriceRet descriptionValueRet stateValRet constraintRet = pure $ Left (askingPriceRet,
                                                                                                                             offerPriceRet,
                                                                                                                             descriptionValueRet,
                                                                                                                             stateValRet,
                                                                                                                             constraintRet)

acceptOfferLogic :: Integer ->
                    Integer ->
                    BuiltinByteString ->
                    [(Integer, (POSIXTime, Slot))] ->
                    Value ->
                    Contract ()
                             SimpleMarketPlaceSchema
                             T.Text
                             (Either (Integer,
                                      Integer,
                                      BuiltinByteString,
                                      Value,
                                      TxConstraints SimpleMarketPlaceInput SimpleMarketPlaceState)
                                     SimpleMarketPlaceError)
acceptOfferLogic askingPrice offerPrice descriptionValue triggerTimeStamps stateVal = do returnOk
                     where output :: LogicOutput
                           output = LogicOutput askingPrice offerPrice descriptionValue stateVal mempty
                           printMsg :: String -> Contract () SimpleMarketPlaceSchema T.Text ()
                           printMsg msg = logInfo @String msg
                           printError :: String ->
                                         Contract () SimpleMarketPlaceSchema T.Text ()
                           printError err = logError @String err
                           returnError :: String ->
                                          Contract ()
                                                   SimpleMarketPlaceSchema
                                                   T.Text
                                                   (Either (Integer,
                                                            Integer,
                                                            BuiltinByteString,
                                                            Value,
                                                            TxConstraints SimpleMarketPlaceInput
                                                                          SimpleMarketPlaceState)
                                                           SimpleMarketPlaceError)
                           returnError err = pure $ Right $ Error $ T.pack err
                           returnOk :: Contract ()
                                                SimpleMarketPlaceSchema
                                                T.Text
                                                (Either (Integer,
                                                         Integer,
                                                         BuiltinByteString,
                                                         Value,
                                                         TxConstraints SimpleMarketPlaceInput
                                                                       SimpleMarketPlaceState)
                                                        SimpleMarketPlaceError)
                           returnOk = pure $ Left (askingPrice,
                                                   offerPrice,
                                                   descriptionValue,
                                                   stateVal,
                                                   mempty)
                           returnOutputOk :: LogicOutput ->
                                             Contract ()
                                                      SimpleMarketPlaceSchema
                                                      T.Text
                                                      (Either (Integer,
                                                               Integer,
                                                               BuiltinByteString,
                                                               Value,
                                                               TxConstraints SimpleMarketPlaceInput
                                                                             SimpleMarketPlaceState)
                                                              SimpleMarketPlaceError)
                           returnOutputOk out = pure $ Left $ getOutput out
                           returnOkWith :: Integer ->
                                           Integer ->
                                           BuiltinByteString ->
                                           Value ->
                                           TxConstraints SimpleMarketPlaceInput
                                                         SimpleMarketPlaceState ->
                                           Contract ()
                                                    SimpleMarketPlaceSchema
                                                    T.Text
                                                    (Either (Integer,
                                                             Integer,
                                                             BuiltinByteString,
                                                             Value,
                                                             TxConstraints SimpleMarketPlaceInput
                                                                           SimpleMarketPlaceState)
                                                            SimpleMarketPlaceError)
                           returnOkWith askingPriceRet offerPriceRet descriptionValueRet stateValRet constraintRet = pure $ Left (askingPriceRet,
                                                                                                                                  offerPriceRet,
                                                                                                                                  descriptionValueRet,
                                                                                                                                  stateValRet,
                                                                                                                                  constraintRet)





-- | Defining the contract to run on the playground
contract :: Contract () SimpleMarketPlaceSchema T.Text ()
contract = selectList[initialise, makeOffer, reject, acceptOffer]

endpoints :: Contract () SimpleMarketPlaceSchema T.Text ()
endpoints = forever contract

mkSchemaDefinitions ''SimpleMarketPlaceSchema