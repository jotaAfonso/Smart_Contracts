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
data RoomThermostatState =
    None [(Integer,(POSIXTime,Slot))]
    | InitState BuiltinByteString Integer [(Integer,(POSIXTime,Slot))]
    | StartThermostatState BuiltinByteString Integer [(Integer,(POSIXTime,Slot))]
    | SetTargetTemperatureState BuiltinByteString Integer [(Integer,(POSIXTime,Slot))]
    | SetModeState BuiltinByteString Integer [(Integer,(POSIXTime,Slot))]
    deriving stock (Show, Generic)

-- | Declaration of the inputs that will be used for the transitions of the State Machine
data RoomThermostatInput =
    InitInput BuiltinByteString Integer [(Integer,(POSIXTime,Slot))] Value
    | StartThermostatInput BuiltinByteString Integer [(Integer,(POSIXTime,Slot))] Value
    | SetTargetTemperatureInput BuiltinByteString Integer [(Integer,(POSIXTime,Slot))] Value
    | SetModeInput BuiltinByteString Integer [(Integer,(POSIXTime,Slot))] Value
    deriving stock (Show, Generic)

-- | Make the types possible to use in the "on chain" part of Plutus (State Machine)

PlutusTx.makeLift ''RoomThermostatState
PlutusTx.makeLift ''RoomThermostatInput
PlutusTx.unstableMakeIsData ''RoomThermostatState
PlutusTx.unstableMakeIsData ''RoomThermostatInput
-- | Declaration of the errors that will be used throughout this contract
data RoomThermostatError = Error Text
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''RoomThermostatError


-- | Transforming errors into Text to comply with endpoint definitions
mapSMError' :: Contract w s SM.SMContractError a -> Contract w s Text a
mapSMError' = mapError $ T.pack . show

mapContractError' :: Contract w s ContractError a -> Contract w s Text a
mapContractError' = mapError $ T.pack . show

-- | Contract schema: Endpoint and the parameters they receive
type RoomThermostatSchema =
        Endpoint "init" InitParams
        .\/ Endpoint "setmode" SetModeParams
        .\/ Endpoint "settargettemperature" SetTargetTemperatureParams
        .\/ Endpoint "startthermostat" ()


data InitParams = InitParams {
  tempInit :: Integer
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data SetModeParams = SetModeParams {
  targetMode :: String
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data SetTargetTemperatureParams = SetTargetTemperatureParams {
  targetTemp :: Integer
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
transition :: State RoomThermostatState -> RoomThermostatInput -> Maybe (TxConstraints Void Void, State RoomThermostatState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (None _, InitInput modeValue temperatureValue triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitState modeValue temperatureValue triggerTimeStamps, stateValue = stateVal})
    (InitState _ _ _, StartThermostatInput modeValue temperatureValue triggerTimeStamps stateVal) -> Just(mempty, State{stateData = StartThermostatState modeValue temperatureValue triggerTimeStamps, stateValue = stateVal})
    (StartThermostatState _ _ _, SetTargetTemperatureInput modeValue temperatureValue triggerTimeStamps stateVal) -> Just(mempty, State{stateData = SetTargetTemperatureState modeValue temperatureValue triggerTimeStamps, stateValue = stateVal})
    (SetTargetTemperatureState _ _ _, SetModeInput modeValue temperatureValue triggerTimeStamps stateVal) -> Just(mempty, State{stateData = SetModeState modeValue temperatureValue triggerTimeStamps, stateValue = mempty})
    
    _ -> Nothing

{-# INLINABLE transitionCheck #-}
transitionCheck :: RoomThermostatState -> RoomThermostatInput -> ScriptContext -> Bool
transitionCheck state input context = case (state, input) of
    (None triggerTimeStamps, InitInput _ _ _ _) -> True
    (InitState modeValue temperatureValue triggerTimeStamps, StartThermostatInput _ _ _ _) -> True
    (StartThermostatState modeValue temperatureValue triggerTimeStamps, SetTargetTemperatureInput _ _ _ _) -> True
    (SetTargetTemperatureState modeValue temperatureValue triggerTimeStamps, SetModeInput _ _ _ _) -> True
    
    _ -> False
    where
        checkKeys keys = any (txSignedBy $ (scriptContextTxInfo context)) $ map unPaymentPubKeyHash keys

{-# INLINABLE machine #-}
machine :: SM.StateMachine RoomThermostatState RoomThermostatInput
machine = SM.StateMachine
        { SM.smTransition = transition
        , SM.smFinal = isFinal
        , SM.smCheck = transitionCheck
        , SM.smThreadToken = Nothing
        }
    where
        isFinal (SetModeState _ _ _) = True
        isFinal _       = False

{-# INLINABLE mkValidator #-} 
mkValidator :: Scripts.ValidatorType (SM.StateMachine RoomThermostatState RoomThermostatInput)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.TypedValidator (SM.StateMachine RoomThermostatState RoomThermostatInput)
scriptInstance = Scripts.mkTypedValidator @(SM.StateMachine RoomThermostatState RoomThermostatInput)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @RoomThermostatState @RoomThermostatInput

machineInstance :: SM.StateMachineInstance RoomThermostatState RoomThermostatInput
machineInstance = SM.StateMachineInstance machine scriptInstance

client :: SM.StateMachineClient RoomThermostatState RoomThermostatInput
client = SM.mkStateMachineClient machineInstance

-- | Methods to interact with the state machine

runContractStep :: SM.StateMachineClient RoomThermostatState RoomThermostatInput -> RoomThermostatInput -> TxConstraints RoomThermostatInput RoomThermostatState -> Contract () RoomThermostatSchema SM.SMContractError ( Maybe RoomThermostatState )
runContractStep client input constraint = 
    do 
    pkh <- ownPaymentPubKeyHash
    r <- SM.runStepWith (Constraints.ownPaymentPubKeyHash pkh) constraint client input 
    case r of 
      SM.TransitionFailure i -> pure Nothing
      SM.TransitionSuccess s -> pure (Just s)

initialiseSM :: Contract () RoomThermostatSchema SM.SMContractError (Maybe RoomThermostatState)
initialiseSM = do
    currentState <- getCurrentStateSM client
    case currentState of
        Nothing -> do
          let triggerTimeStamps = []
          SM.runInitialise client (None  triggerTimeStamps) mempty
          pure Nothing
        x -> pure x

getCurrentStateSM :: SM.StateMachineClient RoomThermostatState RoomThermostatInput -> Contract () RoomThermostatSchema SM.SMContractError (Maybe RoomThermostatState)
getCurrentStateSM client = do
  currentState <- SM.getOnChainState client
  case currentState of
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=state}}, _) -> pure (Just (state))
    _ -> pure Nothing

isValidCallInState :: Maybe RoomThermostatState -> RoomThermostatInput -> Bool
isValidCallInState Nothing input = canInitialiseSM input
isValidCallInState (Just state) input = validTransitions state input

canInitialiseSM :: RoomThermostatInput -> Bool
canInitialiseSM (InitInput _ _ _ _) = True
canInitialiseSM _ = False

validTransitions :: RoomThermostatState -> RoomThermostatInput -> Bool
validTransitions (SetTargetTemperatureState _ _ _) (SetModeInput _ _ _ _) = True
validTransitions (StartThermostatState _ _ _) (SetTargetTemperatureInput _ _ _ _) = True
validTransitions (InitState _ _ _) (StartThermostatInput _ _ _ _) = True
validTransitions (None _) (InitInput _ _ _ _) = True
validTransitions _ _ = False


-- | Some utility functions
fundsAtAddressCondition :: (AsContractError e) => (Value -> Bool)
    -> Address
    -> Contract () RoomThermostatSchema e (Map.Map TxOutRef ChainIndexTxOut)
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

fundsInContract  :: AsContractError e => Contract () RoomThermostatSchema e (Value)
fundsInContract = do
  utxos <-  utxosAt contractAddress
  return $ foldMap (view ciTxOutValue) $ map snd $ Map.toList utxos

zeroLovelace :: Value 
zeroLovelace = Ada.lovelaceValueOf 0

noKey :: PaymentPubKeyHash
noKey = stringToKey "00000000000000000000000000000000000000000000000000000000"

getContractInfo :: Contract () RoomThermostatSchema T.Text ([(Integer,(POSIXTime,Slot))])
getContractInfo = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitState _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (StartThermostatState _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (SetTargetTemperatureState _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (SetModeState _ _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (None triggerTimeStamps) -> pure $ (triggerTimeStamps)

getFields :: Contract () RoomThermostatSchema T.Text (BuiltinByteString, Integer, [(Integer,(POSIXTime,Slot))])
getFields = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitState modeValue temperatureValue triggerTimeStamps) -> pure $ (modeValue, temperatureValue, triggerTimeStamps)
        Just (StartThermostatState modeValue temperatureValue triggerTimeStamps) -> pure $ (modeValue, temperatureValue, triggerTimeStamps)
        Just (SetTargetTemperatureState modeValue temperatureValue triggerTimeStamps) -> pure $ (modeValue, temperatureValue, triggerTimeStamps)
        Just (SetModeState modeValue temperatureValue triggerTimeStamps) -> pure $ (modeValue, temperatureValue, triggerTimeStamps)


-- | Beginning of Endpoint declarations
init :: Promise () RoomThermostatSchema T.Text ()
init = endpoint @"init" @InitParams $ \(InitParams tempInit) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (InitInput undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      mapSMError' $ initialiseSM
      oldVal <- fundsInContract
      (triggerTimeStamps) <- getContractInfo
      logic <- initLogic tempInit mempty 0 triggerTimeStamps oldVal
      case logic of
        Left (modeValue, temperatureValue, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (InitInput modeValue temperatureValue triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint init"

 
startThermostat :: Promise () RoomThermostatSchema T.Text ()
startThermostat = endpoint @"startthermostat" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (StartThermostatInput undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (modeValueOld, temperatureValueOld, triggerTimeStamps) <- getFields
      logic <- startThermostatLogic modeValueOld temperatureValueOld triggerTimeStamps oldVal
      case logic of
        Left (modeValue, temperatureValue, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (StartThermostatInput modeValue temperatureValue triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint startThermostat"

 
setTargetTemperature :: Promise () RoomThermostatSchema T.Text ()
setTargetTemperature = endpoint @"settargettemperature" @SetTargetTemperatureParams $ \(SetTargetTemperatureParams targetTemp) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (SetTargetTemperatureInput undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (modeValueOld, temperatureValueOld, triggerTimeStamps) <- getFields
      logic <- setTargetTemperatureLogic targetTemp modeValueOld temperatureValueOld triggerTimeStamps oldVal
      case logic of
        Left (modeValue, temperatureValue, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (SetTargetTemperatureInput modeValue temperatureValue triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint setTargetTemperature"

 
setMode :: Promise () RoomThermostatSchema T.Text ()
setMode = endpoint @"setmode" @SetModeParams $ \(SetModeParams targetMode) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (SetModeInput undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (modeValueOld, temperatureValueOld, triggerTimeStamps) <- getFields
      logic <- setModeLogic targetMode modeValueOld temperatureValueOld triggerTimeStamps oldVal
      case logic of
        Left (modeValue, temperatureValue, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (SetModeInput modeValue temperatureValue triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint setMode"

 

-- | Data structure used in the logic functions to track the change of the fields that may be altered from the execution of an endpoint.
data LogicOutput = LogicOutput{
    modeValue :: BuiltinByteString,
    temperatureValue :: Integer,
    stateVal :: Value,
    constraint :: TxConstraints RoomThermostatInput RoomThermostatState
}

-- | Changes the value of the modeValue field.
setModeValue :: BuiltinByteString -> LogicOutput -> LogicOutput
setModeValue newModeValue output = output{ modeValue = newModeValue }

-- | Changes the value of the temperatureValue field.
setTemperatureValue :: Integer -> LogicOutput -> LogicOutput
setTemperatureValue newTemperatureValue output = output{ temperatureValue = newTemperatureValue }

-- | Changes the value of the stateVal field.
setStateVal :: Value -> LogicOutput -> LogicOutput
setStateVal newStateVal output = output{ stateVal = newStateVal }

-- | Changes the value of the constraint field.
setConstraint :: TxConstraints RoomThermostatInput RoomThermostatState -> LogicOutput -> LogicOutput
setConstraint newConstraint output = output{ constraint = newConstraint }

getOutput :: LogicOutput -> (BuiltinByteString,Integer,Value,TxConstraints RoomThermostatInput RoomThermostatState)
getOutput out = (modeValue out,temperatureValue out,stateVal out,constraint out)

getSoloOutput :: LogicOutput -> (BuiltinByteString,Integer,Value,TxConstraints RoomThermostatInput RoomThermostatState)
getSoloOutput out = (modeValue out,temperatureValue out,stateVal out,constraint out)

-- | Logic functions (for one of the endpoints and triggers declared in the protocol
-- | TODO: customize this section of the contract!

initLogic :: Integer ->
             BuiltinByteString ->
             Integer ->
             [(Integer, (POSIXTime, Slot))] ->
             Value ->
             Contract ()
                      RoomThermostatSchema
                      T.Text
                      (Either (BuiltinByteString,
                               Integer,
                               Value,
                               TxConstraints RoomThermostatInput RoomThermostatState)
                              RoomThermostatError)
initLogic tempInit modeValue temperatureValue triggerTimeStamps stateVal = do returnOutputOk $ setTemperatureValue tempInit $ setStateVal stateVal output
              where output :: LogicOutput
                    output = LogicOutput modeValue temperatureValue stateVal mempty
                    printMsg :: String -> Contract () RoomThermostatSchema T.Text ()
                    printMsg msg = logInfo @String msg
                    printError :: String -> Contract () RoomThermostatSchema T.Text ()
                    printError err = logError @String err
                    returnError :: String ->
                                   Contract ()
                                            RoomThermostatSchema
                                            T.Text
                                            (Either (BuiltinByteString,
                                                     Integer,
                                                     Value,
                                                     TxConstraints RoomThermostatInput
                                                                   RoomThermostatState)
                                                    RoomThermostatError)
                    returnError err = pure $ Right $ Error $ T.pack err
                    returnOk :: Contract ()
                                         RoomThermostatSchema
                                         T.Text
                                         (Either (BuiltinByteString,
                                                  Integer,
                                                  Value,
                                                  TxConstraints RoomThermostatInput
                                                                RoomThermostatState)
                                                 RoomThermostatError)
                    returnOk = pure $ Left (modeValue,
                                            temperatureValue,
                                            stateVal,
                                            mempty)
                    returnOutputOk :: LogicOutput ->
                                      Contract ()
                                               RoomThermostatSchema
                                               T.Text
                                               (Either (BuiltinByteString,
                                                        Integer,
                                                        Value,
                                                        TxConstraints RoomThermostatInput
                                                                      RoomThermostatState)
                                                       RoomThermostatError)
                    returnOutputOk out = pure $ Left $ getOutput out
                    returnOkWith :: BuiltinByteString ->
                                    Integer ->
                                    Value ->
                                    TxConstraints RoomThermostatInput RoomThermostatState ->
                                    Contract ()
                                             RoomThermostatSchema
                                             T.Text
                                             (Either (BuiltinByteString,
                                                      Integer,
                                                      Value,
                                                      TxConstraints RoomThermostatInput
                                                                    RoomThermostatState)
                                                     RoomThermostatError)
                    returnOkWith modeValueRet temperatureValueRet stateValRet constraintRet = pure $ Left (modeValueRet,
                                                                                                           temperatureValueRet,
                                                                                                           stateValRet,
                                                                                                           constraintRet)

startThermostatLogic :: BuiltinByteString ->
                        Integer ->
                        [(Integer, (POSIXTime, Slot))] ->
                        Value ->
                        Contract ()
                                 RoomThermostatSchema
                                 T.Text
                                 (Either (BuiltinByteString,
                                          Integer,
                                          Value,
                                          TxConstraints RoomThermostatInput RoomThermostatState)
                                         RoomThermostatError)
startThermostatLogic modeValue temperatureValue triggerTimeStamps stateVal = do returnOk
                         where output :: LogicOutput
                               output = LogicOutput modeValue temperatureValue stateVal mempty
                               printMsg :: String -> Contract () RoomThermostatSchema T.Text ()
                               printMsg msg = logInfo @String msg
                               printError :: String -> Contract () RoomThermostatSchema T.Text ()
                               printError err = logError @String err
                               returnError :: String ->
                                              Contract ()
                                                       RoomThermostatSchema
                                                       T.Text
                                                       (Either (BuiltinByteString,
                                                                Integer,
                                                                Value,
                                                                TxConstraints RoomThermostatInput
                                                                              RoomThermostatState)
                                                               RoomThermostatError)
                               returnError err = pure $ Right $ Error $ T.pack err
                               returnOk :: Contract ()
                                                    RoomThermostatSchema
                                                    T.Text
                                                    (Either (BuiltinByteString,
                                                             Integer,
                                                             Value,
                                                             TxConstraints RoomThermostatInput
                                                                           RoomThermostatState)
                                                            RoomThermostatError)
                               returnOk = pure $ Left (modeValue,
                                                       temperatureValue,
                                                       stateVal,
                                                       mempty)
                               returnOutputOk :: LogicOutput ->
                                                 Contract ()
                                                          RoomThermostatSchema
                                                          T.Text
                                                          (Either (BuiltinByteString,
                                                                   Integer,
                                                                   Value,
                                                                   TxConstraints RoomThermostatInput
                                                                                 RoomThermostatState)
                                                                  RoomThermostatError)
                               returnOutputOk out = pure $ Left $ getOutput out
                               returnOkWith :: BuiltinByteString ->
                                               Integer ->
                                               Value ->
                                               TxConstraints RoomThermostatInput
                                                             RoomThermostatState ->
                                               Contract ()
                                                        RoomThermostatSchema
                                                        T.Text
                                                        (Either (BuiltinByteString,
                                                                 Integer,
                                                                 Value,
                                                                 TxConstraints RoomThermostatInput
                                                                               RoomThermostatState)
                                                                RoomThermostatError)
                               returnOkWith modeValueRet temperatureValueRet stateValRet constraintRet = pure $ Left (modeValueRet,
                                                                                                                      temperatureValueRet,
                                                                                                                      stateValRet,
                                                                                                                      constraintRet)

setTargetTemperatureLogic :: Integer ->
                             BuiltinByteString ->
                             Integer ->
                             [(Integer, (POSIXTime, Slot))] ->
                             Value ->
                             Contract ()
                                      RoomThermostatSchema
                                      T.Text
                                      (Either (BuiltinByteString,
                                               Integer,
                                               Value,
                                               TxConstraints RoomThermostatInput
                                                             RoomThermostatState)
                                              RoomThermostatError)
setTargetTemperatureLogic targetTemp modeValue temperatureValue triggerTimeStamps stateVal = do returnOutputOk $ setTemperatureValue targetTemp $ setStateVal stateVal output
                              where output :: LogicOutput
                                    output = LogicOutput modeValue temperatureValue stateVal mempty
                                    printMsg :: String -> Contract () RoomThermostatSchema T.Text ()
                                    printMsg msg = logInfo @String msg
                                    printError :: String ->
                                                  Contract () RoomThermostatSchema T.Text ()
                                    printError err = logError @String err
                                    returnError :: String ->
                                                   Contract ()
                                                            RoomThermostatSchema
                                                            T.Text
                                                            (Either (BuiltinByteString,
                                                                     Integer,
                                                                     Value,
                                                                     TxConstraints RoomThermostatInput
                                                                                   RoomThermostatState)
                                                                    RoomThermostatError)
                                    returnError err = pure $ Right $ Error $ T.pack err
                                    returnOk :: Contract ()
                                                         RoomThermostatSchema
                                                         T.Text
                                                         (Either (BuiltinByteString,
                                                                  Integer,
                                                                  Value,
                                                                  TxConstraints RoomThermostatInput
                                                                                RoomThermostatState)
                                                                 RoomThermostatError)
                                    returnOk = pure $ Left (modeValue,
                                                            temperatureValue,
                                                            stateVal,
                                                            mempty)
                                    returnOutputOk :: LogicOutput ->
                                                      Contract ()
                                                               RoomThermostatSchema
                                                               T.Text
                                                               (Either (BuiltinByteString,
                                                                        Integer,
                                                                        Value,
                                                                        TxConstraints RoomThermostatInput
                                                                                      RoomThermostatState)
                                                                       RoomThermostatError)
                                    returnOutputOk out = pure $ Left $ getOutput out
                                    returnOkWith :: BuiltinByteString ->
                                                    Integer ->
                                                    Value ->
                                                    TxConstraints RoomThermostatInput
                                                                  RoomThermostatState ->
                                                    Contract ()
                                                             RoomThermostatSchema
                                                             T.Text
                                                             (Either (BuiltinByteString,
                                                                      Integer,
                                                                      Value,
                                                                      TxConstraints RoomThermostatInput
                                                                                    RoomThermostatState)
                                                                     RoomThermostatError)
                                    returnOkWith modeValueRet temperatureValueRet stateValRet constraintRet = pure $ Left (modeValueRet,
                                                                                                                           temperatureValueRet,
                                                                                                                           stateValRet,
                                                                                                                           constraintRet)

setModeLogic :: String ->
                BuiltinByteString ->
                Integer ->
                [(Integer, (POSIXTime, Slot))] ->
                Value ->
                Contract ()
                         RoomThermostatSchema
                         T.Text
                         (Either (BuiltinByteString,
                                  Integer,
                                  Value,
                                  TxConstraints RoomThermostatInput RoomThermostatState)
                                 RoomThermostatError)
setModeLogic targetMode modeValue temperatureValue triggerTimeStamps stateVal = do returnOutputOk $ setModeValue (toBuiltin $ C.pack targetMode) $ setStateVal stateVal output
                 where output :: LogicOutput
                       output = LogicOutput modeValue temperatureValue stateVal mempty
                       printMsg :: String -> Contract () RoomThermostatSchema T.Text ()
                       printMsg msg = logInfo @String msg
                       printError :: String -> Contract () RoomThermostatSchema T.Text ()
                       printError err = logError @String err
                       returnError :: String ->
                                      Contract ()
                                               RoomThermostatSchema
                                               T.Text
                                               (Either (BuiltinByteString,
                                                        Integer,
                                                        Value,
                                                        TxConstraints RoomThermostatInput
                                                                      RoomThermostatState)
                                                       RoomThermostatError)
                       returnError err = pure $ Right $ Error $ T.pack err
                       returnOk :: Contract ()
                                            RoomThermostatSchema
                                            T.Text
                                            (Either (BuiltinByteString,
                                                     Integer,
                                                     Value,
                                                     TxConstraints RoomThermostatInput
                                                                   RoomThermostatState)
                                                    RoomThermostatError)
                       returnOk = pure $ Left (modeValue,
                                               temperatureValue,
                                               stateVal,
                                               mempty)
                       returnOutputOk :: LogicOutput ->
                                         Contract ()
                                                  RoomThermostatSchema
                                                  T.Text
                                                  (Either (BuiltinByteString,
                                                           Integer,
                                                           Value,
                                                           TxConstraints RoomThermostatInput
                                                                         RoomThermostatState)
                                                          RoomThermostatError)
                       returnOutputOk out = pure $ Left $ getOutput out
                       returnOkWith :: BuiltinByteString ->
                                       Integer ->
                                       Value ->
                                       TxConstraints RoomThermostatInput RoomThermostatState ->
                                       Contract ()
                                                RoomThermostatSchema
                                                T.Text
                                                (Either (BuiltinByteString,
                                                         Integer,
                                                         Value,
                                                         TxConstraints RoomThermostatInput
                                                                       RoomThermostatState)
                                                        RoomThermostatError)
                       returnOkWith modeValueRet temperatureValueRet stateValRet constraintRet = pure $ Left (modeValueRet,
                                                                                                              temperatureValueRet,
                                                                                                              stateValRet,
                                                                                                              constraintRet)





-- | Defining the contract to run on the playground
contract :: Contract () RoomThermostatSchema T.Text ()
contract = selectList[init, startThermostat, setTargetTemperature, setMode]

endpoints :: Contract () RoomThermostatSchema T.Text ()
endpoints = forever contract

mkSchemaDefinitions ''RoomThermostatSchema