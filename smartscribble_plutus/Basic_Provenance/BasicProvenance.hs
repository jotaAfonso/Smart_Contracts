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
data BasicProvenanceState =
    None [(Integer,(POSIXTime,Slot))]
    | InitialiseState BuiltinByteString [(Integer,(POSIXTime,Slot))]
    | TransferResponsibilityState BuiltinByteString [(Integer,(POSIXTime,Slot))]
    | CompleteState BuiltinByteString [(Integer,(POSIXTime,Slot))]
    deriving stock (Show, Generic)

-- | Declaration of the inputs that will be used for the transitions of the State Machine
data BasicProvenanceInput =
    InitialiseInput BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    | TransferResponsibilityInput BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    | CompleteInput BuiltinByteString [(Integer,(POSIXTime,Slot))] Value
    deriving stock (Show, Generic)

-- | Make the types possible to use in the "on chain" part of Plutus (State Machine)

PlutusTx.makeLift ''BasicProvenanceState
PlutusTx.makeLift ''BasicProvenanceInput
PlutusTx.unstableMakeIsData ''BasicProvenanceState
PlutusTx.unstableMakeIsData ''BasicProvenanceInput
-- | Declaration of the errors that will be used throughout this contract
data BasicProvenanceError = Error Text
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''BasicProvenanceError


-- | Transforming errors into Text to comply with endpoint definitions
mapSMError' :: Contract w s SM.SMContractError a -> Contract w s Text a
mapSMError' = mapError $ T.pack . show

mapContractError' :: Contract w s ContractError a -> Contract w s Text a
mapContractError' = mapError $ T.pack . show

-- | Contract schema: Endpoint and the parameters they receive
type BasicProvenanceSchema =
        Endpoint "complete" ()
        .\/ Endpoint "initialise" ()
        .\/ Endpoint "transferresponsibility" TransferResponsibilityParams


data TransferResponsibilityParams = TransferResponsibilityParams {
  newParty :: String
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
transition :: State BasicProvenanceState -> BasicProvenanceInput -> Maybe (TxConstraints Void Void, State BasicProvenanceState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (None _, InitialiseInput counterParty triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitialiseState counterParty triggerTimeStamps, stateValue = stateVal})
    (InitialiseState _ _, TransferResponsibilityInput counterParty triggerTimeStamps stateVal) -> Just(mempty, State{stateData = TransferResponsibilityState counterParty triggerTimeStamps, stateValue = stateVal})
    (TransferResponsibilityState _ _, CompleteInput counterParty triggerTimeStamps stateVal) -> Just(mempty, State{stateData = CompleteState counterParty triggerTimeStamps, stateValue = mempty})
    
    _ -> Nothing

{-# INLINABLE transitionCheck #-}
transitionCheck :: BasicProvenanceState -> BasicProvenanceInput -> ScriptContext -> Bool
transitionCheck state input context = case (state, input) of
    (None triggerTimeStamps, InitialiseInput _ _ _) -> True
    (InitialiseState counterParty triggerTimeStamps, TransferResponsibilityInput _ _ _) -> True
    (TransferResponsibilityState counterParty triggerTimeStamps, CompleteInput _ _ _) -> True
    
    _ -> False
    where
        checkKeys keys = any (txSignedBy $ (scriptContextTxInfo context)) $ map unPaymentPubKeyHash keys

{-# INLINABLE machine #-}
machine :: SM.StateMachine BasicProvenanceState BasicProvenanceInput
machine = SM.StateMachine
        { SM.smTransition = transition
        , SM.smFinal = isFinal
        , SM.smCheck = transitionCheck
        , SM.smThreadToken = Nothing
        }
    where
        isFinal (CompleteState _ _) = True
        isFinal _       = False

{-# INLINABLE mkValidator #-} 
mkValidator :: Scripts.ValidatorType (SM.StateMachine BasicProvenanceState BasicProvenanceInput)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.TypedValidator (SM.StateMachine BasicProvenanceState BasicProvenanceInput)
scriptInstance = Scripts.mkTypedValidator @(SM.StateMachine BasicProvenanceState BasicProvenanceInput)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @BasicProvenanceState @BasicProvenanceInput

machineInstance :: SM.StateMachineInstance BasicProvenanceState BasicProvenanceInput
machineInstance = SM.StateMachineInstance machine scriptInstance

client :: SM.StateMachineClient BasicProvenanceState BasicProvenanceInput
client = SM.mkStateMachineClient machineInstance

-- | Methods to interact with the state machine

runContractStep :: SM.StateMachineClient BasicProvenanceState BasicProvenanceInput -> BasicProvenanceInput -> TxConstraints BasicProvenanceInput BasicProvenanceState -> Contract () BasicProvenanceSchema SM.SMContractError ( Maybe BasicProvenanceState )
runContractStep client input constraint = 
    do 
    pkh <- ownPaymentPubKeyHash
    r <- SM.runStepWith (Constraints.ownPaymentPubKeyHash pkh) constraint client input 
    case r of 
      SM.TransitionFailure i -> pure Nothing
      SM.TransitionSuccess s -> pure (Just s)

initialiseSM :: Contract () BasicProvenanceSchema SM.SMContractError (Maybe BasicProvenanceState)
initialiseSM = do
    currentState <- getCurrentStateSM client
    case currentState of
        Nothing -> do
          let triggerTimeStamps = []
          SM.runInitialise client (None  triggerTimeStamps) mempty
          pure Nothing
        x -> pure x

getCurrentStateSM :: SM.StateMachineClient BasicProvenanceState BasicProvenanceInput -> Contract () BasicProvenanceSchema SM.SMContractError (Maybe BasicProvenanceState)
getCurrentStateSM client = do
  currentState <- SM.getOnChainState client
  case currentState of
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=state}}, _) -> pure (Just (state))
    _ -> pure Nothing

isValidCallInState :: Maybe BasicProvenanceState -> BasicProvenanceInput -> Bool
isValidCallInState Nothing input = canInitialiseSM input
isValidCallInState (Just state) input = validTransitions state input

canInitialiseSM :: BasicProvenanceInput -> Bool
canInitialiseSM (InitialiseInput _ _ _) = True
canInitialiseSM _ = False

validTransitions :: BasicProvenanceState -> BasicProvenanceInput -> Bool
validTransitions (TransferResponsibilityState _ _) (CompleteInput _ _ _) = True
validTransitions (InitialiseState _ _) (TransferResponsibilityInput _ _ _) = True
validTransitions (None _) (InitialiseInput _ _ _) = True
validTransitions _ _ = False


-- | Some utility functions
fundsAtAddressCondition :: (AsContractError e) => (Value -> Bool)
    -> Address
    -> Contract () BasicProvenanceSchema e (Map.Map TxOutRef ChainIndexTxOut)
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

fundsInContract  :: AsContractError e => Contract () BasicProvenanceSchema e (Value)
fundsInContract = do
  utxos <-  utxosAt contractAddress
  return $ foldMap (view ciTxOutValue) $ map snd $ Map.toList utxos

zeroLovelace :: Value 
zeroLovelace = Ada.lovelaceValueOf 0

noKey :: PaymentPubKeyHash
noKey = stringToKey "00000000000000000000000000000000000000000000000000000000"

getContractInfo :: Contract () BasicProvenanceSchema T.Text ([(Integer,(POSIXTime,Slot))])
getContractInfo = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (TransferResponsibilityState _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (CompleteState _ triggerTimeStamps) -> pure $ (triggerTimeStamps)
        Just (None triggerTimeStamps) -> pure $ (triggerTimeStamps)

getFields :: Contract () BasicProvenanceSchema T.Text (BuiltinByteString, [(Integer,(POSIXTime,Slot))])
getFields = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState counterParty triggerTimeStamps) -> pure $ (counterParty, triggerTimeStamps)
        Just (TransferResponsibilityState counterParty triggerTimeStamps) -> pure $ (counterParty, triggerTimeStamps)
        Just (CompleteState counterParty triggerTimeStamps) -> pure $ (counterParty, triggerTimeStamps)


-- | Beginning of Endpoint declarations
initialise :: Promise () BasicProvenanceSchema T.Text ()
initialise = endpoint @"initialise" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (InitialiseInput undefined undefined undefined) then do
    -- | Calling business logic function...
      mapSMError' $ initialiseSM
      oldVal <- fundsInContract
      (triggerTimeStamps) <- getContractInfo
      logic <- initialiseLogic mempty triggerTimeStamps oldVal
      case logic of
        Left (counterParty, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (InitialiseInput counterParty triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint initialise"

 
transferResponsibility :: Promise () BasicProvenanceSchema T.Text ()
transferResponsibility = endpoint @"transferresponsibility" @TransferResponsibilityParams $ \(TransferResponsibilityParams newParty) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (TransferResponsibilityInput undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (counterPartyOld, triggerTimeStamps) <- getFields
      logic <- transferResponsibilityLogic newParty counterPartyOld triggerTimeStamps oldVal
      case logic of
        Left (counterParty, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (TransferResponsibilityInput counterParty triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint transferResponsibility"

 
complete :: Promise () BasicProvenanceSchema T.Text ()
complete = endpoint @"complete" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (CompleteInput undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (counterPartyOld, triggerTimeStamps) <- getFields
      logic <- completeLogic counterPartyOld triggerTimeStamps oldVal
      case logic of
        Left (counterParty, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (CompleteInput counterParty triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint complete"

 

-- | Data structure used in the logic functions to track the change of the fields that may be altered from the execution of an endpoint.
data LogicOutput = LogicOutput{
    counterParty :: BuiltinByteString,
    stateVal :: Value,
    constraint :: TxConstraints BasicProvenanceInput BasicProvenanceState
}

-- | Changes the value of the counterParty field.
setCounterParty :: BuiltinByteString -> LogicOutput -> LogicOutput
setCounterParty newCounterParty output = output{ counterParty = newCounterParty }

-- | Changes the value of the stateVal field.
setStateVal :: Value -> LogicOutput -> LogicOutput
setStateVal newStateVal output = output{ stateVal = newStateVal }

-- | Changes the value of the constraint field.
setConstraint :: TxConstraints BasicProvenanceInput BasicProvenanceState -> LogicOutput -> LogicOutput
setConstraint newConstraint output = output{ constraint = newConstraint }

getOutput :: LogicOutput -> (BuiltinByteString,Value,TxConstraints BasicProvenanceInput BasicProvenanceState)
getOutput out = (counterParty out,stateVal out,constraint out)

getSoloOutput :: LogicOutput -> (BuiltinByteString,Value,TxConstraints BasicProvenanceInput BasicProvenanceState)
getSoloOutput out = (counterParty out,stateVal out,constraint out)

-- | Logic functions (for one of the endpoints and triggers declared in the protocol
-- | TODO: customize this section of the contract!

initialiseLogic :: BuiltinByteString ->
                   [(Integer, (POSIXTime, Slot))] ->
                   Value ->
                   Contract ()
                            BasicProvenanceSchema
                            T.Text
                            (Either (BuiltinByteString,
                                     Value,
                                     TxConstraints BasicProvenanceInput BasicProvenanceState)
                                    BasicProvenanceError)
initialiseLogic counterParty triggerTimeStamps stateVal = do returnOutputOk $ setStateVal stateVal output
                    where output :: LogicOutput
                          output = LogicOutput counterParty stateVal mempty
                          printMsg :: String -> Contract () BasicProvenanceSchema T.Text ()
                          printMsg msg = logInfo @String msg
                          printError :: String -> Contract () BasicProvenanceSchema T.Text ()
                          printError err = logError @String err
                          returnError :: String ->
                                         Contract ()
                                                  BasicProvenanceSchema
                                                  T.Text
                                                  (Either (BuiltinByteString,
                                                           Value,
                                                           TxConstraints BasicProvenanceInput
                                                                         BasicProvenanceState)
                                                          BasicProvenanceError)
                          returnError err = pure $ Right $ Error $ T.pack err
                          returnOk :: Contract ()
                                               BasicProvenanceSchema
                                               T.Text
                                               (Either (BuiltinByteString,
                                                        Value,
                                                        TxConstraints BasicProvenanceInput
                                                                      BasicProvenanceState)
                                                       BasicProvenanceError)
                          returnOk = pure $ Left (counterParty, stateVal, mempty)
                          returnOutputOk :: LogicOutput ->
                                            Contract ()
                                                     BasicProvenanceSchema
                                                     T.Text
                                                     (Either (BuiltinByteString,
                                                              Value,
                                                              TxConstraints BasicProvenanceInput
                                                                            BasicProvenanceState)
                                                             BasicProvenanceError)
                          returnOutputOk out = pure $ Left $ getOutput out
                          returnOkWith :: BuiltinByteString ->
                                          Value ->
                                          TxConstraints BasicProvenanceInput BasicProvenanceState ->
                                          Contract ()
                                                   BasicProvenanceSchema
                                                   T.Text
                                                   (Either (BuiltinByteString,
                                                            Value,
                                                            TxConstraints BasicProvenanceInput
                                                                          BasicProvenanceState)
                                                           BasicProvenanceError)
                          returnOkWith counterPartyRet stateValRet constraintRet = pure $ Left (counterPartyRet,
                                                                                                stateValRet,
                                                                                                constraintRet)

transferResponsibilityLogic :: String ->
                               BuiltinByteString ->
                               [(Integer, (POSIXTime, Slot))] ->
                               Value ->
                               Contract ()
                                        BasicProvenanceSchema
                                        T.Text
                                        (Either (BuiltinByteString,
                                                 Value,
                                                 TxConstraints BasicProvenanceInput
                                                               BasicProvenanceState)
                                                BasicProvenanceError)
transferResponsibilityLogic newParty counterParty triggerTimeStamps stateVal = do returnOutputOk $ setCounterParty (toBuiltin $ C.pack newParty) $ setStateVal stateVal output
                                where output :: LogicOutput
                                      output = LogicOutput counterParty stateVal mempty
                                      printMsg :: String ->
                                                  Contract () BasicProvenanceSchema T.Text ()
                                      printMsg msg = logInfo @String msg
                                      printError :: String ->
                                                    Contract () BasicProvenanceSchema T.Text ()
                                      printError err = logError @String err
                                      returnError :: String ->
                                                     Contract ()
                                                              BasicProvenanceSchema
                                                              T.Text
                                                              (Either (BuiltinByteString,
                                                                       Value,
                                                                       TxConstraints BasicProvenanceInput
                                                                                     BasicProvenanceState)
                                                                      BasicProvenanceError)
                                      returnError err = pure $ Right $ Error $ T.pack err
                                      returnOk :: Contract ()
                                                           BasicProvenanceSchema
                                                           T.Text
                                                           (Either (BuiltinByteString,
                                                                    Value,
                                                                    TxConstraints BasicProvenanceInput
                                                                                  BasicProvenanceState)
                                                                   BasicProvenanceError)
                                      returnOk = pure $ Left (counterParty, stateVal, mempty)
                                      returnOutputOk :: LogicOutput ->
                                                        Contract ()
                                                                 BasicProvenanceSchema
                                                                 T.Text
                                                                 (Either (BuiltinByteString,
                                                                          Value,
                                                                          TxConstraints BasicProvenanceInput
                                                                                        BasicProvenanceState)
                                                                         BasicProvenanceError)
                                      returnOutputOk out = pure $ Left $ getOutput out
                                      returnOkWith :: BuiltinByteString ->
                                                      Value ->
                                                      TxConstraints BasicProvenanceInput
                                                                    BasicProvenanceState ->
                                                      Contract ()
                                                               BasicProvenanceSchema
                                                               T.Text
                                                               (Either (BuiltinByteString,
                                                                        Value,
                                                                        TxConstraints BasicProvenanceInput
                                                                                      BasicProvenanceState)
                                                                       BasicProvenanceError)
                                      returnOkWith counterPartyRet stateValRet constraintRet = pure $ Left (counterPartyRet,
                                                                                                            stateValRet,
                                                                                                            constraintRet)

completeLogic :: BuiltinByteString ->
                 [(Integer, (POSIXTime, Slot))] ->
                 Value ->
                 Contract ()
                          BasicProvenanceSchema
                          T.Text
                          (Either (BuiltinByteString,
                                   Value,
                                   TxConstraints BasicProvenanceInput BasicProvenanceState)
                                  BasicProvenanceError)
completeLogic counterParty triggerTimeStamps stateVal = do returnOutputOk $ setStateVal stateVal output
                  where output :: LogicOutput
                        output = LogicOutput counterParty stateVal mempty
                        printMsg :: String -> Contract () BasicProvenanceSchema T.Text ()
                        printMsg msg = logInfo @String msg
                        printError :: String -> Contract () BasicProvenanceSchema T.Text ()
                        printError err = logError @String err
                        returnError :: String ->
                                       Contract ()
                                                BasicProvenanceSchema
                                                T.Text
                                                (Either (BuiltinByteString,
                                                         Value,
                                                         TxConstraints BasicProvenanceInput
                                                                       BasicProvenanceState)
                                                        BasicProvenanceError)
                        returnError err = pure $ Right $ Error $ T.pack err
                        returnOk :: Contract ()
                                             BasicProvenanceSchema
                                             T.Text
                                             (Either (BuiltinByteString,
                                                      Value,
                                                      TxConstraints BasicProvenanceInput
                                                                    BasicProvenanceState)
                                                     BasicProvenanceError)
                        returnOk = pure $ Left (counterParty, stateVal, mempty)
                        returnOutputOk :: LogicOutput ->
                                          Contract ()
                                                   BasicProvenanceSchema
                                                   T.Text
                                                   (Either (BuiltinByteString,
                                                            Value,
                                                            TxConstraints BasicProvenanceInput
                                                                          BasicProvenanceState)
                                                           BasicProvenanceError)
                        returnOutputOk out = pure $ Left $ getOutput out
                        returnOkWith :: BuiltinByteString ->
                                        Value ->
                                        TxConstraints BasicProvenanceInput BasicProvenanceState ->
                                        Contract ()
                                                 BasicProvenanceSchema
                                                 T.Text
                                                 (Either (BuiltinByteString,
                                                          Value,
                                                          TxConstraints BasicProvenanceInput
                                                                        BasicProvenanceState)
                                                         BasicProvenanceError)
                        returnOkWith counterPartyRet stateValRet constraintRet = pure $ Left (counterPartyRet,
                                                                                              stateValRet,
                                                                                              constraintRet)





-- | Defining the contract to run on the playground
contract :: Contract () BasicProvenanceSchema T.Text ()
contract = selectList[initialise, transferResponsibility, complete]

endpoints :: Contract () BasicProvenanceSchema T.Text ()
endpoints = forever contract

mkSchemaDefinitions ''BasicProvenanceSchema