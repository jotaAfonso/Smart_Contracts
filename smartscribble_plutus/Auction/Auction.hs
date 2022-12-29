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
data AuctionState =
    None [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))]
    | InitialiseState Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))]
    | CloseAuctionState Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))]
    | CollectFundsAndGiveRewardsState Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))]
    | ContinueAuctionState Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))]
    | CheckWinnerState Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))]
    deriving stock (Show, Generic)

-- | Declaration of the inputs that will be used for the transitions of the State Machine
data AuctionInput =
    InitialiseInput Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))] Value
    | CloseAuctionInput Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))] Value
    | CollectFundsAndGiveRewardsInput Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))] Value
    | ContinueAuctionInput Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))] Value
    | BidInput Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))] Value
    | CheckWinnerInput Value PaymentPubKeyHash Integer [PaymentPubKeyHash] [(Integer,(POSIXTime,Slot))] Value
    deriving stock (Show, Generic)

-- | Make the types possible to use in the "on chain" part of Plutus (State Machine)

PlutusTx.makeLift ''AuctionState
PlutusTx.makeLift ''AuctionInput
PlutusTx.unstableMakeIsData ''AuctionState
PlutusTx.unstableMakeIsData ''AuctionInput
-- | Declaration of the errors that will be used throughout this contract
data AuctionError = Error Text
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''AuctionError


-- | Transforming errors into Text to comply with endpoint definitions
mapSMError' :: Contract w s SM.SMContractError a -> Contract w s Text a
mapSMError' = mapError $ T.pack . show

mapContractError' :: Contract w s ContractError a -> Contract w s Text a
mapContractError' = mapError $ T.pack . show

-- | Contract schema: Endpoint and the parameters they receive
type AuctionSchema =
        Endpoint "bid" BidParams
        .\/ Endpoint "checkwinner" ()
        .\/ Endpoint "closeauction" ()
        .\/ Endpoint "collectfundsandgiverewards" ()
        .\/ Endpoint "continueauction" ()
        .\/ Endpoint "initialise" InitialiseParams


data BidParams = BidParams {
  newBid :: Value
  } 
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

data InitialiseParams = InitialiseParams {
  initBid :: Value,
  assetToSell :: Integer
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
transition :: State AuctionState -> AuctionInput -> Maybe (TxConstraints Void Void, State AuctionState)
transition State{stateData=oldData,stateValue} input = case (oldData, input) of
    (None _ _, InitialiseInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitialiseState currBid lastBidder asset sellerId triggerTimeStamps, stateValue = stateVal})
    (CloseAuctionState _ _ _ _ _, CollectFundsAndGiveRewardsInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) -> Just(mempty, State{stateData = CollectFundsAndGiveRewardsState currBid lastBidder asset sellerId triggerTimeStamps, stateValue = stateVal})
    (ContinueAuctionState _ _ _ _ _, BidInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) -> Just(mempty, State{stateData = InitialiseState currBid lastBidder asset sellerId triggerTimeStamps, stateValue = stateVal})
    (InitialiseState _ _ _ _ _, ContinueAuctionInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) -> Just(mempty, State{stateData = ContinueAuctionState currBid lastBidder asset sellerId triggerTimeStamps, stateValue = stateVal})
    (InitialiseState _ _ _ _ _, CloseAuctionInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) -> Just(mempty, State{stateData = CloseAuctionState currBid lastBidder asset sellerId triggerTimeStamps, stateValue = stateVal})
    (CollectFundsAndGiveRewardsState _ _ _ _ _, CheckWinnerInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) -> Just(mempty, State{stateData = CheckWinnerState currBid lastBidder asset sellerId triggerTimeStamps, stateValue = mempty})
    
    _ -> Nothing

{-# INLINABLE transitionCheck #-}
transitionCheck :: AuctionState -> AuctionInput -> ScriptContext -> Bool
transitionCheck state input context = case (state, input) of
    (None sellerId triggerTimeStamps, InitialiseInput _ _ _ _ _ _) -> checkKeys sellerId
    (CloseAuctionState currBid lastBidder asset sellerId triggerTimeStamps, CollectFundsAndGiveRewardsInput _ _ _ _ _ _) -> checkKeys sellerId
    (ContinueAuctionState currBid lastBidder asset sellerId triggerTimeStamps, BidInput _ _ _ _ _ _) -> True
    (InitialiseState currBid lastBidder asset sellerId triggerTimeStamps, ContinueAuctionInput _ _ _ _ _ _) -> checkKeys sellerId
    (InitialiseState currBid lastBidder asset sellerId triggerTimeStamps, CloseAuctionInput _ _ _ _ _ _) -> checkKeys sellerId
    (CollectFundsAndGiveRewardsState currBid lastBidder asset sellerId triggerTimeStamps, CheckWinnerInput _ _ _ _ _ _) -> checkKeys sellerId
    
    _ -> False
    where
        checkKeys keys = any (txSignedBy $ (scriptContextTxInfo context)) $ map unPaymentPubKeyHash keys

{-# INLINABLE machine #-}
machine :: SM.StateMachine AuctionState AuctionInput
machine = SM.StateMachine
        { SM.smTransition = transition
        , SM.smFinal = isFinal
        , SM.smCheck = transitionCheck
        , SM.smThreadToken = Nothing
        }
    where
        isFinal (CheckWinnerState _ _ _ _ _) = True
        isFinal _       = False

{-# INLINABLE mkValidator #-} 
mkValidator :: Scripts.ValidatorType (SM.StateMachine AuctionState AuctionInput)
mkValidator = SM.mkValidator machine

scriptInstance :: Scripts.TypedValidator (SM.StateMachine AuctionState AuctionInput)
scriptInstance = Scripts.mkTypedValidator @(SM.StateMachine AuctionState AuctionInput)
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @AuctionState @AuctionInput

machineInstance :: SM.StateMachineInstance AuctionState AuctionInput
machineInstance = SM.StateMachineInstance machine scriptInstance

client :: SM.StateMachineClient AuctionState AuctionInput
client = SM.mkStateMachineClient machineInstance

-- | Methods to interact with the state machine

runContractStep :: SM.StateMachineClient AuctionState AuctionInput -> AuctionInput -> TxConstraints AuctionInput AuctionState -> Contract () AuctionSchema SM.SMContractError ( Maybe AuctionState )
runContractStep client input constraint = 
    do 
    pkh <- ownPaymentPubKeyHash
    r <- SM.runStepWith (Constraints.ownPaymentPubKeyHash pkh) constraint client input 
    case r of 
      SM.TransitionFailure i -> pure Nothing
      SM.TransitionSuccess s -> pure (Just s)

initialiseSM :: Contract () AuctionSchema SM.SMContractError (Maybe AuctionState)
initialiseSM = do
    currentState <- getCurrentStateSM client
    case currentState of
        Nothing -> do
          let triggerTimeStamps = []
              sellerId = [stringToKey "abcdef", stringToKey "012345", stringToKey "6789ab"]
          SM.runInitialise client (None sellerId triggerTimeStamps) mempty
          pure Nothing
        x -> pure x

getCurrentStateSM :: SM.StateMachineClient AuctionState AuctionInput -> Contract () AuctionSchema SM.SMContractError (Maybe AuctionState)
getCurrentStateSM client = do
  currentState <- SM.getOnChainState client
  case currentState of
    Just (SM.OnChainState{SM.ocsTxOut=TypedScriptTxOut{tyTxOutData=state}}, _) -> pure (Just (state))
    _ -> pure Nothing

isValidCallInState :: Maybe AuctionState -> AuctionInput -> Bool
isValidCallInState Nothing input = canInitialiseSM input
isValidCallInState (Just state) input = validTransitions state input

canInitialiseSM :: AuctionInput -> Bool
canInitialiseSM (InitialiseInput _ _ _ _ _ _) = True
canInitialiseSM _ = False

validTransitions :: AuctionState -> AuctionInput -> Bool
validTransitions (CollectFundsAndGiveRewardsState _ _ _ _ _) (CheckWinnerInput _ _ _ _ _ _) = True
validTransitions (ContinueAuctionState _ _ _ _ _) (BidInput _ _ _ _ _ _) = True
validTransitions (InitialiseState _ _ _ _ _) (ContinueAuctionInput _ _ _ _ _ _) = True
validTransitions (CloseAuctionState _ _ _ _ _) (CollectFundsAndGiveRewardsInput _ _ _ _ _ _) = True
validTransitions (InitialiseState _ _ _ _ _) (CloseAuctionInput _ _ _ _ _ _) = True
validTransitions (None _ _) (InitialiseInput _ _ _ _ _ _) = True
validTransitions _ _ = False


-- | Some utility functions
fundsAtAddressCondition :: (AsContractError e) => (Value -> Bool)
    -> Address
    -> Contract () AuctionSchema e (Map.Map TxOutRef ChainIndexTxOut)
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

fundsInContract  :: AsContractError e => Contract () AuctionSchema e (Value)
fundsInContract = do
  utxos <-  utxosAt contractAddress
  return $ foldMap (view ciTxOutValue) $ map snd $ Map.toList utxos

zeroLovelace :: Value 
zeroLovelace = Ada.lovelaceValueOf 0

noKey :: PaymentPubKeyHash
noKey = stringToKey "00000000000000000000000000000000000000000000000000000000"

getContractInfo :: Contract () AuctionSchema T.Text ([PaymentPubKeyHash], [(Integer,(POSIXTime,Slot))])
getContractInfo = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState _ _ _ sellerId triggerTimeStamps) -> pure $ (sellerId, triggerTimeStamps)
        Just (CloseAuctionState _ _ _ sellerId triggerTimeStamps) -> pure $ (sellerId, triggerTimeStamps)
        Just (CollectFundsAndGiveRewardsState _ _ _ sellerId triggerTimeStamps) -> pure $ (sellerId, triggerTimeStamps)
        Just (ContinueAuctionState _ _ _ sellerId triggerTimeStamps) -> pure $ (sellerId, triggerTimeStamps)
        Just (CheckWinnerState _ _ _ sellerId triggerTimeStamps) -> pure $ (sellerId, triggerTimeStamps)
        Just (None sellerId triggerTimeStamps) -> pure $ (sellerId, triggerTimeStamps)

getFields :: Contract () AuctionSchema T.Text (Value, PaymentPubKeyHash, Integer, [PaymentPubKeyHash], [(Integer,(POSIXTime,Slot))])
getFields = do
    currState <- mapSMError' $ getCurrentStateSM client
    case currState of
        Just (InitialiseState currBid lastBidder asset sellerId triggerTimeStamps) -> pure $ (currBid, lastBidder, asset, sellerId, triggerTimeStamps)
        Just (CloseAuctionState currBid lastBidder asset sellerId triggerTimeStamps) -> pure $ (currBid, lastBidder, asset, sellerId, triggerTimeStamps)
        Just (CollectFundsAndGiveRewardsState currBid lastBidder asset sellerId triggerTimeStamps) -> pure $ (currBid, lastBidder, asset, sellerId, triggerTimeStamps)
        Just (ContinueAuctionState currBid lastBidder asset sellerId triggerTimeStamps) -> pure $ (currBid, lastBidder, asset, sellerId, triggerTimeStamps)
        Just (CheckWinnerState currBid lastBidder asset sellerId triggerTimeStamps) -> pure $ (currBid, lastBidder, asset, sellerId, triggerTimeStamps)


-- | Beginning of Endpoint declarations
initialise :: Promise () AuctionSchema T.Text ()
initialise = endpoint @"initialise" @InitialiseParams $ \(InitialiseParams initBid assetToSell) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (InitialiseInput undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      mapSMError' $ initialiseSM
      oldVal <- fundsInContract
      (sellerId, triggerTimeStamps) <- getContractInfo
      logic <- initialiseLogic initBid assetToSell mempty (stringToKey "00000000000000000000000000000000000000000000000000000000") 0 sellerId triggerTimeStamps oldVal
      case logic of
        Left (currBid, lastBidder, asset, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (InitialiseInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint initialise"

 
closeAuction :: Promise () AuctionSchema T.Text ()
closeAuction = endpoint @"closeauction" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (CloseAuctionInput undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (currBidOld, lastBidderOld, assetOld, sellerId, triggerTimeStamps) <- getFields
      logic <- closeAuctionLogic currBidOld lastBidderOld assetOld sellerId triggerTimeStamps oldVal
      case logic of
        Left (currBid, lastBidder, asset, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (CloseAuctionInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint closeAuction"

 
collectFundsAndGiveRewards :: Promise () AuctionSchema T.Text ()
collectFundsAndGiveRewards = endpoint @"collectfundsandgiverewards" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (CollectFundsAndGiveRewardsInput undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (currBidOld, lastBidderOld, assetOld, sellerId, triggerTimeStamps) <- getFields
      logic <- collectFundsAndGiveRewardsLogic currBidOld lastBidderOld assetOld sellerId triggerTimeStamps oldVal
      case logic of
        Left (currBid, lastBidder, asset, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (CollectFundsAndGiveRewardsInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint collectFundsAndGiveRewards"

 
continueAuction :: Promise () AuctionSchema T.Text ()
continueAuction = endpoint @"continueauction" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (ContinueAuctionInput undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (currBidOld, lastBidderOld, assetOld, sellerId, triggerTimeStamps) <- getFields
      logic <- continueAuctionLogic currBidOld lastBidderOld assetOld sellerId triggerTimeStamps oldVal
      case logic of
        Left (currBid, lastBidder, asset, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (ContinueAuctionInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint continueAuction"

 
bid :: Promise () AuctionSchema T.Text ()
bid = endpoint @"bid" @BidParams $ \(BidParams newBid) -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (BidInput undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (currBidOld, lastBidderOld, assetOld, sellerId, triggerTimeStamps) <- getFields
      logic <- bidLogic newBid currBidOld lastBidderOld assetOld sellerId triggerTimeStamps oldVal
      case logic of
        Left (currBid, lastBidder, asset, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (BidInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint bid"

 
checkWinner :: Promise () AuctionSchema T.Text ()
checkWinner = endpoint @"checkwinner" $ \() -> do
    -- | Received the parameters
    currentState <- mapSMError' $ getCurrentStateSM client
    if isValidCallInState currentState (CheckWinnerInput undefined undefined undefined undefined undefined undefined) then do
    -- | Calling business logic function...
      oldVal <- fundsInContract
      (currBidOld, lastBidderOld, assetOld, sellerId, triggerTimeStamps) <- getFields
      logic <- checkWinnerLogic currBidOld lastBidderOld assetOld sellerId triggerTimeStamps oldVal
      case logic of
        Left (currBid, lastBidder, asset, stateVal, constraint) -> do
          res <- mapSMError' $ runContractStep client (CheckWinnerInput currBid lastBidder asset sellerId triggerTimeStamps stateVal) constraint
          case res of
                Just s -> do
                    logInfo ("Successful transaction to state: " <> show s)
                _ -> logError @String "Invalid operation in endpoint."
        Right (Error x) -> logWarn @Text x
    else
      logError @String "Invalid invocation of endpoint checkWinner"

 

-- | Data structure used in the logic functions to track the change of the fields that may be altered from the execution of an endpoint.
data LogicOutput = LogicOutput{
    currBid :: Value,
    lastBidder :: PaymentPubKeyHash,
    asset :: Integer,
    stateVal :: Value,
    constraint :: TxConstraints AuctionInput AuctionState
}

-- | Changes the value of the currBid field.
setCurrBid :: Value -> LogicOutput -> LogicOutput
setCurrBid newCurrBid output = output{ currBid = newCurrBid }

-- | Changes the value of the lastBidder field.
setLastBidder :: PaymentPubKeyHash -> LogicOutput -> LogicOutput
setLastBidder newLastBidder output = output{ lastBidder = newLastBidder }

-- | Changes the value of the asset field.
setAsset :: Integer -> LogicOutput -> LogicOutput
setAsset newAsset output = output{ asset = newAsset }

-- | Changes the value of the stateVal field.
setStateVal :: Value -> LogicOutput -> LogicOutput
setStateVal newStateVal output = output{ stateVal = newStateVal }

-- | Changes the value of the constraint field.
setConstraint :: TxConstraints AuctionInput AuctionState -> LogicOutput -> LogicOutput
setConstraint newConstraint output = output{ constraint = newConstraint }

getOutput :: LogicOutput -> (Value,PaymentPubKeyHash,Integer,Value,TxConstraints AuctionInput AuctionState)
getOutput out = (currBid out,lastBidder out,asset out,stateVal out,constraint out)

getSoloOutput :: LogicOutput -> (Value,PaymentPubKeyHash,Integer,Value,TxConstraints AuctionInput AuctionState)
getSoloOutput out = (currBid out,lastBidder out,asset out,stateVal out,constraint out)

-- | Logic functions (for one of the endpoints and triggers declared in the protocol
-- | TODO: customize this section of the contract!

initialiseLogic :: Value ->
                   Integer ->
                   Value ->
                   PaymentPubKeyHash ->
                   Integer ->
                   [PaymentPubKeyHash] ->
                   [(Integer, (POSIXTime, Slot))] ->
                   Value ->
                   Contract ()
                            AuctionSchema
                            T.Text
                            (Either (Value,
                                     PaymentPubKeyHash,
                                     Integer,
                                     Value,
                                     TxConstraints AuctionInput AuctionState)
                                    AuctionError)
initialiseLogic initBid assetToSell currBid lastBidder asset sellerId triggerTimeStamps stateVal = do returnOutputOk $ setCurrBid initBid $ setLastBidder noKey $ setAsset assetToSell output
                    where output :: LogicOutput
                          output = LogicOutput currBid lastBidder asset stateVal mempty
                          printMsg :: String -> Contract () AuctionSchema T.Text ()
                          printMsg msg = logInfo @String msg
                          printError :: String -> Contract () AuctionSchema T.Text ()
                          printError err = logError @String err
                          returnError :: String ->
                                         Contract ()
                                                  AuctionSchema
                                                  T.Text
                                                  (Either (Value,
                                                           PaymentPubKeyHash,
                                                           Integer,
                                                           Value,
                                                           TxConstraints AuctionInput AuctionState)
                                                          AuctionError)
                          returnError err = pure $ Right $ Error $ T.pack err
                          returnOk :: Contract ()
                                               AuctionSchema
                                               T.Text
                                               (Either (Value,
                                                        PaymentPubKeyHash,
                                                        Integer,
                                                        Value,
                                                        TxConstraints AuctionInput AuctionState)
                                                       AuctionError)
                          returnOk = pure $ Left (currBid,
                                                  lastBidder,
                                                  asset,
                                                  stateVal,
                                                  mempty)
                          returnOutputOk :: LogicOutput ->
                                            Contract ()
                                                     AuctionSchema
                                                     T.Text
                                                     (Either (Value,
                                                              PaymentPubKeyHash,
                                                              Integer,
                                                              Value,
                                                              TxConstraints AuctionInput
                                                                            AuctionState)
                                                             AuctionError)
                          returnOutputOk out = pure $ Left $ getOutput out
                          returnOkWith :: Value ->
                                          PaymentPubKeyHash ->
                                          Integer ->
                                          Value ->
                                          TxConstraints AuctionInput AuctionState ->
                                          Contract ()
                                                   AuctionSchema
                                                   T.Text
                                                   (Either (Value,
                                                            PaymentPubKeyHash,
                                                            Integer,
                                                            Value,
                                                            TxConstraints AuctionInput AuctionState)
                                                           AuctionError)
                          returnOkWith currBidRet lastBidderRet assetRet stateValRet constraintRet = pure $ Left (currBidRet,
                                                                                                                  lastBidderRet,
                                                                                                                  assetRet,
                                                                                                                  stateValRet,
                                                                                                                  constraintRet)

closeAuctionLogic :: Value ->
                     PaymentPubKeyHash ->
                     Integer ->
                     [PaymentPubKeyHash] ->
                     [(Integer, (POSIXTime, Slot))] ->
                     Value ->
                     Contract ()
                              AuctionSchema
                              T.Text
                              (Either (Value,
                                       PaymentPubKeyHash,
                                       Integer,
                                       Value,
                                       TxConstraints AuctionInput AuctionState)
                                      AuctionError)
closeAuctionLogic currBid lastBidder asset sellerId triggerTimeStamps stateVal = do returnOk
                      where output :: LogicOutput
                            output = LogicOutput currBid lastBidder asset stateVal mempty
                            printMsg :: String -> Contract () AuctionSchema T.Text ()
                            printMsg msg = logInfo @String msg
                            printError :: String -> Contract () AuctionSchema T.Text ()
                            printError err = logError @String err
                            returnError :: String ->
                                           Contract ()
                                                    AuctionSchema
                                                    T.Text
                                                    (Either (Value,
                                                             PaymentPubKeyHash,
                                                             Integer,
                                                             Value,
                                                             TxConstraints AuctionInput
                                                                           AuctionState)
                                                            AuctionError)
                            returnError err = pure $ Right $ Error $ T.pack err
                            returnOk :: Contract ()
                                                 AuctionSchema
                                                 T.Text
                                                 (Either (Value,
                                                          PaymentPubKeyHash,
                                                          Integer,
                                                          Value,
                                                          TxConstraints AuctionInput AuctionState)
                                                         AuctionError)
                            returnOk = pure $ Left (currBid,
                                                    lastBidder,
                                                    asset,
                                                    stateVal,
                                                    mempty)
                            returnOutputOk :: LogicOutput ->
                                              Contract ()
                                                       AuctionSchema
                                                       T.Text
                                                       (Either (Value,
                                                                PaymentPubKeyHash,
                                                                Integer,
                                                                Value,
                                                                TxConstraints AuctionInput
                                                                              AuctionState)
                                                               AuctionError)
                            returnOutputOk out = pure $ Left $ getOutput out
                            returnOkWith :: Value ->
                                            PaymentPubKeyHash ->
                                            Integer ->
                                            Value ->
                                            TxConstraints AuctionInput AuctionState ->
                                            Contract ()
                                                     AuctionSchema
                                                     T.Text
                                                     (Either (Value,
                                                              PaymentPubKeyHash,
                                                              Integer,
                                                              Value,
                                                              TxConstraints AuctionInput
                                                                            AuctionState)
                                                             AuctionError)
                            returnOkWith currBidRet lastBidderRet assetRet stateValRet constraintRet = pure $ Left (currBidRet,
                                                                                                                    lastBidderRet,
                                                                                                                    assetRet,
                                                                                                                    stateValRet,
                                                                                                                    constraintRet)

collectFundsAndGiveRewardsLogic :: Value ->
                                   PaymentPubKeyHash ->
                                   Integer ->
                                   [PaymentPubKeyHash] ->
                                   [(Integer, (POSIXTime, Slot))] ->
                                   Value ->
                                   Contract ()
                                            AuctionSchema
                                            T.Text
                                            (Either (Value,
                                                     PaymentPubKeyHash,
                                                     Integer,
                                                     Value,
                                                     TxConstraints AuctionInput AuctionState)
                                                    AuctionError)
collectFundsAndGiveRewardsLogic currBid lastBidder asset sellerId triggerTimeStamps stateVal = do returnOutputOk $ setConstraint (Constraints.mustPayToPubKey (head sellerId) currBid) $ setStateVal mempty output
                                    where output :: LogicOutput
                                          output = LogicOutput currBid lastBidder asset stateVal mempty
                                          printMsg :: String -> Contract () AuctionSchema T.Text ()
                                          printMsg msg = logInfo @String msg
                                          printError :: String ->
                                                        Contract () AuctionSchema T.Text ()
                                          printError err = logError @String err
                                          returnError :: String ->
                                                         Contract ()
                                                                  AuctionSchema
                                                                  T.Text
                                                                  (Either (Value,
                                                                           PaymentPubKeyHash,
                                                                           Integer,
                                                                           Value,
                                                                           TxConstraints AuctionInput
                                                                                         AuctionState)
                                                                          AuctionError)
                                          returnError err = pure $ Right $ Error $ T.pack err
                                          returnOk :: Contract ()
                                                               AuctionSchema
                                                               T.Text
                                                               (Either (Value,
                                                                        PaymentPubKeyHash,
                                                                        Integer,
                                                                        Value,
                                                                        TxConstraints AuctionInput
                                                                                      AuctionState)
                                                                       AuctionError)
                                          returnOk = pure $ Left (currBid,
                                                                  lastBidder,
                                                                  asset,
                                                                  stateVal,
                                                                  mempty)
                                          returnOutputOk :: LogicOutput ->
                                                            Contract ()
                                                                     AuctionSchema
                                                                     T.Text
                                                                     (Either (Value,
                                                                              PaymentPubKeyHash,
                                                                              Integer,
                                                                              Value,
                                                                              TxConstraints AuctionInput
                                                                                            AuctionState)
                                                                             AuctionError)
                                          returnOutputOk out = pure $ Left $ getOutput out
                                          returnOkWith :: Value ->
                                                          PaymentPubKeyHash ->
                                                          Integer ->
                                                          Value ->
                                                          TxConstraints AuctionInput AuctionState ->
                                                          Contract ()
                                                                   AuctionSchema
                                                                   T.Text
                                                                   (Either (Value,
                                                                            PaymentPubKeyHash,
                                                                            Integer,
                                                                            Value,
                                                                            TxConstraints AuctionInput
                                                                                          AuctionState)
                                                                           AuctionError)
                                          returnOkWith currBidRet lastBidderRet assetRet stateValRet constraintRet = pure $ Left (currBidRet,
                                                                                                                                  lastBidderRet,
                                                                                                                                  assetRet,
                                                                                                                                  stateValRet,
                                                                                                                                  constraintRet)

continueAuctionLogic :: Value ->
                        PaymentPubKeyHash ->
                        Integer ->
                        [PaymentPubKeyHash] ->
                        [(Integer, (POSIXTime, Slot))] ->
                        Value ->
                        Contract ()
                                 AuctionSchema
                                 T.Text
                                 (Either (Value,
                                          PaymentPubKeyHash,
                                          Integer,
                                          Value,
                                          TxConstraints AuctionInput AuctionState)
                                         AuctionError)
continueAuctionLogic currBid lastBidder asset sellerId triggerTimeStamps stateVal = do returnOk
                         where output :: LogicOutput
                               output = LogicOutput currBid lastBidder asset stateVal mempty
                               printMsg :: String -> Contract () AuctionSchema T.Text ()
                               printMsg msg = logInfo @String msg
                               printError :: String -> Contract () AuctionSchema T.Text ()
                               printError err = logError @String err
                               returnError :: String ->
                                              Contract ()
                                                       AuctionSchema
                                                       T.Text
                                                       (Either (Value,
                                                                PaymentPubKeyHash,
                                                                Integer,
                                                                Value,
                                                                TxConstraints AuctionInput
                                                                              AuctionState)
                                                               AuctionError)
                               returnError err = pure $ Right $ Error $ T.pack err
                               returnOk :: Contract ()
                                                    AuctionSchema
                                                    T.Text
                                                    (Either (Value,
                                                             PaymentPubKeyHash,
                                                             Integer,
                                                             Value,
                                                             TxConstraints AuctionInput
                                                                           AuctionState)
                                                            AuctionError)
                               returnOk = pure $ Left (currBid,
                                                       lastBidder,
                                                       asset,
                                                       stateVal,
                                                       mempty)
                               returnOutputOk :: LogicOutput ->
                                                 Contract ()
                                                          AuctionSchema
                                                          T.Text
                                                          (Either (Value,
                                                                   PaymentPubKeyHash,
                                                                   Integer,
                                                                   Value,
                                                                   TxConstraints AuctionInput
                                                                                 AuctionState)
                                                                  AuctionError)
                               returnOutputOk out = pure $ Left $ getOutput out
                               returnOkWith :: Value ->
                                               PaymentPubKeyHash ->
                                               Integer ->
                                               Value ->
                                               TxConstraints AuctionInput AuctionState ->
                                               Contract ()
                                                        AuctionSchema
                                                        T.Text
                                                        (Either (Value,
                                                                 PaymentPubKeyHash,
                                                                 Integer,
                                                                 Value,
                                                                 TxConstraints AuctionInput
                                                                               AuctionState)
                                                                AuctionError)
                               returnOkWith currBidRet lastBidderRet assetRet stateValRet constraintRet = pure $ Left (currBidRet,
                                                                                                                       lastBidderRet,
                                                                                                                       assetRet,
                                                                                                                       stateValRet,
                                                                                                                       constraintRet)

bidLogic :: Value ->
            Value ->
            PaymentPubKeyHash ->
            Integer ->
            [PaymentPubKeyHash] ->
            [(Integer, (POSIXTime, Slot))] ->
            Value ->
            Contract ()
                     AuctionSchema
                     T.Text
                     (Either (Value,
                              PaymentPubKeyHash,
                              Integer,
                              Value,
                              TxConstraints AuctionInput AuctionState)
                             AuctionError)
bidLogic newBid currBid lastBidder asset sellerId triggerTimeStamps stateVal = do {pkh <- ownPaymentPubKeyHash;
                                                                                   if (newBid `V.gt` mempty) && (newBid `V.gt` currBid)
                                                                                    then returnOutputOk $ (if (lastBidder == noKey)
                                                                                                            then setCurrBid newBid $ setLastBidder pkh $ setStateVal newBid output
                                                                                                            else setCurrBid newBid $ setLastBidder pkh $ setStateVal newBid $ setConstraint (Constraints.mustPayToPubKey lastBidder currBid) output)
                                                                                    else returnError "Invalid Bid!"}
             where output :: LogicOutput
                   output = LogicOutput currBid lastBidder asset stateVal mempty
                   printMsg :: String -> Contract () AuctionSchema T.Text ()
                   printMsg msg = logInfo @String msg
                   printError :: String -> Contract () AuctionSchema T.Text ()
                   printError err = logError @String err
                   returnError :: String ->
                                  Contract ()
                                           AuctionSchema
                                           T.Text
                                           (Either (Value,
                                                    PaymentPubKeyHash,
                                                    Integer,
                                                    Value,
                                                    TxConstraints AuctionInput AuctionState)
                                                   AuctionError)
                   returnError err = pure $ Right $ Error $ T.pack err
                   returnOk :: Contract ()
                                        AuctionSchema
                                        T.Text
                                        (Either (Value,
                                                 PaymentPubKeyHash,
                                                 Integer,
                                                 Value,
                                                 TxConstraints AuctionInput AuctionState)
                                                AuctionError)
                   returnOk = pure $ Left (currBid,
                                           lastBidder,
                                           asset,
                                           stateVal,
                                           mempty)
                   returnOutputOk :: LogicOutput ->
                                     Contract ()
                                              AuctionSchema
                                              T.Text
                                              (Either (Value,
                                                       PaymentPubKeyHash,
                                                       Integer,
                                                       Value,
                                                       TxConstraints AuctionInput AuctionState)
                                                      AuctionError)
                   returnOutputOk out = pure $ Left $ getOutput out
                   returnOkWith :: Value ->
                                   PaymentPubKeyHash ->
                                   Integer ->
                                   Value ->
                                   TxConstraints AuctionInput AuctionState ->
                                   Contract ()
                                            AuctionSchema
                                            T.Text
                                            (Either (Value,
                                                     PaymentPubKeyHash,
                                                     Integer,
                                                     Value,
                                                     TxConstraints AuctionInput AuctionState)
                                                    AuctionError)
                   returnOkWith currBidRet lastBidderRet assetRet stateValRet constraintRet = pure $ Left (currBidRet,
                                                                                                           lastBidderRet,
                                                                                                           assetRet,
                                                                                                           stateValRet,
                                                                                                           constraintRet)

checkWinnerLogic :: Value -> PaymentPubKeyHash -> Integer -> [PaymentPubKeyHash] -> [(Integer,(POSIXTime,Slot))] -> Value -> Contract () AuctionSchema T.Text (Either (Value, PaymentPubKeyHash, Integer, Value, TxConstraints AuctionInput AuctionState) AuctionError)
checkWinnerLogic currBid lastBidder asset sellerId triggerTimeStamps stateVal =  do
  -- TODO: Implement the logic here
  returnError "undefined function"
    where
        -- | Data structure that stores the information to eventually update the information inside the current or new state
        output :: LogicOutput
        output = LogicOutput currBid lastBidder asset stateVal mempty

        -- | Prints a message in the console
        printMsg :: String -> Contract () AuctionSchema T.Text ()
        printMsg msg = logInfo @String msg

        -- | Prints an error in the console
        printError :: String -> Contract () AuctionSchema T.Text ()
        printError err = logError @String err

        -- | Returns an error
        returnError :: String -> Contract () AuctionSchema T.Text (Either (Value, PaymentPubKeyHash, Integer, Value, TxConstraints AuctionInput AuctionState) AuctionError)
        returnError err = pure $ Right $ Error $ T.pack err

        -- | Returns in case of sucess with the fields unaltered
        returnOk :: Contract () AuctionSchema T.Text (Either (Value, PaymentPubKeyHash, Integer, Value, TxConstraints AuctionInput AuctionState) AuctionError)
        returnOk = pure $ Left (currBid, lastBidder, asset, stateVal, mempty)

        -- | Returns in case of sucess with the help of a data structure that tracks the change of some fields
        returnOutputOk :: LogicOutput -> Contract () AuctionSchema T.Text (Either (Value, PaymentPubKeyHash, Integer, Value, TxConstraints AuctionInput AuctionState) AuctionError)
        returnOutputOk out = pure $ Left $ getOutput out

        -- | Returns in case of sucess with the all the fields especified with the same or a new value
        returnOkWith :: Value -> PaymentPubKeyHash -> Integer -> Value -> TxConstraints AuctionInput AuctionState -> Contract () AuctionSchema T.Text (Either (Value, PaymentPubKeyHash, Integer, Value, TxConstraints AuctionInput AuctionState) AuctionError)
        returnOkWith currBidRet lastBidderRet assetRet stateValRet constraintRet = pure $ Left (currBidRet, lastBidderRet, assetRet, stateValRet, constraintRet)





-- | Defining the contract to run on the playground
contract :: Contract () AuctionSchema T.Text ()
contract = selectList[initialise, closeAuction, collectFundsAndGiveRewards, continueAuction, bid, checkWinner]

endpoints :: Contract () AuctionSchema T.Text ()
endpoints = forever contract

mkSchemaDefinitions ''AuctionSchema