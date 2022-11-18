module DUsd.PriceOracle
  (startPriceOracle
  ,startPriceOracle'
  ,pushPriceOracle
  ,pushPriceOracle'
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Chain (currentTime)
import Contract.Log (logError')
import Contract.Monad (Contract, liftContractM)
import Contract.Plutarch.Types (PRational, (%))
import Contract.PlutusData (Datum(..), OutputDatum(..), PlutusData(..), Redeemer(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Time (Extended(..), Interval(..), LowerBound(..), POSIXTime(..), UpperBound(..))
import Contract.Transaction (TransactionOutput(..), TransactionOutputWithRefScript(..))
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getWalletPubKeyHash, waitForTx)
import DUsd.CborTyped (priceOracleValidator)
import DUsd.Nft (lookupUtxo, mintNft)
import DUsd.Types (Protocol(..), UtxoId(..))
import Data.Array (cons, take)
import Data.BigInt as BigInt
import Data.Map (singleton)
import Data.Time.Duration (class Duration, Hours(..), Minutes(..), Seconds(..), fromDuration)
import Effect.Aff (error)
import Effect.Exception (throw)

-- | Starts the price oracle
startPriceOracle :: Contract () UtxoId
startPriceOracle = do
  startPriceOracle' (Hours 1.0) (Minutes 1.0)

-- | Starts the price oracle with a custom interval time (important for tests to not take 1 hour)
startPriceOracle'
  :: forall d1 d2.
  Duration d1 => Duration d2 =>
  d1 -> d2 -> Contract () UtxoId
startPriceOracle' interval' margin' = do
  interval <- durationToPoxisTime interval'
  margin <- durationToPoxisTime margin'
  price <- liftAff getPrice
  time <- currentTime
  pkh <- getWalletPubKeyHash
  nftCs <- mintNft
  oracleVal <- priceOracleValidator interval margin pkh nftCs
  utxo <- waitForTx (scriptHashAddress (validatorHash oracleVal) Nothing)
    =<< buildBalanceSignAndSubmitTx
      mempty
      ( Constraints.mustPayToScript
        (validatorHash oracleVal)
        (Datum $ List [ List [toData time , toData price]])
        DatumInline
        (Value.singleton nftCs adaToken one)
      )
  pure $ UtxoId
    { nft : nftCs /\ adaToken
    , script: oracleVal
    , guess : Just utxo
    }

-- Updates the price oracle
pushPriceOracle :: Protocol -> Contract () Unit
pushPriceOracle (Protocol{priceOracle}) = pushPriceOracle' priceOracle

-- Updates the price oracle by utxoid
-- usefull for testing without initializing the whole
-- porotocol
pushPriceOracle' :: UtxoId -> Contract () Unit
pushPriceOracle'
    priceOracle@(UtxoId
      {nft: nftCs /\ nftTn
      ,script:oracleVal
      }
    )
  = do
  newPrice <- liftAff getPrice
  newTime <- currentTime
  let nextEntry = List [ toData newTime , toData newPrice ]
  oldIn /\ oldOut@(TransactionOutput { datum }) <- lookupUtxo priceOracle
  old <- case datum of
    (OutputDatum (Datum (List old))) -> pure old
    _ -> liftEffect $ throw "old datum was formatted incorectly or a datum hash or missing"
  let newDatum = List $ cons nextEntry (take 47 old)
  pkh <- getWalletPubKeyHash
  halfMargin <- durationToPoxisTime $ Seconds 30.0
  let start = POSIXTime $ unwrap newTime - unwrap halfMargin
  let end = POSIXTime $ unwrap newTime + unwrap halfMargin
  logError' $ "next" <> show newTime
  logError' $ "ent" <> show nextEntry
  logError' $ "datum" <> show newDatum
  logError' $ "start" <> show start
  logError' $ "end" <> show end
  void $ waitForTx (scriptHashAddress (validatorHash oracleVal) Nothing)
    =<< buildBalanceSignAndSubmitTx
      ( Lookups.unspentOutputs
          ( singleton oldIn
              (TransactionOutputWithRefScript { output: oldOut, scriptRef: Nothing })
          )
          <> Lookups.validator oracleVal
      )
      ( Constraints.mustSpendScriptOutput
          oldIn
          (Redeemer $ toData unit)
          <> Constraints.mustPayToScript
            (validatorHash oracleVal)
            (Datum $ newDatum)
            DatumInline
            (Value.singleton nftCs nftTn one)
          <> Constraints.mustBeSignedBy (wrap pkh)
          <> Constraints.mustValidateIn
            (Interval
              {from : LowerBound (Finite $ POSIXTime $ unwrap newTime - unwrap halfMargin) true
              , to : UpperBound (Finite $ POSIXTime $ unwrap newTime + unwrap halfMargin) true
              }
            )
      )

durationToPoxisTime :: forall d . Duration d => d -> Contract () POSIXTime
durationToPoxisTime d =
  liftContractM "time conversion failed"
  $ POSIXTime <$> BigInt.fromNumber (( _ / 1000.0) $ unwrap $ fromDuration d)

getPrice :: Aff PRational
getPrice = do
  liftM (error "price data caused division by zero") $ 4 % 10
  -- TODO connect to real price data

