module DUsd.PriceOracle
  (startPriceOracle
  ,pushPriceOracle
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Chain (currentTime)
import Contract.Monad (Contract)
import Contract.Plutarch.Types (PRational, (%))
import Contract.PlutusData (Datum(..), OutputDatum(..), PlutusData(..), Redeemer(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionOutput(..), TransactionOutputWithRefScript(..))
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getWalletPubkeyhash, waitForTx)
import DUsd.CborTyped (priceOracleValidator)
import DUsd.Nft (lookupUtxo, mintNft)
import DUsd.Types (Protocol(..), UtxoId(..))
import Data.Array (cons, take)
import Data.Map (singleton)
import Effect.Aff (error)
import Effect.Exception (throw)

startPriceOracle :: Contract () UtxoId
startPriceOracle = do
  price <- liftAff getPrice
  time <- currentTime
  pkh <- getWalletPubkeyhash
  nftCs <- mintNft
  oracleVal <- priceOracleValidator pkh nftCs
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

pushPriceOracle :: Protocol -> Contract () Protocol
pushPriceOracle
  (Protocol
    protocolRec@{priceOracle:priceOracle@(UtxoId
      priceOracleRec@
      {nft: nftCs /\ nftTn
      ,script:oracleVal
      })
    }
  ) = do
  newPrice <- liftAff getPrice
  newTime <- currentTime
  let nextEntry = List [ toData newPrice , toData newTime ]
  oldIn /\ oldOut@(TransactionOutput { datum }) <- lookupUtxo priceOracle
  old <- case datum of
    (OutputDatum (Datum (List old))) -> pure old
    _ -> liftEffect $ throw "old datum was formatted incorectly or a datum hash or missing"
  let newDatum = List $ cons nextEntry (take 47 old)
  pkh <- getWalletPubkeyhash
  utxo <- waitForTx (scriptHashAddress (validatorHash oracleVal) Nothing)
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
            (Datum $ List $ cons newDatum old)
            DatumInline
            (Value.singleton nftCs nftTn one)
          <> Constraints.mustBeSignedBy (wrap pkh)
      )
  pure $ Protocol protocolRec{priceOracle=UtxoId priceOracleRec{guess=Just utxo}}


getPrice :: Aff PRational
getPrice = do
  liftM (error "price data caused division by zero") $ 4 % 10
  -- TODO connect to real price data

