module DUsd.Params
  ( initParams
  , updateParams
  , updateDebtFloor
  , updateLiquidationDiscount
  , updateLiquidationFee
  , updateLiquidationRatio
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash(..), scriptHashAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.Plutarch.Types (PRational)
import Contract.PlutusData (Datum(..), OutputDatum(..), Redeemer(..), fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionOutput(..), TransactionOutputWithRefScript(..))
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getWalletPubkeyhash, waitForTx)
import DUsd.CborTyped (paramAddressValidator)
import DUsd.Nft (lookupUtxo, mintNft)
import DUsd.Types (Params(..), UtxoId(..))
import Data.BigInt (BigInt)
import Data.Map (singleton)
import Effect.Exception (throw)

initParams :: Params -> Contract () UtxoId
initParams params = do
  paramNftCS <- mintNft
  pkh <- getWalletPubkeyhash
  paramAdrVal <- paramAddressValidator pkh paramNftCS
  utxo <- waitForTx (scriptHashAddress (validatorHash paramAdrVal) Nothing)
    =<< buildBalanceSignAndSubmitTx
      mempty
      ( Constraints.mustPayToScript
          (validatorHash paramAdrVal)
          (Datum $ toData params)
          DatumInline
          (Value.singleton paramNftCS adaToken one)
      )
  pure $ UtxoId
    { nft: paramNftCS /\ adaToken
    , script: paramAdrVal
    , guess: Just utxo
    }

updateParams :: UtxoId -> (Params -> Params) -> Contract () UtxoId
updateParams utxoid@(UtxoId rec@{ nft: cs /\ tn, script }) paramUpdate = do
  txIn /\ oldOut@(TransactionOutput { datum: outDatum }) <- lookupUtxo utxoid
  Datum datum <- case outDatum of
    OutputDatum datum -> pure datum
    _ -> liftEffect $ throw "no datum or datum was datum hash"
  oldParams :: Params <- fromData datum # liftContractM "old datum didn't parse"
  pkh <- getWalletPubkeyhash
  utxo <- waitForTx (scriptHashAddress (validatorHash script) Nothing)
    =<< buildBalanceSignAndSubmitTx
      ( Lookups.validator script
          <> Lookups.unspentOutputs
            ( singleton txIn
                (TransactionOutputWithRefScript { output: oldOut, scriptRef: Nothing })
            )
      )
      ( Constraints.mustSpendScriptOutput txIn
          (Redeemer $ toData unit)
          <>
            Constraints.mustBeSignedBy (PaymentPubKeyHash pkh)
          <>
            Constraints.mustPayToScript
              (validatorHash script)
              (Datum $ toData $ paramUpdate oldParams)
              DatumInline
              (Value.singleton cs tn one)
      )
  pure $ UtxoId rec { guess = Just utxo }

updateDebtFloor :: UtxoId -> BigInt -> Contract () UtxoId
updateDebtFloor utxoid new = updateParams utxoid
  (\(Params p) -> Params $ p { debtFloor = new })

updateLiquidationDiscount :: UtxoId -> PRational -> Contract () UtxoId
updateLiquidationDiscount utxoid new = updateParams utxoid
  (\(Params p) -> Params $ p { liquidationDiscount = new })

updateLiquidationFee :: UtxoId -> BigInt -> Contract () UtxoId
updateLiquidationFee utxoid new = updateParams utxoid
  (\(Params p) -> Params $ p { liquidationFee = new })

updateLiquidationRatio :: UtxoId -> PRational -> Contract () UtxoId
updateLiquidationRatio utxoid new = updateParams utxoid
  (\(Params p) -> Params $ p { liquidationRatio = new })
