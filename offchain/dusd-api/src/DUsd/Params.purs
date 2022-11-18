module DUsd.Params
  ( initProtocolParams
  , updateProtocolParams
  , updateDebtFloor
  , updateLiquidationDiscount
  , updateLiquidationFee
  , updateLiquidationRatio
  -- Testing
  , updateDebtFloor'
  , updateLiquidationFee'
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash(..), scriptHashAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.Numeric.Natural (Natural, toBigInt)
import Contract.Plutarch.Types (PRational)
import Contract.PlutusData (Datum(..), OutputDatum(..), Redeemer(..), fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionOutput(..), TransactionOutputWithRefScript(..))
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getWalletPubKeyHash, waitForTx)
import DUsd.CborTyped (paramAddressValidator)
import DUsd.Nft (lookupUtxo, mintNft)
import DUsd.Types (ProtocolParams(..), UtxoId(..))
import Data.BigInt (BigInt)
import Data.Map (singleton)
import Effect.Exception (throw)

initProtocolParams :: ProtocolParams -> Contract () UtxoId
initProtocolParams params = do
  paramNftCS <- mintNft
  pkh <- getWalletPubKeyHash
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

updateProtocolParams :: UtxoId -> (ProtocolParams -> ProtocolParams) -> Contract () UtxoId
updateProtocolParams utxoid@(UtxoId rec@{ nft: cs /\ tn, script }) paramUpdate = do
  txIn /\ oldOut@(TransactionOutput { datum: outDatum }) <- lookupUtxo utxoid
  Datum datum <- case outDatum of
    OutputDatum datum -> pure datum
    _ -> liftEffect $ throw "no datum or datum was datum hash"
  oldParams :: ProtocolParams <- fromData datum # liftContractM "old datum didn't parse"
  pkh <- getWalletPubKeyHash
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

updateDebtFloor :: UtxoId -> Natural -> Contract () UtxoId
updateDebtFloor utxo nat = updateDebtFloor' utxo (toBigInt nat)

-- unsafe version that allows negatives for testing
updateDebtFloor' :: UtxoId -> BigInt -> Contract () UtxoId
updateDebtFloor' utxoid new = updateProtocolParams utxoid
  (\(ProtocolParams p) -> ProtocolParams $ p { debtFloor = new })

updateLiquidationDiscount :: UtxoId -> PRational -> Contract () UtxoId
updateLiquidationDiscount utxoid new = updateProtocolParams utxoid
  (\(ProtocolParams p) -> ProtocolParams $ p { liquidationDiscount = new })

updateLiquidationFee :: UtxoId -> Natural -> Contract () UtxoId
updateLiquidationFee utxoid nat = updateLiquidationFee' utxoid (toBigInt nat)

-- unsafe version that allows negatives for testing
updateLiquidationFee' :: UtxoId -> BigInt -> Contract () UtxoId
updateLiquidationFee' utxoid new = updateProtocolParams utxoid
  (\(ProtocolParams p) -> ProtocolParams $ p { liquidationFee = new })

updateLiquidationRatio :: UtxoId -> PRational -> Contract () UtxoId
updateLiquidationRatio utxoid new = updateProtocolParams utxoid
  (\(ProtocolParams p) -> ProtocolParams $ p { liquidationRatio = new })
