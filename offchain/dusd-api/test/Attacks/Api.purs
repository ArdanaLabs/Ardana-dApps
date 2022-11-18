module Test.Attacks.Api
  ( updateConfigUtxoAttack
  , UpdateConfAttack
  , defConfUpdate
  , updateProtocolParamsAttack
  , defParamUpdate
  ) where

import Contract.Prelude

import Contract.Address (PaymentPubKeyHash(..), scriptHashAddress)
import Contract.Log (logDebug')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), OutputDatum(..), PlutusData(..), Redeemer(..), fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionOutput(..), TransactionOutputWithRefScript(..))
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getWalletPubKeyHash, waitForTx)
import DUsd.Api (ProtocolParams)
import DUsd.Nft (lookupUtxo)
import DUsd.Types (UtxoId(..))
import Data.Array (cons)
import Data.Map (singleton)
import Effect.Exception (throw)

-- There's a package for this but it's not in
-- ps-pkgs and we only need 2 lines of code from it anyway

type UpdateConfAttack =
  { noSignature :: Boolean
  , overwriteDatum :: Maybe PlutusData
  }

defConfUpdate :: UpdateConfAttack
defConfUpdate =
  { noSignature: false
  , overwriteDatum: Nothing
  }

-- | technically not part of this version of the protocol
updateConfigUtxoAttack :: UpdateConfAttack -> PlutusData -> UtxoId -> Contract () UtxoId
updateConfigUtxoAttack
  attack
  newDatum
  utxoId@(UtxoId rec@{ nft: nftCs /\ nftTn, script: configVal }) = do
  oldIn /\ oldOut@(TransactionOutput { datum }) <- lookupUtxo utxoId
  old <- case datum of
    (OutputDatum (Datum (List old))) -> pure old
    _ -> liftEffect $ throw "old datum was formatted incorectly or a datum hash or missing"
  pkh <- getWalletPubKeyHash
  utxo <- waitForTx (scriptHashAddress (validatorHash configVal) Nothing)
    =<< buildBalanceSignAndSubmitTx
      ( Lookups.unspentOutputs
          ( singleton oldIn
              (TransactionOutputWithRefScript { output: oldOut, scriptRef: Nothing })
          )
          <> Lookups.validator configVal
      )
      ( Constraints.mustSpendScriptOutput
          oldIn
          (Redeemer $ toData unit)
          <> Constraints.mustPayToScript
            (validatorHash configVal)
            (Datum $ (fromMaybe (List $ cons newDatum old) attack.overwriteDatum))
            DatumInline
            (Value.singleton nftCs nftTn one)
          <> (if attack.noSignature then mempty else Constraints.mustBeSignedBy (wrap pkh))
      )
  logDebug' "config utxo submitted, waiting for confirmation"
  logDebug' "protocol init complete"
  pure $ UtxoId rec { guess = Just utxo }

type UpdateParamAttack =
  { noSignature :: Boolean
  }

defParamUpdate :: UpdateParamAttack
defParamUpdate = { noSignature: false }

updateProtocolParamsAttack :: UpdateParamAttack -> UtxoId -> (ProtocolParams -> ProtocolParams) -> Contract () UtxoId
updateProtocolParamsAttack attack utxoid@(UtxoId rec@{ nft: cs /\ tn, script }) paramUpdate = do
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
            ( if attack.noSignature then mempty
              else Constraints.mustBeSignedBy (PaymentPubKeyHash pkh)
            )
          <>
            Constraints.mustPayToScript
              (validatorHash script)
              (Datum $ toData $ paramUpdate oldParams)
              DatumInline
              (Value.singleton cs tn one)
      )
  pure $ UtxoId rec { guess = Just utxo }
