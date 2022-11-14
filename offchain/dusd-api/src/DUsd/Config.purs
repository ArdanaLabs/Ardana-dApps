module DUsd.Config
  ( initConfigWith
  , updateConfig
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Log (logDebug')
import Contract.Monad (Contract)
import Contract.PlutusData (Datum(..), OutputDatum(..), PlutusData(..), Redeemer(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionOutput(..), TransactionOutputWithRefScript(..))
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, adaToken)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getWalletPubkeyhash, waitForTx)
import DUsd.CborTyped (configAddressValidator)
import DUsd.Nft (lookupUtxo)
import DUsd.Types (UtxoId(..))
import Data.Array (cons)
import Data.Map (singleton)
import Effect.Exception (throw)

-- | Initializes the config utxo with a given datum
initConfigWith :: CurrencySymbol -> PlutusData -> Contract () UtxoId
initConfigWith nftCs datum = do
  logDebug' "start config utxo init"
  pkh <- getWalletPubkeyhash
  configVal <- configAddressValidator pkh nftCs
  utxo <- waitForTx (scriptHashAddress (validatorHash configVal) Nothing)
    =<< buildBalanceSignAndSubmitTx
      (mempty)
      ( Constraints.mustPayToScript
          (validatorHash configVal)
          (Datum $ List [ datum ]) -- TODO real protocol datum
          DatumInline
          (Value.singleton nftCs adaToken one)
      )
  logDebug' "protocol init complete"
  pure $ UtxoId
    { nft: nftCs /\ adaToken
    , script: configVal
    , guess: Just utxo
    }

-- | technically not part of this version of the protocol
-- but pushes a new datum onto the config utxo
updateConfig :: PlutusData -> UtxoId -> Contract () UtxoId
updateConfig newDatum utxoId@(UtxoId rec@{ nft: nftCs /\ nftTn, script: configVal }) = do
  oldIn /\ oldOut@(TransactionOutput { datum }) <- lookupUtxo utxoId
  old <- case datum of
    (OutputDatum (Datum (List old))) -> pure old
    _ -> liftEffect $ throw "old datum was formatted incorectly or a datum hash or missing"
  pkh <- getWalletPubkeyhash
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
            (Datum $ List $ cons newDatum old)
            DatumInline
            (Value.singleton nftCs nftTn one)
          <> Constraints.mustBeSignedBy (wrap pkh)
      )
  logDebug' "config utxo submitted, waiting for confirmation"
  logDebug' "protocol init complete"
  pure $ UtxoId rec { guess = Just utxo }
