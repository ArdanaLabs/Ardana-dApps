module Test.Attacks.Api
  ( updateConfigAttack
  , UpdateAttack
  , defUpdate
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
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getWalletPubkeyhash, waitForTx)
import DUsd.Nft (lookupUtxo)
import DUsd.Types (UtxoId(..))
import Data.Array (cons)
import Data.Map (singleton)
import Effect.Exception (throw)

-- There's a package for this but it's not in
-- ps-pkgs and we only need 2 lines of code from it anyway

type UpdateAttack =
  { noSignature :: Boolean
  , overwriteDatum :: Maybe PlutusData
  }

defUpdate :: UpdateAttack
defUpdate =
  { noSignature: false
  , overwriteDatum: Nothing
  }

-- | technically not part of this version of the protocol
updateConfigAttack :: UpdateAttack -> PlutusData -> UtxoId -> Contract () UtxoId
updateConfigAttack
  atack
  newDatum
  utxoId@(UtxoId rec@{ nft: nftCs /\ nftTn, script: configVal }) = do
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
            (Datum $ (fromMaybe (List $ cons newDatum old) atack.overwriteDatum))
            DatumInline
            (Value.singleton nftCs nftTn one)
          <> (if atack.noSignature then mempty else Constraints.mustBeSignedBy (wrap pkh))
      )
  logDebug' "config utxo submitted, waiting for confirmation"
  logDebug' "protocol init complete"
  pure $ UtxoId rec { guess = Just utxo }
