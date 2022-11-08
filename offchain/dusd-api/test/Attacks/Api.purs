module Test.Attacks.Api
  (updateProtoclAttack
  ,UpdateAttack
  ,defUpdate
  )where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Log (logDebug')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), OutputDatum(..), PlutusData(..), Redeemer(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionOutput(..), TransactionOutputWithRefScript(..))
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (adaToken)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, waitForTx)
import Data.Array (cons)
import Data.Map (singleton)
import Dusd.Api (Protocol(..), getWalletPubkeyhash)
import Effect.Exception (throw)

-- There's a package for this but it's not in
-- ps-pkgs and we only need 2 lines of code from it anyway

type UpdateAttack =
  {noSignature :: Boolean
  ,overwriteDatum :: Maybe PlutusData
  }

defUpdate :: UpdateAttack
defUpdate =
  {noSignature : false
  , overwriteDatum : Nothing
  }


-- | technically not part of this version of the protocol
updateProtoclAttack :: UpdateAttack -> PlutusData -> Protocol -> Contract () Protocol
updateProtoclAttack attack new (Protocol { utxo: oldUtxo, nftCs, configVal }) = do
  TransactionOutput { datum } <- getUtxo oldUtxo >>= liftContractM "lookup failed. Maybe config utxo was already spent"
  old <- case datum of
    (OutputDatum (Datum (List old))) -> pure old
    _ -> liftEffect $ throw "old datum was formatted incorectly or a datum hash or missing"
  oldOut <- getUtxo oldUtxo >>= liftContractM "lookup failed"
  pkh <- getWalletPubkeyhash
  txid <- buildBalanceSignAndSubmitTx
    ( Lookups.unspentOutputs
        ( singleton oldUtxo
            (TransactionOutputWithRefScript { output: oldOut, scriptRef: Nothing })
        )
        <> Lookups.validator configVal
    )
    ( Constraints.mustSpendScriptOutput
        oldUtxo
        (Redeemer $ toData unit)
        <> Constraints.mustPayToScript
          (validatorHash configVal)
          (Datum $ fromMaybe (List $ cons new old) attack.overwriteDatum)
          DatumInline
          (Value.singleton nftCs adaToken one)
        <> (if attack.noSignature then mempty else Constraints.mustBeSignedBy (wrap pkh))
    )
  logDebug' "config utxo submitted, waiting for confirmation"
  utxo <- waitForTx (scriptHashAddress $ validatorHash configVal) txid
  logDebug' "protocol init complete"
  pure $ Protocol { utxo, datum: new, nftCs, configVal }
