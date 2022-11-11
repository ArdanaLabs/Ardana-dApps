module DUsd.Params
  ( initParams
  , updateParams
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract)
import Contract.PlutusData (Datum(..), toData)
import Contract.Scripts (validatorHash)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getWalletPubkeyhash, waitForTx)
import DUsd.CborTyped (paramAddressValidator)
import DUsd.Nft (mintNft)
import DUsd.Types (Params, ParamsInfo, UtxoId(..))

initParams :: Params -> Contract () UtxoId
initParams params = do
  paramNftCS <- mintNft
  pkh <- getWalletPubkeyhash
  paramAdrVal <- paramAddressValidator pkh paramNftCS
  txid <- waitForTx (scriptHashAddress $ validatorHash paramAdrVal)
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
    , guess: Just txid
    }

updateParams :: ParamsInfo -> Params -> Contract () Unit
updateParams _ _ = pure unit

