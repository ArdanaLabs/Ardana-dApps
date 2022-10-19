module Test.Api
  (openPoolWrongToken
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), PlutusData(..), Redeemer(..), toData)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (mintingPolicyHash, validatorHash)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (mkTokenName, mpsSymbol)
import Contract.Value as Value
import Ctl.Util (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import DanaSwap.Api (Protocol, PoolId)
import DanaSwap.CborTyped (configAddressValidator)

-- TODO rework this when the real open pool gets updated
openPoolWrongToken :: Protocol -> Contract () PoolId
openPoolWrongToken {poolVal,liquidityMP,poolIdMP,configUtxo} = do
  poolID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aaaa"
  wrongID <-liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aabb"
  let poolIdMph = mintingPolicyHash poolIdMP
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol poolIdMph
  --liquidityMPH <- liftContractM "failed to hash mp" $ mintingPolicyHash liq
  let idNft = Value.singleton poolIdCs poolID one
  configVal <- configAddressValidator
  configAdrUtxos <- getUtxos (scriptHashAddress $ validatorHash configVal)
  txid <- buildBalanceSignAndSubmitTx
    (Lookups.mintingPolicy poolIdMP
    <> Lookups.mintingPolicy liquidityMP
    <> Lookups.unspentOutputs configAdrUtxos
    )
    (Constraints.mustMintCurrencyWithRedeemer -- Pool id token
      poolIdMph
      (Redeemer $ toData unit)
      poolID
      one
    <> Constraints.mustMintCurrencyWithRedeemer -- Liquidity tokens
      (mintingPolicyHash liquidityMP)
      (Redeemer $ Constr one []) -- This should be correct but should probably be a named constant
      wrongID
      one
    <> Constraints.mustReferenceOutput configUtxo
    <> Constraints.mustPayToScript
        (validatorHash poolVal)
        (Datum $ toData unit) -- TODO real pool datum
        DatumInline
        idNft
    )
  void $ waitForTx (scriptHashAddress $ validatorHash poolVal) txid
  pure poolID
