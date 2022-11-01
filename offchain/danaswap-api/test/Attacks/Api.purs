module Test.Attacks.Api
  ( openPoolWrongTokenRightRedeemer
  , openPoolWrongTokenWrongRedeemer
  , openPoolMultipleTokens
  , depositLiquidityWrongTokenWrongRedeemer
  , depositLiquidityWrongTokenRightRedeemer
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
import Ctl.Utils (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import DanaSwap.Api (PoolId, Protocol(..), getPoolById)
import DanaSwap.CborTyped (configAddressValidator)
import Data.BigInt as BigInt
import Data.Map as Map

-- TODO rework this when the real open pool gets updated
openPoolWrongTokenWrongRedeemer :: Protocol -> Contract () PoolId
openPoolWrongTokenWrongRedeemer (Protocol { poolAdrVal, liquidityMP, poolIdMP, configUtxo }) = do
  poolID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aaaa"
  wrongID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aabb"
  let poolIdMph = mintingPolicyHash poolIdMP
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol poolIdMph
  let idNft = Value.singleton poolIdCs poolID one
  configVal <- configAddressValidator
  configAdrUtxos <- getUtxos (scriptHashAddress $ validatorHash configVal)
  txid <- buildBalanceSignAndSubmitTx
    ( Lookups.mintingPolicy poolIdMP
        <> Lookups.mintingPolicy liquidityMP
        <> Lookups.unspentOutputs configAdrUtxos
    )
    ( Constraints.mustMintCurrencyWithRedeemer -- Pool id token

        poolIdMph
        (Redeemer $ toData unit)
        poolID
        one
        <> Constraints.mustMintCurrencyWithRedeemer -- Liquidity tokens

          (mintingPolicyHash liquidityMP)
          (Redeemer $ List [ toData wrongID, Constr zero [] ])
          wrongID
          one
        <> Constraints.mustReferenceOutput configUtxo
        <> Constraints.mustPayToScript
          (validatorHash poolAdrVal)
          (Datum $ toData unit) -- TODO real pool datum
          DatumInline
          idNft
    )
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) txid
  pure poolID

openPoolWrongTokenRightRedeemer :: Protocol -> Contract () PoolId
openPoolWrongTokenRightRedeemer (Protocol { poolAdrVal, liquidityMP, poolIdMP, configUtxo }) = do
  poolID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aaaa"
  wrongID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aabb"
  let poolIdMph = mintingPolicyHash poolIdMP
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol poolIdMph
  --liquidityMPH <- liftContractM "failed to hash mp" $ mintingPolicyHash liq
  let idNft = Value.singleton poolIdCs poolID one
  configVal <- configAddressValidator
  configAdrUtxos <- getUtxos (scriptHashAddress $ validatorHash configVal)
  txid <- buildBalanceSignAndSubmitTx
    ( Lookups.mintingPolicy poolIdMP
        <> Lookups.mintingPolicy liquidityMP
        <> Lookups.unspentOutputs configAdrUtxos
    )
    ( Constraints.mustMintCurrencyWithRedeemer -- Pool id token

        poolIdMph
        (Redeemer $ toData unit)
        poolID
        one
        <> Constraints.mustMintCurrencyWithRedeemer -- Liquidity tokens

          (mintingPolicyHash liquidityMP)
          (Redeemer $ List [ toData poolID, Constr zero [] ])
          wrongID
          one
        <> Constraints.mustReferenceOutput configUtxo
        <> Constraints.mustPayToScript
          (validatorHash poolAdrVal)
          (Datum $ toData unit) -- TODO real pool datum
          DatumInline
          idNft
    )
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) txid
  pure poolID

-- TODO rework this when the real open pool gets updated
openPoolMultipleTokens :: Protocol -> Contract () PoolId
openPoolMultipleTokens (Protocol { poolAdrVal, liquidityMP, poolIdMP, configUtxo }) = do
  poolID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aaaa"
  wrongID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aabb"
  let poolIdMph = mintingPolicyHash poolIdMP
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol poolIdMph
  --liquidityMPH <- liftContractM "failed to hash mp" $ mintingPolicyHash liq
  let idNft = Value.singleton poolIdCs poolID one
  configVal <- configAddressValidator
  configAdrUtxos <- getUtxos (scriptHashAddress $ validatorHash configVal)
  txid <- buildBalanceSignAndSubmitTx
    ( Lookups.mintingPolicy poolIdMP
        <> Lookups.mintingPolicy liquidityMP
        <> Lookups.unspentOutputs configAdrUtxos
    )
    ( Constraints.mustMintCurrencyWithRedeemer -- Pool id token

        poolIdMph
        (Redeemer $ toData unit)
        poolID
        one
        <> Constraints.mustMintCurrencyWithRedeemer -- Liquidity tokens

          (mintingPolicyHash liquidityMP)
          (Redeemer $ List [ toData poolID, Constr zero [] ]) -- This should be correct but should probably be a named constant
          poolID
          one
        <> Constraints.mustMintCurrencyWithRedeemer -- Liquidity tokens

          (mintingPolicyHash liquidityMP)
          (Redeemer $ Constr one []) -- This should be correct but should probably be a named constant
          wrongID
          one
        <> Constraints.mustReferenceOutput configUtxo
        <> Constraints.mustPayToScript
          (validatorHash poolAdrVal)
          (Datum $ toData unit) -- TODO real pool datum
          DatumInline
          idNft
    )
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) txid
  pure poolID

-- TODO rework with real depositLiquidity
depositLiquidityWrongTokenRightRedeemer :: Protocol -> PoolId -> Contract () Unit
depositLiquidityWrongTokenRightRedeemer protocol@(Protocol { poolAdrVal, liquidityMP, poolIdMP }) poolID = do
  (poolIn /\ poolOut) <- getPoolById protocol poolID
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol $ mintingPolicyHash poolIdMP
  wrongID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aabb"
  let idNft = Value.singleton poolIdCs poolID one
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) =<<
    buildBalanceSignAndSubmitTx
      ( Lookups.unspentOutputs (Map.singleton poolIn poolOut)
          <> Lookups.mintingPolicy liquidityMP
          <> Lookups.validator poolAdrVal
      )
      ( Constraints.mustSpendScriptOutput
          poolIn
          (Redeemer $ toData unit)
          <> Constraints.mustMintCurrencyWithRedeemer
            (mintingPolicyHash liquidityMP)
            (Redeemer $ List [ toData poolID, Constr one [] ])
            wrongID
            (BigInt.fromInt 10)
          <> Constraints.mustPayToScript
            (validatorHash poolAdrVal)
            (Datum $ toData unit)
            DatumInline
            idNft
      )

depositLiquidityWrongTokenWrongRedeemer :: Protocol -> PoolId -> Contract () Unit
depositLiquidityWrongTokenWrongRedeemer protocol@(Protocol { poolAdrVal, liquidityMP, poolIdMP }) poolID = do
  (poolIn /\ poolOut) <- getPoolById protocol poolID
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol $ mintingPolicyHash poolIdMP
  wrongID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aabb"
  let idNft = Value.singleton poolIdCs poolID one
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) =<<
    buildBalanceSignAndSubmitTx
      ( Lookups.unspentOutputs (Map.singleton poolIn poolOut)
          <> Lookups.mintingPolicy liquidityMP
          <> Lookups.validator poolAdrVal
      )
      ( Constraints.mustSpendScriptOutput
          poolIn
          (Redeemer $ toData unit)
          <> Constraints.mustMintCurrencyWithRedeemer
            (mintingPolicyHash liquidityMP)
            (Redeemer $ List [ toData wrongID, Constr one [] ])
            wrongID
            (BigInt.fromInt 10)
          <> Constraints.mustPayToScript
            (validatorHash poolAdrVal)
            (Datum $ toData unit)
            DatumInline
            idNft
      )
