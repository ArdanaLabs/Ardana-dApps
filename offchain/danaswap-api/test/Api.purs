module Test.Api
  ( openPoolSneaky
  , regularOpen
  , depositLiquiditySneaky
  , regularDeposit
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
import Contract.Value (CurrencySymbol, TokenName, Value, mkTokenName, mpsSymbol)
import Contract.Value as Value
import Ctl.Util (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import DanaSwap.Api (AssetClass, PoolDatum(..), PoolId, Protocol, getPoolById)
import DanaSwap.CborTyped (configAddressValidator)
import Data.BigInt (BigInt)
import Data.Map as Map
import Data.BigInt as BigInt

-- This module provides alternative implementations
-- for several normal API functions which take
-- extra "sneaky" (someone please suggest a better word)
-- options

type SneakyOptionsOpen =
  { reportIssued :: Maybe BigInt
  , actuallyMint :: Maybe (CurrencySymbol -> TokenName -> Value)
  , redeemer :: Maybe (TokenName -> Redeemer)
  }

regularOpen :: SneakyOptionsOpen
regularOpen =
  { reportIssued: Nothing
  , actuallyMint: Nothing
  , redeemer: Nothing
  }

-- | like open pool but takes several "sneaky" options
-- usefull for atack testing
openPoolSneaky
  :: SneakyOptionsOpen
  -> Protocol
  -> AssetClass
  -> AssetClass
  -> BigInt
  -> BigInt
  -> Contract () PoolId
openPoolSneaky
  sneaky
  { poolVal, liquidityMP, poolIdMP, configUtxo }
  ac1
  ac2
  amt1
  amt2 = do
  poolID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aaaa"
  let poolIdMph = mintingPolicyHash poolIdMP
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol poolIdMph
  let idNft = Value.singleton poolIdCs poolID one
  configVal <- configAddressValidator
  configAdrUtxos <- getUtxos (scriptHashAddress $ validatorHash configVal)
  let liquidity = amt1 * amt2
  liquidityCs <- liftContractM "failed to hash mp" (mpsSymbol $ mintingPolicyHash liquidityMP)
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
        <> Constraints.mustMintValueWithRedeemer -- Liquidity tokens

          ( fromMaybe
              (Redeemer $ List [ toData poolID, Constr zero [] ])
              (sneaky.redeemer <#> (_ $ poolID))
          )
          ( fromMaybe
              ( Value.singleton
                  liquidityCs
                  poolID
                  liquidity
              )
              (sneaky.actuallyMint <#> (_ $ liquidityCs) <#> (_ $ poolID))
          )
        <> Constraints.mustReferenceOutput configUtxo
        <> Constraints.mustPayToScript
          (validatorHash poolVal)
          ( Datum $ toData $
              PoolDatum
                { ac1
                , ac2
                , bal1: amt1
                , bal2: amt1
                , adminBal1: zero
                , adminBal2: zero
                , liquidity: fromMaybe liquidity sneaky.reportIssued
                , live: true
                }
          )
          DatumInline
          ( idNft
              <> Value.singleton (fst ac1) (snd ac1) amt1
              <> Value.singleton (fst ac2) (snd ac2) amt2
          )
    )
  void $ waitForTx (scriptHashAddress $ validatorHash poolVal) txid
  pure poolID

type SneakyOptionsDeposit =
  { reportIssued :: Maybe (BigInt -> BigInt)
  , actuallyMint :: Maybe (CurrencySymbol -> Value)
  , redeemer :: Maybe Redeemer
  }

regularDeposit :: SneakyOptionsDeposit
regularDeposit =
  { reportIssued: Nothing
  , actuallyMint: Nothing
  , redeemer: Nothing
  }

-- TODO ignores report issued for now
-- this won't matter till we have an actuall pool address validator script
depositLiquiditySneaky :: SneakyOptionsDeposit -> Protocol -> PoolId -> Contract () Unit
depositLiquiditySneaky sneaky protocol@{ poolVal, liquidityMP, poolIdMP } poolID = do
  (poolIn /\ poolOut) <- getPoolById protocol poolID
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol $ mintingPolicyHash poolIdMP
  liquidityCs <- liftContractM "failed to hash mp" (mpsSymbol $ mintingPolicyHash liquidityMP)
  let idNft = Value.singleton poolIdCs poolID one
  void $ waitForTx (scriptHashAddress $ validatorHash poolVal) =<<
    buildBalanceSignAndSubmitTx
      ( Lookups.unspentOutputs (Map.singleton poolIn poolOut)
          <> Lookups.mintingPolicy liquidityMP
          <> Lookups.validator poolVal
      )
      ( Constraints.mustSpendScriptOutput
          poolIn
          (Redeemer $ toData unit)
          <> Constraints.mustMintValueWithRedeemer
            ( fromMaybe
                (Redeemer $ List [ toData poolID, Constr zero [] ])
                sneaky.redeemer
            )
            ( fromMaybe
                (Value.singleton liquidityCs poolID $ BigInt.fromInt 10)
                (sneaky.actuallyMint <#> (_ $ liquidityCs))
            )

          <> Constraints.mustPayToScript
            (validatorHash poolVal)
            (Datum $ toData unit)
            DatumInline
            idNft
      )
