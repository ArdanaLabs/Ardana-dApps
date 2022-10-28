module Test.Attacks.Api
  ( openPoolSneaky
  , regularOpen
  , depositLiquiditySneaky
  , regularDeposit
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress, scriptHashAddress)
import Contract.BalanceTxConstraints (mustNotSpendUtxoWithOutRef) as Constraints
import Contract.Hashing (datumHash)
import Contract.Log (logError')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), PlutusData(..), Redeemer(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (mintingPolicyHash, validatorHash)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints (mustMintCurrencyWithRedeemer, mustMintValueWithRedeemer, mustPayToScript, mustReferenceOutput, mustSpendPubKeyOutput, mustSpendScriptOutput) as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value, mkTokenName, mpsSymbol)
import Contract.Value as Value
import Ctl.Util (buildBalanceSignAndSubmitTx, buildBalanceSignAndSubmitTx', getUtxos, waitForTx)
import DanaSwap.Api (AssetClass, PoolDatum(..), PoolId, Protocol, getPoolById, seedTx)
import DanaSwap.CborTyped (configAddressValidator)
import Data.BigInt (BigInt, toNumber)
import Data.BigInt as BigInt
import Data.Int (floor)
import Data.Map as Map
import Math (sqrt)

-- This module provides alternative implementations
-- for several normal API functions which take
-- extra "sneaky" (someone please suggest a better word)
-- options

type SneakyOptionsOpen =
  { reportIssued :: Maybe BigInt
  , actuallyMint :: Maybe (CurrencySymbol -> TokenName -> Value)
  , redeemer :: Maybe (TokenName -> Redeemer)
  , hasConfig :: Boolean
  , badConfig :: Maybe TransactionInput
  , numberOfIdsToMint :: Maybe BigInt
  , keepId :: Boolean
  , idToMint :: Maybe TokenName
  , spendSeedTx :: Boolean
  }

regularOpen :: SneakyOptionsOpen
regularOpen =
  { reportIssued: Nothing
  , actuallyMint: Nothing
  , redeemer: Nothing
  , hasConfig: true
  , badConfig: Nothing
  , numberOfIdsToMint: Nothing
  , keepId: false
  , idToMint: Nothing
  , spendSeedTx: true
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
  { poolAdrVal, liquidityMP, poolIdMP, configUtxo }
  ac1
  ac2
  amt1
  amt2 = do
  seed <- seedTx
  poolID' <- liftContractM "failed to make poolID" $ datumHash (Datum (toData seed)) <#> unwrap >>= mkTokenName
  let poolID = fromMaybe poolID' sneaky.idToMint
  let poolIdMph = mintingPolicyHash poolIdMP
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol poolIdMph
  let idNft = Value.singleton poolIdCs poolID one
  configVal <- configAddressValidator
  configAdrUtxos <- getUtxos (scriptHashAddress $ validatorHash configVal)
  liquidityCs <- liftContractM "failed to hash mp" (mpsSymbol $ mintingPolicyHash liquidityMP)
  adr <- getWalletAddress >>= liftContractM "no wallet"
  utxos <- getUtxos adr
  when (not sneaky.spendSeedTx) $ logError' $ "utxos:" <> show utxos
  let
    liq = BigInt.fromInt $ floor $ sqrt $ toNumber $ amt1 * amt2
    pool = PoolDatum
      { ac1
      , ac2
      , bal1: amt1
      , bal2: amt1
      , adminBal1: zero
      , adminBal2: zero
      , liquidity: fromMaybe liq sneaky.reportIssued
      , live: true
      }
  txid <- buildBalanceSignAndSubmitTx'
    ( Lookups.mintingPolicy poolIdMP
        <> Lookups.mintingPolicy liquidityMP
        <> Lookups.unspentOutputs configAdrUtxos
        <> Lookups.unspentOutputs utxos
    )
    ( Constraints.mustMintCurrencyWithRedeemer
        poolIdMph
        (Redeemer $ toData unit)
        poolID
        (fromMaybe one sneaky.numberOfIdsToMint)
        <>
          ( if liq >= one then
              Constraints.mustMintValueWithRedeemer
                ( fromMaybe
                    (Redeemer $ List [ toData poolID, Constr zero [] ])
                    (sneaky.redeemer <#> (_ $ poolID))
                )
                ( fromMaybe
                    ( Value.singleton
                        liquidityCs
                        poolID
                        liq
                    )
                    (sneaky.actuallyMint <#> (_ $ liquidityCs) <#> (_ $ poolID))
                )
            else mempty
          )
        <>
          ( if sneaky.hasConfig then Constraints.mustReferenceOutput (fromMaybe configUtxo sneaky.badConfig)
            else mempty
          )
        <> (if sneaky.spendSeedTx then Constraints.mustSpendPubKeyOutput seed else mempty)
        <> Constraints.mustPayToScript
          (validatorHash poolAdrVal)
          (Datum $ toData pool)
          DatumInline
          ( (if sneaky.keepId then mempty else idNft)
              <> Value.singleton (fst ac1) (snd ac1) amt1
              <> Value.singleton (fst ac2) (snd ac2) amt2
          )
    )
    ( if sneaky.spendSeedTx then mempty
      else
        Constraints.mustNotSpendUtxoWithOutRef seed
    )
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) txid
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
depositLiquiditySneaky sneaky protocol@{ poolAdrVal, liquidityMP, poolIdMP } poolID = do
  (poolIn /\ poolOut) <- getPoolById protocol poolID
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol $ mintingPolicyHash poolIdMP
  liquidityCs <- liftContractM "failed to hash mp" (mpsSymbol $ mintingPolicyHash liquidityMP)
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
            (validatorHash poolAdrVal)
            (Datum $ toData unit)
            DatumInline
            idNft
      )

