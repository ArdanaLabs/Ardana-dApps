module Test.Attacks.Api
  ( openPoolAttack
  , regularOpen
  , depositLiquidityAttack
  , regularDeposit
  , swapLeftAttack
  , regularSwap
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress, scriptHashAddress)
import Contract.BalanceTxConstraints (mustNotSpendUtxoWithOutRef) as Constraints
import Contract.Hashing (datumHash)
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), OutputDatum(..), PlutusData(..), Redeemer(..), fromData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (mintingPolicyHash, validatorHash)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints (mustMintCurrencyWithRedeemer, mustMintValueWithRedeemer, mustPayToScript, mustReferenceOutput, mustSpendPubKeyOutput, mustSpendScriptOutput) as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value, mkTokenName, mpsSymbol)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, buildBalanceSignAndSubmitTx', getUtxos, waitForTx)
import DanaSwap.Api (AssetClass, PoolAction(..), PoolDatum(..), PoolId, PoolRed(..), Protocol(..), ceilDiv, getPoolById, seedTx)
import DanaSwap.CborTyped (configAddressValidator)
import Data.BigInt (BigInt, toNumber)
import Data.BigInt as BigInt
import Data.Int (floor)
import Data.Map as Map
import Effect.Exception (throw)
import Math (sqrt)

-- This module provides alternative implementations
-- for several normal API functions which take
-- extra "attack" (someone please suggest a better word)
-- options

type AttackOptionsOpen =
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

regularOpen :: AttackOptionsOpen
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

-- | like open pool but takes several "attack" options
-- usefull for atack testing
openPoolAttack
  :: AttackOptionsOpen
  -> Protocol
  -> AssetClass
  -> AssetClass
  -> BigInt
  -> BigInt
  -> Contract () PoolId
openPoolAttack
  attack
  (Protocol { poolAdrVal, liquidityMP, poolIdMP, configUtxo })
  ac1
  ac2
  amt1
  amt2 = do
  seed <- seedTx
  poolID' <- liftContractM "failed to make poolID" $ datumHash (Datum (toData seed)) <#> unwrap >>= mkTokenName
  let poolID = fromMaybe poolID' attack.idToMint
  let poolIdMph = mintingPolicyHash poolIdMP
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol poolIdMph
  let idNft = Value.singleton poolIdCs poolID one
  configVal <- configAddressValidator
  configAdrUtxos <- getUtxos (scriptHashAddress $ validatorHash configVal)
  liquidityCs <- liftContractM "failed to hash mp" (mpsSymbol $ mintingPolicyHash liquidityMP)
  adr <- getWalletAddress >>= liftContractM "no wallet"
  utxos <- getUtxos adr
  let
    liq = BigInt.fromInt $ floor $ sqrt $ toNumber $ amt1 * amt2
    pool = PoolDatum
      { ac1
      , ac2
      , bal1: amt1
      , bal2: amt1
      , adminBal1: zero
      , adminBal2: zero
      , liquidity: fromMaybe liq attack.reportIssued
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
        (fromMaybe one attack.numberOfIdsToMint)
        <>
          ( if liq >= one then
              Constraints.mustMintValueWithRedeemer
                ( fromMaybe
                    (Redeemer $ List [ toData poolID, Constr zero [] ])
                    (attack.redeemer <#> (_ $ poolID))
                )
                ( fromMaybe
                    ( Value.singleton
                        liquidityCs
                        poolID
                        liq
                    )
                    (attack.actuallyMint <#> (_ $ liquidityCs) <#> (_ $ poolID))
                )
            else mempty
          )
        <>
          ( if attack.hasConfig then Constraints.mustReferenceOutput (fromMaybe configUtxo attack.badConfig)
            else mempty
          )
        <> (if attack.spendSeedTx then Constraints.mustSpendPubKeyOutput seed else mempty)
        <> Constraints.mustPayToScript
          (validatorHash poolAdrVal)
          (Datum $ toData pool)
          DatumInline
          ( (if attack.keepId then mempty else idNft)
              <> Value.singleton (fst ac1) (snd ac1) amt1
              <> Value.singleton (fst ac2) (snd ac2) amt2
          )
    )
    ( if attack.spendSeedTx then mempty
      else
        Constraints.mustNotSpendUtxoWithOutRef seed
    )
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) txid
  pure poolID



type AttackOptionsSwap =
  { grabNft :: Boolean
  , killThePool :: Boolean
  , mintLiquidity :: Maybe (Value /\ Redeemer)
  , underPay :: Maybe (BigInt /\ BigInt)
  , underReport :: Maybe (BigInt /\ BigInt)
  , underReportAdmin :: Maybe (BigInt /\ BigInt)
  }

regularSwap :: AttackOptionsSwap
regularSwap =
  { grabNft: false
  , killThePool: false
  , mintLiquidity: Nothing
  , underPay: Nothing
  , underReport: Nothing
  , underReportAdmin: Nothing
  }

swapLeftAttack :: AttackOptionsSwap -> Protocol -> PoolId -> Int -> Contract () Unit
swapLeftAttack attack protocol@(Protocol { poolIdMP, poolAdrVal, liquidityMP }) poolID amt = do
  (poolIn /\ poolOut) <- getPoolById protocol poolID
  poolIdCS <- liftContractM "hash was bad hex string" $ mpsSymbol $ mintingPolicyHash poolIdMP
  let idNft = Value.singleton poolIdCS poolID one
  let inPoolOutDatum = poolOut # unwrap # _.output # unwrap # _.datum
  PoolDatum inPoolDatum <-
    liftContractM "pool didn't parse" =<< fromData <$> case inPoolOutDatum of
      OutputDatum d -> pure $ unwrap d
      _ -> liftEffect $ throw "input pool had no datum"
  let
    newBal1 = inPoolDatum.bal1 + BigInt.fromInt amt
    invariant = inPoolDatum.bal1 * inPoolDatum.bal2
    newBal2' = invariant `ceilDiv` newBal1
    fee = ((inPoolDatum.bal2 - newBal2') * (BigInt.fromInt 3)) `ceilDiv` (BigInt.fromInt 1000)
    newBal2 = newBal2' + fee
    ac1 = inPoolDatum.ac1
    ac2 = inPoolDatum.ac2
    newAdminBal2 = inPoolDatum.adminBal2 + fee
    outPool = PoolDatum $
      inPoolDatum
        { bal1 = newBal1 - (fromMaybe zero (fst <$> attack.underReport))
        , bal2 = newBal2 - (fromMaybe zero (snd <$> attack.underReport))
        , adminBal1 = inPoolDatum.adminBal1 - (fromMaybe zero (fst <$> attack.underReportAdmin))
        , adminBal2 = newAdminBal2 - (fromMaybe zero (snd <$> attack.underReportAdmin))
        , live = not attack.killThePool
        }
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) =<<
    buildBalanceSignAndSubmitTx
      ( Lookups.unspentOutputs (Map.singleton poolIn poolOut)
          <> Lookups.validator poolAdrVal
          <> (if isJust attack.mintLiquidity then Lookups.mintingPolicy liquidityMP else mempty)
      )
      ( Constraints.mustSpendScriptOutput
          poolIn
          (Redeemer $ toData $ PoolRed poolID $ Swap fee)
          <>
            ( case attack.mintLiquidity of
                Nothing -> mempty
                Just (val /\ red) ->
                  Constraints.mustMintValueWithRedeemer
                    red
                    val
            )
          <> Constraints.mustPayToScript
            (validatorHash poolAdrVal)
            (Datum $ toData outPool)
            DatumInline
            ( (if attack.grabNft then mempty else idNft)
                <> Value.singleton (fst ac1) (snd ac1) (newBal1 + inPoolDatum.adminBal1 - (fromMaybe zero (fst <$> attack.underPay)))
                <> Value.singleton (fst ac2) (snd ac2) (newBal2 + newAdminBal2 - (fromMaybe zero (snd <$> attack.underPay)))
            )
      )

type AttackOptionsDeposit =
  { grabNft :: Boolean
  -- TODO reimplement these options
  , reportIssued :: Maybe (BigInt -> BigInt)
  , actuallyMint :: Maybe (CurrencySymbol -> Value)
  , redeemer :: Maybe Redeemer
  }

regularDeposit :: AttackOptionsDeposit
regularDeposit =
  { grabNft: false
  , reportIssued : Nothing
  , actuallyMint : Nothing
  , redeemer : Nothing
  }

depositLiquidityAttack :: AttackOptionsDeposit -> Protocol -> PoolId -> BigInt -> BigInt -> Contract () Unit
depositLiquidityAttack attack protocol@(Protocol { poolAdrVal, liquidityMP, poolIdMP }) poolID a b = do
  (poolIn /\ poolOut) <- getPoolById protocol poolID
  poolIdCS <- liftContractM "hash was bad hex string" $ mpsSymbol $ mintingPolicyHash poolIdMP
  let idNft = Value.singleton poolIdCS poolID one
  let inPoolOutDatum = poolOut # unwrap # _.output # unwrap # _.datum
  PoolDatum inPoolDatum <-
    liftContractM "pool didn't parse" =<< fromData <$> case inPoolOutDatum of
      OutputDatum d -> pure $ unwrap d
      _ -> liftEffect $ throw "input pool had no datum"
  let
    fee1 = (BigInt.fromInt 3 * a) `div` (BigInt.fromInt 1006)
    fee2 = (BigInt.fromInt 3 * b) `div` (BigInt.fromInt 1006)
    d1 = a - BigInt.fromInt 2 * fee1
    d2 = b - BigInt.fromInt 2 * fee2
    ac1 = inPoolDatum.ac1
    ac2 = inPoolDatum.ac2
    square x = x * x
    liqInv = (inPoolDatum.bal1 * inPoolDatum.bal2) / (square inPoolDatum.liquidity)
    poolInvOut = ((inPoolDatum.bal1 + d1) * (inPoolDatum.bal2 + d2))
    newLiq = BigInt.fromInt $ floor $ sqrt $ BigInt.toNumber poolInvOut / BigInt.toNumber liqInv
    mintLiq = newLiq - inPoolDatum.liquidity
    newBal1 = inPoolDatum.bal1 + d1 + fee1
    newBal2 = inPoolDatum.bal2 + d2 + fee2
    outPool = PoolDatum $
      inPoolDatum
        { bal1 = newBal1
        , bal2 = newBal2
        , adminBal1 = inPoolDatum.bal1 + fee1
        , adminBal2 = inPoolDatum.bal2 + fee2
        , liquidity = newLiq
        }
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) =<<
    buildBalanceSignAndSubmitTx
      ( Lookups.unspentOutputs (Map.singleton poolIn poolOut)
          <> Lookups.mintingPolicy liquidityMP
          <> Lookups.validator poolAdrVal
      )
      ( Constraints.mustSpendScriptOutput
          poolIn
          (Redeemer $ toData $ PoolRed poolID $ Liquidity fee1 fee2)
          <> Constraints.mustMintCurrencyWithRedeemer
            (mintingPolicyHash liquidityMP)
            (Redeemer $ List [ toData poolID, Constr (BigInt.fromInt 2) [] ])
            poolID
            mintLiq
          <> Constraints.mustPayToScript
            (validatorHash poolAdrVal)
            (Datum $ toData outPool)
            DatumInline
            ((if attack.grabNft then mempty else idNft)
              <> Value.singleton (fst ac1) (snd ac1) newBal1
              <> Value.singleton (fst ac2) (snd ac2) newBal2
            )
      )

