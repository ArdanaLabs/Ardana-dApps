module Test.Attacks.Api
  ( openPoolAttack
  , regularOpen
  , depositLiquidityAttack
  , regularDeposit
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress, scriptHashAddress)
import Contract.BalanceTxConstraints (mustNotSpendUtxoWithOutRef) as Constraints
import Contract.Hashing (datumHash)
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), PlutusData(..), Redeemer(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (mintingPolicyHash, validatorHash)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints (mustMintCurrencyWithRedeemer, mustMintValueWithRedeemer, mustPayToScript, mustReferenceOutput, mustSpendPubKeyOutput, mustSpendScriptOutput) as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value, mkTokenName, mpsSymbol)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, buildBalanceSignAndSubmitTx', getUtxos, waitForTx)
import DanaSwap.Api (AssetClass, PoolDatum(..), PoolId, Protocol(..), getPoolById, seedTx)
import DanaSwap.CborTyped (configAddressValidator)
import Data.BigInt (BigInt, toNumber)
import Data.BigInt as BigInt
import Data.Int (floor)
import Data.Map as Map
import Math (sqrt)

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
  poolIdCS <- liftContractM "hash was bad hex string" $ mpsSymbol poolIdMph
  let idNft = Value.singleton poolIdCS poolID one
  configVal <- configAddressValidator
  configAdrUtxos <- getUtxos (scriptHashAddress $ validatorHash configVal)
  liquidityCS <- liftContractM "failed to hash mp" (mpsSymbol $ mintingPolicyHash liquidityMP)
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
      , issuedLiquidityTokens: fromMaybe liq attack.reportIssued
      , isLive: true
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
                        liquidityCS
                        poolID
                        liq
                    )
                    (attack.actuallyMint <#> (_ $ liquidityCS) <#> (_ $ poolID))
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

type AttackOptionsDeposit =
  { reportIssued :: Maybe (BigInt -> BigInt)
  , actuallyMint :: Maybe (CurrencySymbol -> Value)
  , redeemer :: Maybe Redeemer
  }

regularDeposit :: AttackOptionsDeposit
regularDeposit =
  { reportIssued: Nothing
  , actuallyMint: Nothing
  , redeemer: Nothing
  }

-- TODO ignores report issued for now
-- this won't matter till we have an actuall pool address validator script
depositLiquidityAttack :: AttackOptionsDeposit -> Protocol -> PoolId -> Contract () Unit
depositLiquidityAttack attack protocol@(Protocol { poolAdrVal, liquidityMP, poolIdMP }) poolID = do
  (poolIn /\ poolOut) <- getPoolById protocol poolID
  poolIdCS <- liftContractM "hash was bad hex string" $ mpsSymbol $ mintingPolicyHash poolIdMP
  liquidityCS <- liftContractM "failed to hash mp" (mpsSymbol $ mintingPolicyHash liquidityMP)
  let idNft = Value.singleton poolIdCS poolID one
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
                attack.redeemer
            )
            ( fromMaybe
                (Value.singleton liquidityCS poolID $ BigInt.fromInt 10)
                (attack.actuallyMint <#> (_ $ liquidityCS))
            )

          <> Constraints.mustPayToScript
            (validatorHash poolAdrVal)
            (Datum $ toData unit)
            DatumInline
            idNft
      )

