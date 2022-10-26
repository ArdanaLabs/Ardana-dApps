module DanaSwap.Api
  ( initProtocol
  , openPool
  , getAllPools
  , getPoolById
  , depositLiquidity
  -- Types
  , AssetClass
  , Protocol
  , PoolId
  -- Testing
  , PoolDatum(..)
  , mintNft
  , seedTx
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral, scriptHashAddress)
import Contract.Hashing (datumHash)
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), PlutusData(..), Redeemer(..), class ToData, toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, mintingPolicyHash, validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol, TokenName, adaToken, mkTokenName, mpsSymbol, scriptCurrencySymbol, symbols, valueOf)
import Contract.Value as Value
import Ctl.Util (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import DanaSwap.CborTyped (configAddressValidator, liqudityTokenMintingPolicy, poolAddressValidator, poolIdTokenMintingPolicy, simpleNft)
import Data.BigInt (BigInt, toNumber)
import Data.BigInt as BigInt
import Data.Int (floor)
import Data.List (head)
import Data.Map (Map, keys)
import Data.Map as Map
import Data.Set (toUnfoldable)
import Effect.Exception (throw)
import Math (sqrt)

type Protocol =
  { configUtxo :: TransactionInput
  , poolAdrVal :: Validator
  , liquidityMP :: MintingPolicy
  , poolIdMP :: MintingPolicy
  }

type PoolId = TokenName
type AssetClass = CurrencySymbol /\ TokenName

newtype PoolDatum =
  PoolDatum
    { ac1 :: AssetClass
    , ac2 :: AssetClass
    , bal1 :: BigInt
    , bal2 :: BigInt
    , adminBal1 :: BigInt
    , adminBal2 :: BigInt
    , liquidity :: BigInt
    , live :: Boolean
    }

instance ToData PoolDatum where
  toData (PoolDatum { ac1, ac2, bal1, bal2, adminBal1, adminBal2, liquidity, live }) = List
    [ toData ac1
    , toData ac2
    , toData bal1
    , toData bal2
    , toData adminBal1
    , toData adminBal2
    , toData liquidity
    , toData live
    ]

getAllPools :: Protocol -> Contract () (Map TransactionInput TransactionOutputWithRefScript)
getAllPools protocol@{ poolAdrVal } =
  getUtxos (scriptHashAddress $ validatorHash poolAdrVal)
    <#> Map.filter (hasNft protocol)

getPoolById :: Protocol -> PoolId -> Contract () (TransactionInput /\ TransactionOutputWithRefScript)
getPoolById protocol@{ poolIdMP } token = do
  pools <- getAllPools protocol
  cs <- liftContractM "Failed to get the currency symbol for the protocols mintingPolicy" $ mpsSymbol $ mintingPolicyHash poolIdMP
  let valid = Map.filter (\vault -> valueOf (unwrap (unwrap vault).output).amount cs token > BigInt.fromInt 0) pools
  case Map.toUnfoldableUnordered valid of
    [] -> liftEffect $ throw "no pools with that ID"
    [ vault ] -> pure vault
    _ -> liftEffect $ throw "more than one pool with the same ID, this is really bad"

hasNft :: Protocol -> TransactionOutputWithRefScript -> Boolean
hasNft { poolIdMP } out = case (mpsSymbol $ mintingPolicyHash poolIdMP) of
  Nothing -> false -- protocol was invalid
  Just cs -> cs `elem` (symbols $ (unwrap (unwrap out).output).amount)

-- TODO this is a placeholder implementation
depositLiquidity :: Protocol -> PoolId -> Contract () Unit
depositLiquidity protocol@{ poolAdrVal, liquidityMP, poolIdMP } poolID = do
  (poolIn /\ poolOut) <- getPoolById protocol poolID
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol $ mintingPolicyHash poolIdMP
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
            poolID
            (BigInt.fromInt 10)
          <> Constraints.mustPayToScript
            (validatorHash poolAdrVal)
            (Datum $ toData unit)
            DatumInline
            idNft
      )

-- TODO this is a placeholder implementation
-- The real implementation will also take more arguments
-- it should generate a usable pool for some tests
openPool :: Protocol -> AssetClass -> AssetClass -> BigInt -> BigInt -> Contract () PoolId
openPool { poolAdrVal, liquidityMP, poolIdMP, configUtxo } ac1 ac2 amt1 amt2 = do
  seed <- seedTx
  poolID <- liftContractM "failed to make poolID" $ datumHash (Datum (toData seed)) <#> unwrap >>= mkTokenName
  let poolIdMph = mintingPolicyHash poolIdMP
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol poolIdMph
  let idNft = Value.singleton poolIdCs poolID one
  configVal <- configAddressValidator
  configAdrUtxos <- getUtxos (scriptHashAddress $ validatorHash configVal)
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
      , liquidity: liq
      , live: true
      }
  txid <- buildBalanceSignAndSubmitTx
    ( Lookups.mintingPolicy poolIdMP
        <> Lookups.mintingPolicy liquidityMP
        <> Lookups.unspentOutputs configAdrUtxos
        <> Lookups.unspentOutputs utxos
    )
    ( Constraints.mustMintCurrencyWithRedeemer
        poolIdMph
        (Redeemer $ toData seed)
        poolID
        one
        <>
          ( if liq >= one then
              Constraints.mustMintCurrencyWithRedeemer
                (mintingPolicyHash liquidityMP)
                (Redeemer $ List [ toData poolID, Constr zero [] ])
                poolID
                liq
            else mempty
          )
        <> Constraints.mustReferenceOutput configUtxo
        <> Constraints.mustSpendPubKeyOutput seed
        <> Constraints.mustPayToScript
          (validatorHash poolAdrVal)
          (Datum $ toData $ pool)
          DatumInline
          ( idNft
              <> Value.singleton (fst ac1) (snd ac1) amt1
              <> Value.singleton (fst ac2) (snd ac2) amt2
          )
    )
  void $ waitForTx (scriptHashAddress $ validatorHash poolAdrVal) txid
  pure poolID

initProtocol :: Contract () Protocol
initProtocol = do
  logDebug' "starting protocol init"
  nftCs <- mintNft
  logDebug' "nft minted"
  logDebug' $ "currency symbol:" <> show nftCs
  poolIdMP <- poolIdTokenMintingPolicy nftCs
  poolIdCS <- liftContractM "invalid hex string from mintingPolicyHash"
    $ mpsSymbol
    $ mintingPolicyHash poolIdMP
  liquidityMP <- liqudityTokenMintingPolicy poolIdCS
  liquidityCS <- liftContractM "invalid hex string from mintingPolicyHash"
    $ mpsSymbol
    $ mintingPolicyHash liquidityMP
  poolAdrVal <- poolAddressValidator poolIdCS liquidityCS
  let poolVH = validatorHash poolAdrVal
  let poolAdr = scriptHashAddress poolVH
  configAdrVal <- configAddressValidator
  logDebug' "about to submit config utxo"
  txid <- buildBalanceSignAndSubmitTx
    (mempty)
    ( Constraints.mustPayToScript
        (validatorHash configAdrVal)
        (Datum $ List [ toData poolAdr, toData liquidityCS ])
        -- This could have a data type with a ToData instance
        -- but it only gets used once so it seems unnesecary
        DatumInline
        (Value.singleton nftCs adaToken one)
    )
  logDebug' "config utxo submitted, waiting for confirmation"
  configUtxo <- waitForTx (scriptHashAddress $ validatorHash configAdrVal) txid
  logDebug' "protocol init complete"
  pure
    { configUtxo
    , poolAdrVal
    , liquidityMP
    , poolIdMP
    }

-- | Mints an nft where the txid is a parameter of the contract and returns the currency symbol
mintNft :: Contract () CurrencySymbol
mintNft = do
  logInfo' "starting mint"
  txOut <- seedTx
  adr <- liftContractM "no wallet" =<< getWalletAddress
  utxos <- getUtxos adr
  logDebug' $ "seed tx id was:" <> show txOut
  nftPolicy <- simpleNft txOut
  cs <- liftContractM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
  logInfo' $ "NFT cs: " <> show cs
  let
    lookups :: Lookups.ScriptLookups PlutusData
    lookups = Lookups.mintingPolicy nftPolicy
      <> Lookups.unspentOutputs utxos

    constraints :: TxConstraints Unit Unit
    constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
      <> Constraints.mustSpendPubKeyOutput txOut
  logDebug' "about to submit"
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  logDebug' "submitted"
  _ <- waitForTx adr txId
  pure $ cs

-- | Selects a utxo owned by the current wallet usefull for minting nfts
seedTx :: Contract () TransactionInput
seedTx = do
  adr <- liftContractM "no wallet" =<< getWalletAddress
  utxos <- getUtxos adr
  logInfo' $ show adr
  logInfo' $ "utxos: " <> show utxos
  col <- fromMaybe [] <$> getWalletCollateral
  logInfo' $ "col: " <> show col
  let colIns = (unwrap >>> _.input) <$> col
  logInfo' $ "colIns: " <> show colIns
  let nonColateralUtxOs = Map.filterKeys (\utxo -> utxo `notElem` colIns) utxos
  logInfo' $ "nonColUtxos: " <> show nonColateralUtxOs
  sending <- case head $ toUnfoldable $ keys nonColateralUtxOs of
    Just sending -> pure sending
    Nothing -> do
      logInfo' "all utxos were collateral using collateral utxo"
      liftContractM "no utxos at all" $ head $ toUnfoldable $ keys utxos
  logInfo' $ "sending: " <> show sending
  out <- liftContractM "no output" =<< getUtxo sending
  logInfo' $ "out: " <> show out
  pure sending
