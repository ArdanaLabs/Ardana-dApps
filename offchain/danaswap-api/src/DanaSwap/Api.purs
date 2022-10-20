module DanaSwap.Api
  ( initProtocol
  , openPool
  , getAllPools
  , getPoolById
  , depositLiquidity
  -- Types
  , Protocol
  , PoolId
  -- Testing
  , mintNft
  , seedTx
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral, scriptHashAddress)
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), PlutusData(..), Redeemer(..), toData)
import Contract.Prim.ByteArray (hexToByteArray)
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
import Data.BigInt as BigInt
import Data.List (head)
import Data.Map (Map, keys)
import Data.Map as Map
import Data.Set (toUnfoldable)
import Effect.Exception (throw)

type Protocol =
  { configUtxo :: TransactionInput
  , poolVal :: Validator
  , liquidityMP :: MintingPolicy
  , poolIdMP :: MintingPolicy
  }

-- TODO should this be a newtype?
type PoolId = TokenName

getAllPools :: Protocol -> Contract () (Map TransactionInput TransactionOutputWithRefScript)
getAllPools protocol@{ poolVal } =
  getUtxos (scriptHashAddress $ validatorHash poolVal)
    <#> Map.filter (hasNft protocol)

getPoolById :: Protocol -> PoolId -> Contract () (TransactionInput /\ TransactionOutputWithRefScript)
getPoolById protocol@{ poolIdMP } token = do
  pools <- getAllPools protocol
  cs <- liftContractM "invalid protocol" $ mpsSymbol $ mintingPolicyHash poolIdMP
  let valid = Map.filter (\vault -> valueOf (unwrap (unwrap vault).output).amount cs token > BigInt.fromInt 0) pools
  case Map.toUnfoldable valid of
    [] -> liftEffect $ throw "no pools with that ID"
    [ vault ] -> pure vault
    _ -> liftEffect $ throw "more than one pool with the same ID, this is really bad"

hasNft :: Protocol -> TransactionOutputWithRefScript -> Boolean
hasNft { poolIdMP } out = case (mpsSymbol $ mintingPolicyHash poolIdMP) of
  Nothing -> false -- protocol was invalid
  Just cs -> cs `elem` (symbols $ (unwrap (unwrap out).output).amount)

-- TODO this is a placeholder implementation
depositLiquidity :: Protocol -> PoolId -> Contract () Unit
depositLiquidity protocol@{ poolVal, liquidityMP, poolIdMP } poolID = do
  (poolIn /\ poolOut) <- getPoolById protocol poolID
  poolIdCs <- liftContractM "hash was bad hex string" $ mpsSymbol $ mintingPolicyHash poolIdMP
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
          <> Constraints.mustMintCurrencyWithRedeemer
            (mintingPolicyHash liquidityMP)
            (Redeemer $ List [ toData poolID, Constr one [] ])
            poolID
            (BigInt.fromInt 10)
          <> Constraints.mustPayToScript
            (validatorHash poolVal)
            (Datum $ toData unit)
            DatumInline
            idNft
      )

-- TODO this is a placeholder implementation
-- The real implementation will also take more arguments
-- it should generate a usable pool for some tests
openPool :: Protocol -> Contract () PoolId
openPool { poolVal, liquidityMP, poolIdMP, configUtxo } = do
  poolID <- liftContractM "failed to make token name" $ mkTokenName =<< hexToByteArray "aaaa"
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
          (Redeemer $ List [ toData poolID, Constr zero [] ])
          poolID
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

initProtocol :: Contract () Protocol
initProtocol = do
  logDebug' "starting protocol init"
  nftCs <- mintNft
  logDebug' "nft minted"
  logDebug' $ "cs:" <> show nftCs
  poolIdMP <- poolIdTokenMintingPolicy nftCs
  poolIdCS <- liftContractM "invalid hex string from mintingPolicyHash"
    $ mpsSymbol
    $ mintingPolicyHash poolIdMP
  liquidityMP <- liqudityTokenMintingPolicy poolIdCS
  liquidityCS <- liftContractM "invalid hex string from mintingPolicyHash"
    $ mpsSymbol
    $ mintingPolicyHash liquidityMP
  poolVal <- poolAddressValidator poolIdCS liquidityCS
  let poolVH = validatorHash poolVal
  let poolAdr = scriptHashAddress poolVH
  configAdrVal <- configAddressValidator
  logDebug' "about to submit config utxo"
  txid <- buildBalanceSignAndSubmitTx
    (mempty)
    ( Constraints.mustPayToScript
        (validatorHash configAdrVal)
        (Datum $ Constr zero [ toData poolAdr, toData liquidityCS ])
        -- This could have a data type with a ToData instance
        -- but it only gets used once so it seems unnesecary
        DatumInline
        (Value.singleton nftCs adaToken one)
    )
  logDebug' "config utxo submited waiting for confirmation"
  configUtxo <- waitForTx (scriptHashAddress $ validatorHash configAdrVal) txid
  logDebug' "protocol init complete"
  pure
    { configUtxo
    , poolVal
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
  logDebug' "submited"
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
