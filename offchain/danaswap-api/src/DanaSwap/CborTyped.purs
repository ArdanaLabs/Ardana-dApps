module DanaSwap.CborTyped
  ( poolAddressValidator
  , poolIdTokenMintingPolicy
  , liquidityTokenMintingPolicy
  , simpleNft
  , configAddressValidator
  -- Test exports
  , testToken
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Log (logDebug', logError')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (PlutusData, toData)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Scripts (MintingPolicy(..), PlutusScript, Validator(..), applyArgs)
import Contract.Transaction (TransactionInput, plutusV2Script)
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import Effect.Exception (throw)

{- This module should be the only place where CBOR is imported
- all of its exports should handle all of the validator's parameters
- this way there is only one module that needs to be checked
- for type errors between on and off chain code
-}

-- | Placeholder
poolAddressValidator :: CurrencySymbol -> CurrencySymbol -> Contract () Validator
poolAddressValidator poolIdToken liquidityToken = do
  logDebug' "creating pool address validator"
  logDebug' $ "pool id:" <> show poolIdToken
  logDebug' $ "liquidity token:" <> show liquidityToken
  decodeCbor CBOR.trivial []
    <#> Validator

-- | Placeholder
poolIdTokenMintingPolicy :: CurrencySymbol -> Contract () MintingPolicy
poolIdTokenMintingPolicy configUtxoNftCS = do
  logDebug' "creating pool id token minting policy"
  logDebug' $ "nft cs:" <> show configUtxoNftCS
  decodeCbor CBOR.poolIdTokenMintingPolicy [ toData configUtxoNftCS ]
    <#> PlutusMintingPolicy

-- | MintingPolicy for the pool liquidity tokens parametized by the
-- currency symbol of the poolId tokens
liquidityTokenMintingPolicy :: CurrencySymbol -> Contract () MintingPolicy
liquidityTokenMintingPolicy poolId = do
  logDebug' "creating liquidity token minting policy"
  logDebug' $ "pool id:" <> show poolId
  decodeCbor CBOR.liquidityTokenMintingPolicy [ toData poolId ]
    <#> PlutusMintingPolicy

configAddressValidator :: Contract () Validator
configAddressValidator = decodeCbor CBOR.configScript []
  <#> Validator

-- | Simple always accepts mintingPolicy
-- the integer is ignored by the script
-- but is coded in the script so it affects the
-- script hash so each value gets a new currency symbols
testToken :: BigInt -> Contract () MintingPolicy
testToken n = do
  logDebug' "Creating test token"
  logDebug' $ "index:" <> show n
  decodeCbor CBOR.trivial [ toData n ]
    <#> PlutusMintingPolicy

-- | Simple NFT minting policy parametized by a transaction input
simpleNft :: TransactionInput -> Contract () MintingPolicy
simpleNft ref = do
  decodeCbor CBOR.nft [ toData ref ]
    <#> PlutusMintingPolicy

-- This helper should not be exported
decodeCbor :: String -> Array PlutusData -> Contract () PlutusScript
decodeCbor cborHex args = do
  rawScript <- liftContractM "failed to decode cbor"
    $ plutusV2Script
    <$> hexToByteArray cborHex
  applyArgs rawScript args >>= case _ of
    Left err -> do
      logError' $ "error in apply args:" <> show err
      liftEffect $ throw $ show err
    Right newScript -> pure newScript
