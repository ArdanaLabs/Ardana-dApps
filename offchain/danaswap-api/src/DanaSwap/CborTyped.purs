module DanaSwap.CborTyped
  ( poolAddressValidator
  , poolIdTokenMintingPolicy
  , liqudityTokenMintingPolicy
  , simpleNft
  , configAddressValidator
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Log (logDebug')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Scripts (MintingPolicy(..), PlutusScript, Validator(..), applyArgsM)
import Contract.Transaction (TransactionInput, plutusV2Script)
import Contract.Value (CurrencySymbol)

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
  Validator <$> decodeCbor CBOR.trivial

-- | Placeholder
poolIdTokenMintingPolicy :: CurrencySymbol -> Contract () MintingPolicy
poolIdTokenMintingPolicy configUtxoNftCS = do
  logDebug' "creating pool id token minting policy"
  logDebug' $ "nft cs:" <> show configUtxoNftCS
  PlutusMintingPolicy <$> decodeCbor CBOR.trivial

-- | Placeholder
liqudityTokenMintingPolicy :: CurrencySymbol -> Contract () MintingPolicy
liqudityTokenMintingPolicy poolId = do
  logDebug' "creating liquidity token minting policy"
  logDebug' $ "pool id:" <> show poolId
  PlutusMintingPolicy <$> decodeCbor CBOR.trivial

configAddressValidator :: Contract () Validator
configAddressValidator = Validator <$> decodeCbor CBOR.configScript

-- | Simple NFT minting policy parametized by a transaction input
simpleNft :: TransactionInput -> Contract () MintingPolicy
simpleNft ref = do
  raw <- decodeCbor CBOR.nft
  applyArgsM raw [ toData ref ]
    >>= liftContractM "failed to apply args"
    <#> PlutusMintingPolicy

-- These helpers should not be exported

decodeCbor :: String -> Contract () PlutusScript
decodeCbor cborHex = liftContractM "failed to decode cbor"
  $ plutusV2Script
  <$> hexToByteArray cborHex
