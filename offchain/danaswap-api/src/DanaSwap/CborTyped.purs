module DanaSwap.CborTyped
  ( poolAddressValidator
  , poolIdTokenMintingPolicy
  , liqudityTokenMintingPolicy
  , simpleNft
  , configAddressValidator
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson)
import CBOR as CBOR
import Contract.Log (logDebug', logError')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (PlutusData, toData)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Scripts (MintingPolicy(..), PlutusScript, Validator(..), applyArgs, applyArgsM)
import Contract.Transaction (TransactionInput, plutusV2Script)
import Contract.Value (CurrencySymbol)
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
  decodeCbor CBOR.trivial []
    <#> PlutusMintingPolicy

-- | MintingPolicy for the pool liquidity tokens parametized by the
-- currency symbol of the poolId tokens
liqudityTokenMintingPolicy :: CurrencySymbol -> Contract () MintingPolicy
liqudityTokenMintingPolicy poolId = do
  logDebug' "creating liquidity token minting policy"
  logDebug' $ "pool id:" <> show poolId
  decodeCbor CBOR.liqudityTokenMintingPolicy [ toData poolId ]
    <#> PlutusMintingPolicy

configAddressValidator :: Contract () Validator
configAddressValidator = decodeCbor CBOR.configScript []
  <#> Validator

-- | Simple NFT minting policy parametized by a transaction input
simpleNft :: TransactionInput -> Contract () MintingPolicy
simpleNft ref = do
  decodeCbor CBOR.nft [ toData ref ]
    <#> PlutusMintingPolicy

-- This helper should not be exported
decodeCbor :: String -> Array PlutusData -> Contract () PlutusScript
decodeCbor cborHex args = do
  rawScript <- liftContractM "failed to decode cbor"
    $ plutusV2Script <$> hexToByteArray cborHex
  applyArgs rawScript args >>= case _ of
    Left err -> do
      logError' $ "error in apply args:" <> show err
      liftEffect $ throw $ show err
    Right newScript -> pure newScript
