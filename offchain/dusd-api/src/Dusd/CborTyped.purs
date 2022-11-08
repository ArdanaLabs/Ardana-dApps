module Dusd.CborTyped
  ( simpleNft
  , configAddressValidator
  ) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Address (PubKeyHash)
import Contract.Log (logError')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (PlutusData, toData)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Scripts (MintingPolicy(..), PlutusScript, Validator(..), applyArgs)
import Contract.Transaction (TransactionInput, plutusV2Script)
import Contract.Value (CurrencySymbol)
import Effect.Exception (throw)

{- This module should be the only place where CBOR is imported
- all of its exports should handle all of the validator's parameters
- this way there is only one module that needs to be checked
- for type errors between on and off chain code
-}

configAddressValidator :: PubKeyHash -> CurrencySymbol -> Contract () Validator
configAddressValidator pkh cs =
  do
    decodeCbor CBOR.configWithUpdates [ toData pkh, toData cs ]
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
    $ plutusV2Script
    <$> hexToByteArray cborHex
  applyArgs rawScript args >>= case _ of
    Left err -> do
      logError' $ "error in apply args:" <> show err
      liftEffect $ throw $ show err
    Right newScript -> pure newScript
