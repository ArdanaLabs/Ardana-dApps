module CborTyped (trivial,simpleNft) where

import Contract.Prelude

import CBOR as CBOR
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Scripts (MintingPolicy(..), Validator(..), applyArgsM)
import Contract.Transaction (TransactionInput, plutusV2Script)

{- This module should be the only place where CBOR is imported
 - all of its exports should handle all of the validator's parameters
 - this way there is only one module that needs to be checked
 - for type errors between on and off chain code
 -}

trivial :: Contract () Validator
trivial = decodeCbor CBOR.trivial

simpleNft :: TransactionInput -> Contract () MintingPolicy
simpleNft ref = do
  raw <- decodeCborMp CBOR.nft
  applyArgsM raw [toData ref]
    >>= liftContractM "failed to apply args"

-- These helpers should not be exported

decodeCbor :: String -> Contract () Validator
decodeCbor cborHex = liftContractM "failed to decode cbor"
  $ Validator <<< plutusV2Script <$> hexToByteArray cborHex

decodeCborMp :: String -> Contract () MintingPolicy
decodeCborMp cborHex = liftContractM "failed to decode cbor"
  $ MintingPolicy <<< plutusV2Script <$> hexToByteArray cborHex
