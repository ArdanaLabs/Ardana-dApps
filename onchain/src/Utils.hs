module Utils (
  validatorToHexString,
  trivialHexString,
  closedTermToHexString,
  Cbor (..),
  toPureScript,
) where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.List (intercalate)
import Data.Word (Word8)
import Numeric
import Plutarch (Config, compile)

-- please forgive the (..) I don't want to change the import every time I change the tracing mode
import Plutarch.Api.V1
import Plutarch.Prelude
import PlutusLedgerApi.V1.Scripts (Script, Validator)
import System.Exit (die)

{- | This function turns a validator into a hex string usable with CTL.
 It works by serialising  the validator to a cbor byte string,
 unpacking the bytestring into a [Word8],
 and then using showHex from numeric (with a helper to pad 0s as needed)
 to show each byte in hexidecimal.
-}
validatorToHexString :: Validator -> String
validatorToHexString v = concatMap byteToHex $ BSL.unpack $ serialise (v :: Validator)

closedTermToHexString :: forall (p :: PType). ClosedTerm p -> Config -> Maybe String
closedTermToHexString t config = do
  case compile config t of
    Left _ -> Nothing
    Right script -> Just $ concatMap byteToHex $ BSL.unpack $ serialise (script :: Script)

byteToHex :: Word8 -> String
byteToHex b = padToLen 2 '0' (showHex b "")

padToLen :: Int -> Char -> String -> String
padToLen len c w = replicate (len - length w) c <> w

trivialHexString :: Config -> Maybe String
trivialHexString = closedTermToHexString trivialValidator

trivialValidator :: ClosedTerm PValidator
trivialValidator = plam $ \_ _ _ -> popaque $ pcon PUnit

-- | Represents a declaration of a constant cbor string in purescript
data Cbor = Cbor {name :: String, cbor :: Config -> Maybe String}

-- | Turns a list of Cbor objects into the text of a purescript module which declares them all
toPureScript :: Config -> [Cbor] -> IO String
toPureScript conf cs =
  (("module CBOR (\n  " <> intercalate ",\n  " (name <$> cs) <> "\n) where\n\n") <>)
    . intercalate "\n\n"
    <$> mapM (toDec conf) cs

toDec :: Config -> Cbor -> IO String
toDec conf c = case cbor c conf of
  Nothing -> die $ name c <> " didn't compile"
  Just hex ->
    pure $
      name c <> " :: String\n"
        <> name c
        <> " = \""
        <> hex
        <> "\""
