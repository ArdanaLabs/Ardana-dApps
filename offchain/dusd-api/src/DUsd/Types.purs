module DUsd.Types
  ( Protocol(..)
  , Params(..)
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson')
import Contract.Numeric.Rational (Rational)
import Contract.PlutusData (class FromData, class ToData, PlutusData(..), fromData, toData)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import Data.UInt as UInt

newtype Protocol = Protocol
  { datum :: PlutusData
  , utxo :: TransactionInput
  , nftCs :: CurrencySymbol
  , configVal :: Validator
  }

newtype Params = Params
  { debtFloor :: BigInt
  , liquidationDiscount :: Rational
  , liquidationFee :: BigInt
  , liquidationRatio :: Rational
  }

instance ToData Params where
  toData (Params {debtFloor,liquidationDiscount,liquidationFee,liquidationRatio})
    = List [ toData debtFloor
           , toData liquidationDiscount
           , toData liquidationFee
           , toData liquidationRatio
           ]

instance FromData Params where
  fromData =
    case _ of
      List [ df , ld , lf , lr] -> do
        debtFloor <- fromData df
        liquidationDiscount <- fromData ld
        liquidationFee <- fromData lf
        liquidationRatio <- fromData lr
        pure $ Params
          { debtFloor
          , liquidationDiscount
          , liquidationFee
          , liquidationRatio
          }
      _ -> Nothing

derive instance Newtype Protocol _

instance EncodeAeson Protocol where
  encodeAeson' = jsonifyProtocol >>> encodeAeson'

instance DecodeAeson Protocol where
  decodeAeson = (unJsonifyProtocol <$> _) <<< decodeAeson

type ProtocolJson = { datum :: PlutusData, utxo :: TransactionInputJson, nftCs :: CurrencySymbol, configVal :: Validator }

jsonifyProtocol :: Protocol -> ProtocolJson
jsonifyProtocol (Protocol { datum, utxo, nftCs, configVal }) = { datum, utxo: jsonifyTxIn utxo, nftCs, configVal }

unJsonifyProtocol :: ProtocolJson -> Protocol
unJsonifyProtocol { datum, utxo, nftCs, configVal } = Protocol { datum, utxo: unJsonifyTxIn utxo, nftCs, configVal }

-- TODO maybe we should just upstream an AESON instance for this
type TransactionInputJson = { index :: Int, transactionId :: String }

jsonifyTxIn :: TransactionInput -> TransactionInputJson
jsonifyTxIn (TransactionInput { index, transactionId }) =
  { index: UInt.toInt index
  , transactionId: byteArrayToHex $ unwrap transactionId
  }

unJsonifyTxIn :: TransactionInputJson -> TransactionInput
unJsonifyTxIn { index, transactionId } = TransactionInput
  { index: UInt.fromInt index
  , transactionId: TransactionHash $ hexToByteArrayUnsafe transactionId
  }

