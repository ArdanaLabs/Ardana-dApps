module DUsd.Types
  ( Protocol(..)
  , Params(..)
  , UtxoId(..)
  , AssetClass
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson')
import Contract.PlutusData (class FromData, class ToData, PlutusData(..), fromData, toData)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Contract.Scripts (Validator)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Value (CurrencySymbol, TokenName)
import Data.BigInt (BigInt)
import Contract.Plutarch.Types (PRational)
import Data.UInt as UInt

newtype Protocol = Protocol
  { configUtxo :: UtxoId
  , params :: UtxoId
  , priceOracle :: UtxoId
  }

derive newtype instance EncodeAeson Protocol
derive newtype instance DecodeAeson Protocol

-- | A common patern in protocol design
-- is to identify a utxo by an nft
-- and an address the optional
-- guess can improve
-- lookup performance if
-- the utxo hasn't changed
newtype UtxoId = UtxoId
  { nft :: AssetClass
  , script :: Validator
  , guess :: Maybe TransactionInput
  }

type AssetClass = CurrencySymbol /\ TokenName

newtype Params = Params
  { debtFloor :: BigInt
  , liquidationDiscount :: PRational
  , liquidationFee :: BigInt
  , liquidationRatio :: PRational
  }

derive newtype instance EncodeAeson Params
derive newtype instance DecodeAeson Params

instance ToData Params where
  toData (Params { debtFloor, liquidationDiscount, liquidationFee, liquidationRatio }) = List
    [ toData debtFloor
    , toData liquidationDiscount
    , toData liquidationFee
    , toData liquidationRatio
    ]

instance FromData Params where
  fromData =
    case _ of
      List [ df, ld, lf, lr ] -> do
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

instance EncodeAeson UtxoId where
  encodeAeson' = jsonifyUtxoId >>> encodeAeson'

instance DecodeAeson UtxoId where
  decodeAeson = decodeAeson >>> map unJsonifyUtxoId

type UtxoIdJson =
  { nft :: AssetClass
  , script :: Validator
  , guess :: Maybe TransactionInputJson
  }

jsonifyUtxoId :: UtxoId -> UtxoIdJson
jsonifyUtxoId (UtxoId { nft, script, guess }) = { nft, script, guess: jsonifyTxIn <$> guess }

unJsonifyUtxoId :: UtxoIdJson -> UtxoId
unJsonifyUtxoId { nft, script, guess } = UtxoId { nft, script, guess: unJsonifyTxIn <$> guess }

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

