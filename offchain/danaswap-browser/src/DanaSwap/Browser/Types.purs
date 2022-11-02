module DanaSwap.Browser.Types where

import Contract.Prelude

import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)

newtype Asset = Asset
  { name :: String
  , value :: BigInt
  }

derive instance newtypeAsset :: Newtype Asset _
derive instance eqAsset :: Eq Asset

newtype Pool = Pool
  { assetA :: Asset
  , assetB :: Asset
  , fee :: Int
  }

derive instance newtypePool :: Newtype Pool _
derive instance eqPool :: Eq Pool
