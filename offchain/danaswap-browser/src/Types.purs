module DanaSwapBrowser.Types where

import Contract.Prelude

import Data.BigInt (BigInt, toString)
import Data.Newtype (class Newtype)

newtype Asset = Asset
  { name :: String
  , value :: BigInt
  }

derive instance newtypeAsset :: Newtype Asset _
derive instance eqAsset :: Eq Asset
instance showAsset :: Show Asset where
  show (Asset { name, value }) = (toString value) <> " " <> name

newtype Pool = Pool
  { assetA :: Asset
  , assetB :: Asset
  , fee :: Int
  }

derive instance newtypePool :: Newtype Pool _
derive instance eqPool :: Eq Pool
instance showPool :: Show Pool where
  show (Pool { assetA: (Asset a), assetB: (Asset b) }) = a.name <> " + " <> b.name