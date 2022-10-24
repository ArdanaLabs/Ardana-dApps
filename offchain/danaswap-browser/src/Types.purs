module DanaSwapBrowser.Types where

import Contract.Prelude

import Data.BigInt (BigInt)
import Data.Newtype (class Newtype)

newtype Pool = Pool
  { id :: Int
  , title :: String
  , subTitle :: String
  , tvl :: Number
  , tradingFeesApr :: Number
  , lpFee :: Number
  , totalLps :: Maybe BigInt
  }

derive instance newtypePool :: Newtype Pool _
derive instance eqPool :: Eq Pool