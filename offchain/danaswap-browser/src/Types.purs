module DanaSwapBrowser.Types where

import Contract.Prelude

import Data.Newtype (class Newtype)

newtype Pool = Pool
  { id :: Int
  , title :: String
  , subTitle :: String
  , baseApy :: Number
  , tvl :: Number
  , vol24H :: Number
  , vol7D :: Number
  , queueing :: Number
  }

derive instance newtypePool :: Newtype Pool _
derive instance eqPool :: Eq Pool