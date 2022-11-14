module Dusd.Browser.Types where

import Data.Newtype (class Newtype)

newtype Asset = Asset String

derive instance Newtype Asset _

type Vault =
  { asset :: Asset
  , assetType :: String
  , dUsdAvailable :: Number
  , stablilityFee :: Number
  , minCollRatio :: Number
  }
