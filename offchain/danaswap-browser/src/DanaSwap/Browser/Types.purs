module DanaSwap.Browser.Types where

type Vault =
  { asset :: String
  , assetType :: String
  , dUsdAvailable :: Number
  , stablilityFee :: Number
  , minCollRatio :: Number
  }

