module DUsd.Browser.Types.Vault where

import Prelude

import Data.Newtype (class Newtype)

data Collateral = Ada Number

derive instance Eq Collateral

instance Show Collateral where
  show (Ada amount) = show amount <> " ADA"

data Debt = DUsd Number

derive instance Eq Debt

instance Show Debt where
  show (DUsd amount) = show amount <> " dUSD"

newtype LiquidationPrice = LiquidationPrice Number

derive instance Newtype LiquidationPrice _

type Vault =
  { debt :: Debt
  , collateral :: Collateral
  , liquidationPrice :: LiquidationPrice
  }

collateralType :: Vault -> String
collateralType v = case v.collateral of
  Ada _ -> "ADA"