module DUsd.Browser.Capability.DUsdApi where

import Prelude

import DUsd.Browser.Types.NewVault (NewVault)
import DUsd.Browser.Types.Vault (Vault)
import Data.Either (Either)
import Effect.Aff (Error)
import Halogen (HalogenM, lift)

class Monad m <= DUsdApi m where
  loadVaults :: m (Either Error (Array Vault))
  createVault :: NewVault -> m (Either Error Vault)

instance dusdApiHalogenM :: DUsdApi m => DUsdApi (HalogenM st act slots msg m) where
  loadVaults = lift loadVaults
  createVault = lift <<< createVault