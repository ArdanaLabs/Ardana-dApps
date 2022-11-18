module DUsd.Browser.Capability.CardanoApi where

import Contract.Prelude

import Ctl.Internal.Wallet (WalletExtension)
import Data.Either (Either)
import Effect.Aff (Error)
import Halogen (HalogenM, lift)

class Monad m <= CardanoApi m where
  availableWallets :: m (Array WalletExtension)
  enable :: WalletExtension -> m (Either Error Unit)

instance cardanoApiHalogenM :: CardanoApi m => CardanoApi (HalogenM st act slots msg m) where
  availableWallets = lift availableWallets
  enable = lift <<< enable