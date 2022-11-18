module DUsd.Browser.Types.AvailableWallets where

import Ctl.Internal.Wallet (WalletExtension)

import Effect.Exception (Error)

data State
  = Loading
  | Success (Array WalletExtension)
  | Failure Error