module DUsd.Browser.Types.CurrentWallet where

import Ctl.Internal.Wallet (WalletExtension)

import Effect.Exception (Error)

data State
  = NotAsked
  | Enabling
  | Success WalletExtension
  | Failure Error

