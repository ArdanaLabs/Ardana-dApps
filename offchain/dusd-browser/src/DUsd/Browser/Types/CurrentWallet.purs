module DUsd.Browser.Types.CurrentWallet where

import Effect.Exception (Error)

data State
  = NotAsked
  | Enabling
  | Failure Error