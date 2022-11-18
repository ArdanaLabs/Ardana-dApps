module DUsd.Browser.Types.NewVault where

import Effect.Exception (Error)

data State
  = NotAsked
  | Loading
  | Failure Error

data NewVault = Ada Int