module DUsd.Browser.Types.Vaults where

import DUsd.Browser.Types.Vault (Vault)
import Effect.Exception (Error)

data State
  = Loading
  | Success (Array Vault)
  | Failure Error