module DUsd.Browser.Utils where

import Contract.Prelude

import Ctl.Internal.Wallet (WalletExtension(..))
import Effect.Exception (throw)

walletToString :: WalletExtension -> Effect String
walletToString = case _ of
  NamiWallet -> pure "nami"
  EternlWallet -> pure "eternl"
  _ -> throw "unknown wallet"

stringToWallet :: String -> Effect WalletExtension
stringToWallet = case _ of
  "nami" -> pure NamiWallet
  "eternl" -> pure EternlWallet
  _ -> throw "unknown wallet"