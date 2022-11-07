module Test.Main
  ( main
  ) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Dusd.Api (initProtocol)
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Test.Spec (describe, it, parallel, sequential)
import Ctl.Utils.Test (runWithMode, useRunnerSimple)
import Ctl.Utils.Test.Types (Mode(..))

main :: Effect Unit
main = launchAff_ $ do
  mode <- liftEffect $ lookupEnv "MODE" >>= case _ of
    Just "local" -> pure Local
    Just "testnet" -> do
      -- TODO add this back when we have an API key again
      -- topup "addr_test1qqevfdhu80jsmjhzkf8lkkv5rza9h6k0u6hmwwr0r7vyjt9j3f374a2all0hc6vzxa6v7sax7du2lk5fu5q592d5fhqswar4hc"
      pure Testnet
    Just e -> throw $ "expected local or testnet got: " <> e
    Nothing -> throw "expected MODE to be set"
  log "about to start"
  let maybePar = if mode == Local then parallel else sequential
  runWithMode mode $ do
    describe "Protocol Initialization" $ maybePar $ do
      -- @Todo implement https://github.com/ArdanaLabs/Danaswap/issues/16
      it "Init protocol doesn't error" $ useRunnerSimple $ do
        initProtocol
