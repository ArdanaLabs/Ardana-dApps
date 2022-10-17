module Test.Main
  ( main
  ) where

import Contract.Prelude

import Api (initProtocol)
import Contract.Monad (launchAff_)
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import TestUtil (Mode(..), getEnvRunner, runEnvSpec, useRunnerSimple)

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
  envRunner <- getEnvRunner mode
  log "about to start"
  runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] $ (_ `runEnvSpec` envRunner) $ do
    describe "protocol init" $ do
      it "init protocol doesn't error" $ useRunnerSimple $ do
        initProtocol
