module Test.Main
  ( main
  ) where

import Contract.Prelude

import Contract.Address (scriptHashAddress)
import Contract.Monad (launchAff_)
import Contract.PlutusData (Datum(..), PlutusData(..), Redeemer(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Value (adaToken)
import Contract.Value as Value
import Ctl.Util (waitForTx)
import Ctl.Utils (buildBalanceSignAndSubmitTx)
import Ctl.Utils.Test (runWithMode, useRunnerSimple)
import Ctl.Utils.Test.Types (Mode(..))
import DanaSwap.Api (mintNft)
import DanaSwap.CborTyped (configAddressValidator)
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Test.Spec (describe, it, parallel, sequential)

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
    describe "DUSD config test" $ maybePar do
      it "can append" $ useRunnerSimple $ do
        cs <- mintNft
        configVal <- configAddressValidator
        firstConfig <- waitForTx (scriptHashAddress $ validatorHash configVal)
          =<< buildBalanceSignAndSubmitTx
          (Lookups.validator configVal)
          (Constraints.mustPayToScript
            (validatorHash configVal)
            (Datum $ List [ Constr zero []])
            DatumInline
            (Value.singleton cs adaToken one)
          )
        waitForTx (scriptHashAddress $ validatorHash configVal)
          =<< buildBalanceSignAndSubmitTx
          (Lookups.validator configVal)
          (Constraints.mustSpendScriptOutput
            firstConfig
            (Redeemer $ toData unit)
          <>
          Constraints.mustPayToScript
            (validatorHash configVal)
            (Datum $ List [ Constr one [] , Constr zero []])
            DatumInline
            (Value.singleton cs adaToken one)
          )
      it "can't edit" $ useRunnerSimple $ do
        cs <- mintNft
        configVal <- configAddressValidator
        firstConfig <- waitForTx (scriptHashAddress $ validatorHash configVal)
          =<< buildBalanceSignAndSubmitTx
          (Lookups.validator configVal)
          (Constraints.mustPayToScript
            (validatorHash configVal)
            (Datum $ List [ Constr zero []])
            DatumInline
            (Value.singleton cs adaToken one)
          )
        waitForTx (scriptHashAddress $ validatorHash configVal)
          =<< buildBalanceSignAndSubmitTx
          (Lookups.validator configVal)
          (Constraints.mustSpendScriptOutput
            firstConfig
            (Redeemer $ toData unit)
          <>
          Constraints.mustPayToScript
            (validatorHash configVal)
            (Datum $ List [ Constr one [] , Constr one []])
            DatumInline
            (Value.singleton cs adaToken one)
          )

          -- TODO test that it requires a signature
