module Test.Main
  ( main
  ) where

import Contract.Prelude

import Contract.Monad (launchAff_, liftContractM)
import Contract.Plutarch.Types ((%))
import Contract.PlutusData (PlutusData(..))
import Ctl.Utils.Test (expectScriptError, runWithMode, useRunnerSimple)
import Ctl.Utils.Test.Types (Mode(..))
import DUsd.Api (Params(..), initParams, initProtocol, mintNft, updateConfig)
import DUsd.Config (initConfigWith)
import DUsd.Params (updateDebtFloor)
import Data.BigInt as BigInt
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Test.Attacks.Api (updateConfigAttack, defUpdate)
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
    describe "Protocol" $ do
      it "Init protocol doesn't error" $ useRunnerSimple $ do
        threeHalves <- liftContractM "2==0" $ 3 % 2
        fiveThirds <- liftContractM "3==0" $ 5 % 3
        initProtocol $
          Params
            { debtFloor: BigInt.fromInt 1
            , liquidationDiscount: threeHalves
            , liquidationFee: BigInt.fromInt 3
            , liquidationRatio: fiveThirds
            }

    describe "Params utxo" $ maybePar $ do
      it "Init prams doesn't error" $ useRunnerSimple $ do
        threeHalves <- liftContractM "2==0" $ 3 % 2
        fiveThirds <- liftContractM "3==0" $ 5 % 3
        initParams $
          Params
            { debtFloor: BigInt.fromInt 1
            , liquidationDiscount: threeHalves
            , liquidationFee: BigInt.fromInt 3
            , liquidationRatio: fiveThirds
            }
      it "Update params doesn't error" $ useRunnerSimple $ do
        threeHalves <- liftContractM "2==0" $ 3 % 2
        fiveThirds <- liftContractM "3==0" $ 5 % 3
        paramsId <- initParams $
          Params
            { debtFloor: BigInt.fromInt 1
            , liquidationDiscount: threeHalves
            , liquidationFee: BigInt.fromInt 3
            , liquidationRatio: fiveThirds
            }
        updateDebtFloor paramsId (BigInt.fromInt 2)

    describe "Config utxo" $ maybePar $ do
      -- @Todo implement https://github.com/ArdanaLabs/Danaswap/issues/16
      it "Init config doesn't error" $ useRunnerSimple $ do
        cs <- mintNft
        initConfigWith cs (Constr zero [])
      it "Update config doesn't error" $ useRunnerSimple $ do
        cs <- mintNft
        configUtxo <- initConfigWith cs (Constr zero [])
        updateConfig (Constr one []) configUtxo
      it "Update with edit fails validation" $ useRunnerSimple $ do
        cs <- mintNft
        configUtxo <- initConfigWith cs (Constr zero [])
        expectScriptError $
            (defUpdate { overwriteDatum = Just $ List [ Constr zero [], Constr one [] ] })
            (Constr one [])
            configUtxo
      it "Update without signature fails validation" $ useRunnerSimple $ do
        cs <- mintNft
        configUtxo <- initConfigWith cs (Constr zero [])
        expectScriptError $
          updateConfigAttack
            (defUpdate { noSignature = true })
            (Constr one [])
            configUtxo

