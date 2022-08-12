module Test.Main
  ( main
  ) where

import CmdUtils(fails,failsSaying,passesSaying,spawnAff)
import Contract.Prelude
import Contract.Test.Plutip (PlutipConfig, InitialUTxO, runPlutipContract, withPlutipContractEnv, runContractInEnv)
import Data.String(trim)
import Effect.Aff(launchAff_)
import Node.Process(lookupEnv)
import Test.Spec(it,describe,Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec',defaultConfig)
import Test.Wallet(makeWallet,rmWallet)

import Data.BigInt as BigInt
import Data.UInt as UInt

main :: Effect Unit
main = do
  fixturesDir <- fromMaybe "./fixtures" <$> lookupEnv "TEST_RESOURCES"
  let initialAdaAmount = BigInt.fromInt 20_000_000
      initialValue = 20
      incParam = 200
  let jsonDir = fixturesDir <> "/jsons/"
  let plutipWalletDir = "./" -- TODO get a writeable dir from nix
  launchAff_ $ withPlutipContractEnv config [ initialAdaAmount ] \env alice -> do
    let plutipPorts = "--ctl-port 8083 --ogmios-port 1338 --odc-port 10000 "
    let cli = "hello-world-cli " <> plutipPorts
    conf <- (\w -> " " <> w <> " ") <$> makeWallet plutipWalletDir "plutip" alice
    let badConf = jsonDir <> "badWalletCfg.json "
    let state = " script.clistate "
    let badState = jsonDir <> "badState.json "
    runSpec' defaultConfig{timeout=Nothing} [ consoleReporter ] $ do
      describe "shell sanity checks" do
        it "ls err"
          $ failsSaying "ls bad_path"
            "No such file"
        it "ls pass"
          $ passesSaying "ls -a"
            "."
      describe "help page" do
        it "knows its own name" -- it used to call itself `purs-nix run`
          $ passesSaying
            (cli <> "--help")
            "Usage: hello-world-cli"
      describe "lock" do
        it "fails on no conf"
          $ failsSaying
            (cli <> "-s" <> state <> "lock -i 0 -p 1")
            "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ failsSaying
            (cli <> "-c" <> conf <> "lock -i 0 -p 1")
            "Missing: (-s|--state-file STATE_FILE)"
        it "fails on no inc"
          $ fails $ cli <> "-c" <> conf <> "-s" <> state <> "lock -p 1"
        it "fails on no param"
          $ fails $ cli <> "-c" <> conf <> "-s" <> state <> "lock -i 0"
        it "fails on bad conf"
          $ fails $ cli <> "-c" <> badConf <> "-s" <> state <> "lock -i 0 -p 1"
        -- There's no hard reason this couldn't be made to work without the runtime
        -- but it happens to look up the datum before noticing the state shouldn't exist
        it "fails when state exists"
          $ failsSaying
            (cli <> "-c" <> conf <> "-s" <> badState <> "lock -i 0 -p 1")
            "Can't use lock when state file already exists"
      describe "increment" do
        it "fails on no conf"
          $ failsSaying
            (cli <> "-s" <> state <> "increment")
            "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ failsSaying
            (cli <> "-c" <> conf <> "increment")
            "Missing: (-s|--state-file STATE_FILE)"
        it "fails when state doesn't exists"
          $ failsSaying
            (cli <> "-c bad_path -s" <> state <> "increment")
            "[Error: ENOENT: no such file or directory, open 'bad_path']"
      describe "unlock" do
        it "fails on no conf"
          $ failsSaying
            (cli <> "-s" <> state <> "unlock")
            "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ failsSaying
            (cli <> "-c" <> conf <> "unlock")
            "Missing: (-s|--state-file STATE_FILE)"
        it "fails when state doesn't exists"
          $ failsSaying
            (cli <> "-c bad_path -s" <> state <> "unlock")
            "[Error: ENOENT: no such file or directory, open 'bad_path']"
      describe "query" do
        it "fails on no conf"
          $ failsSaying
            (cli <> "-s" <> state <> "query")
            "Missing: (-c|--config CONFIG_FILE)"
        it "fails on no state"
          $ failsSaying
            (cli <> "-c" <> conf <> "query")
            "Missing: (-s|--state-file STATE_FILE)"
        it "fails when state doesn't exists"
          $ failsSaying
            (cli <> "-c bad_path -s" <> state <> "query")
            "[Error: ENOENT: no such file or directory, open 'bad_path']"
      describe "integration test" do
        -- TODO I'm not sure why they aren't saying finished
        it "locks the value"
          $ passesSaying
            (cli <> "-c" <> conf <> "-s" <> state <> "lock -i 0 -p 1")
            "finished"
        it "querys the state"
          $ passesSaying
          (cli <> "-c" <> conf <> "-s" <> state <> "query")
          "Current datum:0"
        it "increments the datum"
          $ passesSaying
            (cli <> "-c" <> conf <> "-s" <> state <> "increment")
            "finished"
        it "querys the new state"
          $ passesSaying
          (cli <> "-c" <> conf <> "-s" <> state <> "query")
          "Current datum:1"
        it "it unlocks the value"
          $ passesSaying
            (cli <> "-c" <> conf <> "-s" <> state <> "unlock")
            "finished"
        it "removed the state file"
          $ failsSaying
            ("ls" <> state)
            "No such file"
      describe "plutip cleanup" do
        it "remove tmp wallet"
          $ rmWallet (trim $ conf)

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Error
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig:
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  }
