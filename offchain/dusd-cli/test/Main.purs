module Test.Main
  ( main
  ) where

import Contract.Prelude

import CmdUtils (fails, failsSaying, passesSaying)
import Ctl.Utils.Test (getPlutipConfig)
import Ctl.Utils.Test.Options (parser, Options(..))
import Ctl.Utils.Test.Types (Mode(..))
import Data.BigInt as BigInt
import Data.String (trim)
import Effect.Aff (launchAff_)
import Node.FS.Aff (unlink, exists)
import Node.Path (normalize)
import Options.Applicative (execParser)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldReturn)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec', defaultConfig)
import Test.Wallet (withFundedHsmWalletFile, withPlutipWalletFile, withTmpDir)

main :: Effect Unit
main = launchAff_ $ liftEffect (execParser parser) >>= executeTests

executeTests :: Options -> Aff Unit
executeTests (Options { mode, testResources }) = do
  let
    jsonDir = normalize $ testResources <> "/jsons/"

    -- TODO add this back when we have an API key again
    -- topup "addr_test1qrwdtldyjseyn3k978de87renmp2kt3vcajk65nk543tw865kp7y0evgnnne7ukzhqsmdmyefhpevpepl9p7xpe8zqpsag6004"

    -- The first string is the port arguments the second string is the wallet config path
    withEnv :: (String -> String -> Aff Unit) -> Aff Unit
    withEnv f = do
      config <- getPlutipConfig
      if mode == Local then withPlutipWalletFile config [ BigInt.fromInt 35_000_000 ] f
      else f "" (normalize $ jsonDir <> "/testWalletCfg.json ")

    cli = "dusd-cli --mainnet "
    badWalletConf = jsonDir <> "badWalletCfg.json "
    protocolFile = " protocol.json "
  -- remove the protocol config file if it exists
  hasOldProtocol <- exists (trim protocolFile)
  when hasOldProtocol $ unlink (trim protocolFile)

  runSpec' defaultConfig { timeout = Nothing } [ consoleReporter ] $ do
    describe "shell sanity checks" do
      it "ls should fail for a non-existing path" $
        "ls bad_path" `failsSaying` "No such file"

      it "ls should not fail for an existing path" $
        "ls -a" `passesSaying` "."

    describe ("dusd-cli --help") do
      it "contains contains correct executable name" $
        ("dusd-cli --help") `passesSaying` ("Usage: dusd-cli")

    describe ("dusd-cli") do
      it "should fail if the testnet or mainnet flag is missing"
        $ withEnv
        $ \ports wallet ->
            ("dusd-cli " <> ports <> "-w" <> wallet <> "-p" <> protocolFile <> "init") `failsSaying` "Missing: (--mainnet | --testnet)"

      it "should fail if the wallet-config option was not set" $
        (cli <> "-p" <> protocolFile <> "init") `failsSaying` ("Missing: (-w|--wallet-config FILE_PATH)")

      it "should fail if the wallet-config option path doesn't exist" $
        (cli <> "-w bad_path -p" <> protocolFile <> "init") `failsSaying` ("[Error: ENOENT: no such file or directory, open 'bad_path']")

      it "should fail if the path passed to wallet-config has the wrong contents"
        $ fails
        $ cli
        <> "-w"
        <> badWalletConf
        <> "-p"
        <> protocolFile
        <> "init"

    describe (cli <> "init") do
      when (mode == Local) $
        it "should succeed using the HSM wallet and create a protocol configuration file" do
          config <- getPlutipConfig
          withTmpDir $ \tmpDir ->
            withFundedHsmWalletFile config [ BigInt.fromInt 35_000_000 ] tmpDir $
              \ports wallet -> do
                hasOldProtocol <- exists (trim protocolFile)
                when hasOldProtocol $ unlink (trim protocolFile)
                (cli <> ports <> "-w" <> wallet <> "-p" <> protocolFile <> "init") `passesSaying` "initialized protocol"
                exists (trim protocolFile) `shouldReturn` true

      it "should initialize the protocol successfully and create a protocol config file"
        $ withEnv
        $ \ports wallet -> do
            hasOldProtocol <- exists (trim protocolFile)
            when hasOldProtocol $ unlink (trim protocolFile)
            (cli <> ports <> "-w" <> wallet <> "-p" <> protocolFile <> "init")
              `passesSaying` "initialized protocol"
            exists (trim protocolFile) `shouldReturn` true

      it "should initialize the protocol successfully and create a protocol config file at the default location"
        $ withEnv
        $ \ports wallet -> do
            hasOldProtocol <- exists "protocol.json"
            when hasOldProtocol $ unlink "protocol.json"
            (cli <> ports <> "-w" <> wallet <> "init")
              `passesSaying` "initialized protocol"
            exists "protocol.json" `shouldReturn` true

