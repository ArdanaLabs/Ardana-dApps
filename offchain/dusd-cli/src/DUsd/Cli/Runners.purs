module DUsd.Cli.Runners
  ( runCli
  ) where

import Contract.Prelude

import Aeson (decodeAeson, parseJsonStringToAeson, encodeAeson)
import Contract.Address (NetworkId)
import Contract.Config (testnetConfig)
import Contract.Monad (ConfigParams, runContract)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet, withKeyWallet)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Ctl.Utils.HsmWallet (makeHsmWallet)
import DUsd.Api (Protocol, ProtocolParams, initProtocol)
import DUsd.Cli.Types (Command(..), Options(..), WalletConf(..))
import Data.UInt as U
import Effect.Exception (throw)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.FS.Sync (exists)
import Node.Path (FilePath, dirname, isAbsolute, normalize)
import Node.Process (cwd)

runCli :: Options -> Aff Unit
runCli (Options { command, protocolFilePath, walletConfigFilePath, networkId, ctlPort, ogmiosPort, odcPort }) = do
  wallet <- parseWalletFromConfigFile walletConfigFilePath
  let configParams = createCtlConfigParams networkId ctlPort ogmiosPort odcPort

  realProtocolFilePath <- case protocolFilePath of
    Nothing -> do
      dir <- liftEffect cwd
      pure $ normalize $ dir <> "/protocol.json"
    Just path -> pure path

  case command of
    (InitializeProtocol { paramsPath }) -> do
      stateExists <- liftEffect $ exists realProtocolFilePath
      when stateExists $ do
        void $ liftEffect $ throw "Can't use initialize when state file already exists"
      params <- parseParamsFromFile paramsPath
      protocolParameters <- runContract configParams $ withKeyWallet wallet $ initProtocol params
      writeTextFile UTF8 realProtocolFilePath $ show $ encodeAeson protocolParameters
      log $ "created protocol configuration file: " <> realProtocolFilePath
      log "initialized protocol"

parseParamsFromFile :: FilePath -> Aff ProtocolParams
parseParamsFromFile path = do
  rawText <- readTextFile UTF8 path
  throwE =<< decodeAeson <$> throwE (parseJsonStringToAeson rawText)

parseWalletFromConfigFile :: FilePath -> Aff KeyWallet
parseWalletFromConfigFile walletConfigFilePath = do
  walletConfigRaw <- readTextFile UTF8 walletConfigFilePath
  walletConfigJson <- throwE =<< decodeAeson <$> throwE (parseJsonStringToAeson walletConfigRaw)
  let walletConfigDir = dirname walletConfigFilePath
  case walletConfigJson of
    KeyWalletFiles { walletPath, stakingPath } -> do
      let
        getAbsolutePath path = normalize $
          if isAbsolute path then path
          else walletConfigDir <> "/" <> path
      key <- privatePaymentKeyFromFile $ getAbsolutePath walletPath
      mstake <- traverse privateStakeKeyFromFile $ getAbsolutePath <$> stakingPath
      pure $ privateKeysToKeyWallet key mstake
    YubiHSM _ -> makeHsmWallet

createCtlConfigParams :: NetworkId -> Maybe U.UInt -> Maybe U.UInt -> Maybe U.UInt -> ConfigParams ()
createCtlConfigParams networkId maybeCtlPort maybeOgmiosPort maybeOdcPort =
  let
    cfg' = testnetConfig { walletSpec = Nothing, networkId = networkId }
  in
    cfg'
      { ctlServerConfig = case cfg'.ctlServerConfig of
          Nothing -> Nothing
          Just serverConfig -> case maybeCtlPort of
            Nothing -> Just serverConfig
            Just port -> Just $ serverConfig { port = port }
      , ogmiosConfig { port = fromMaybe cfg'.ogmiosConfig.port maybeOgmiosPort }
      , datumCacheConfig { port = fromMaybe cfg'.datumCacheConfig.port maybeOdcPort }
      }

readProtocol :: String -> Aff Protocol
readProtocol statePath = do
  stateExists <- liftEffect $ exists statePath
  unless stateExists $ do
    liftEffect $ throw "State file could not be read because it doesn't exist"
  stateTxt <- readTextFile UTF8 statePath
  throwE =<< decodeAeson <$> throwE (parseJsonStringToAeson stateTxt)

throwE :: forall a b. Show a => Either a b -> Aff b
throwE (Left a) = liftEffect $ throw $ show a
throwE (Right b) = pure b

