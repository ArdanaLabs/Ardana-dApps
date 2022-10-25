module DanaSwap.Cli.Runners
  ( runCli
  ) where

import Contract.Prelude

import Aeson (decodeAeson, parseJsonStringToAeson, encodeAeson)
import Contract.Address (NetworkId)
import Contract.Config (testnetConfig)
import Contract.Monad (ConfigParams, runContract)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet, withKeyWallet)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Ctl.Utils.HsmWallet (makeHsmWallet)
import DanaSwap.Api (initProtocol)
import DanaSwap.Cli.Types (CliState(..), Command(..), FileState, Options(..), WalletConf(..))
import Data.UInt as U
import Effect.Exception (throw)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, writeTextFile, unlink)
import Node.FS.Sync (exists)
import Node.Path (FilePath, dirname, isAbsolute, normalize)

runCli :: Options -> Aff Unit
runCli (Options { command, stateFilePath, walletConfigFilePath, networkId, ctlPort, ogmiosPort, odcPort }) = do
  wallet <- parseWalletFromConfigFile walletConfigFilePath
  let configParams = createCtlConfigParams networkId ctlPort ogmiosPort odcPort

  case command of
    InitializeProtocol -> do
      stateExists <- liftEffect $ exists stateFilePath
      when stateExists $ do
        void $ liftEffect $ throw "Can't use lock when state file already exists"
      protocolParameters <- runContract configParams $ withKeyWallet wallet initProtocol
      writeState stateFilePath $ State { lastOutput: (protocolParameters.configUtxo) }
      log "initialized protocol"

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

writeState :: String -> CliState -> Aff Unit
writeState statePath s = do
  writeTextFile UTF8 statePath $ s # logState # encodeAeson # show

readState :: String -> Aff CliState
readState statePath = do
  stateExists <- liftEffect $ exists statePath
  unless stateExists $ do
    liftEffect $ throw "State file could not be read because it doesn't exist"
  stateTxt <- readTextFile UTF8 statePath
  (partial :: FileState) <- throwE =<< decodeAeson <$> throwE (parseJsonStringToAeson stateTxt)
  pure $ State $
    { lastOutput: parseTxId partial.lastOutput
    }

logState :: CliState -> FileState
logState (State { lastOutput }) = { lastOutput: logTxId lastOutput }

logTxId :: TransactionInput -> { index :: Int, transactionId :: String }
logTxId (TransactionInput { index, transactionId: TransactionHash bytes }) = { index: U.toInt index, transactionId: byteArrayToHex bytes }

parseTxId :: { index :: Int, transactionId :: String } -> TransactionInput
parseTxId { index, transactionId } = TransactionInput
  { index: U.fromInt index, transactionId: TransactionHash $ hexToByteArrayUnsafe transactionId }

clearState :: String -> Aff Unit
clearState = unlink

throwE :: forall a b. Show a => Either a b -> Aff b
throwE (Left a) = liftEffect $ throw $ show a
throwE (Right b) = pure b

