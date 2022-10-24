module DanaSwap.Cli.Parser
  ( parser
  ) where

import Prelude

import Contract.Address (NetworkId(..))
import Control.Alt ((<|>))
import DanaSwap.Cli.Types (Options(..), Command(..))
import Data.Either (Either(..))
import Data.String.Common (toLower)
import Data.UInt (UInt, fromInt)
import Node.Path (FilePath)
import Options.Applicative (CommandFields, Mod, Parser, ParserInfo, command, commandGroup, fullDesc, header, help, helper, hsubparser, info, int, long, metavar, option, progDesc, short, strOption, eitherReader, flag')
import Options.Applicative.Types (ReadM, optional)

parser :: ParserInfo Options
parser = info rawParser
  ( fullDesc
      <> progDesc "This comand-line tool can be used to automate transactions with the DanaSwap API"
      <> header "DanaSwap CLI"
  )

rawParser :: Parser Options
rawParser = (helper <*> _)
  $ map Options
  $ { stateFilePath: _, walletConfigFilePath: _, networkId: _, command: _, ctlPort: _, ogmiosPort: _, odcPort: _ }
      <$> stateFilePath
      <*> walletConfigFilePath
      <*> networkId
      <*> command'
      <*> optional ctlPort
      <*> optional ogmiosPort
      <*> optional odcPort

walletConfigFilePath :: Parser FilePath
walletConfigFilePath = strOption $
  long "wallet-config"
    <> short 'w'
    <> metavar "FILE_PATH"
    <> help "the config file for the wallet to be used"

networkId :: Parser NetworkId
networkId = mainnet <|> testnet

mainnet :: Parser NetworkId
mainnet = flag' MainnetId $
  long "mainnet"
    <> help "Run on mainnet."

testnet :: Parser NetworkId
testnet = flag' TestnetId $
  long "testnet"
    <> help "Run on tesnet."

stateFilePath :: Parser FilePath
stateFilePath = strOption $
  long "state-file"
    <> short 's'
    <> metavar "FILE_PATH"
    <> help "the path to the state file to be used"

ctlPort :: Parser UInt
ctlPort = option (fromInt <$> int) $
  long "ctl-port"
    <> metavar "PORT"
    <> help "The port number for the ctl-server."

ogmiosPort :: Parser UInt
ogmiosPort = option (fromInt <$> int) $
  long "ogmios-port"
    <> metavar "PORT"
    <> help "The port number for ogmios."

odcPort :: Parser UInt
odcPort = option (fromInt <$> int) $
  long "odc-port"
    <> metavar "PORT"
    <> help "The port number for the ogmios datum cache."

command' :: Parser Command
command' = hsubparser $
  (commandGroup "CLI commands:")
    <> initializeProtocol

initializeProtocol :: Mod CommandFields Command
initializeProtocol = command "init" (info (pure InitializeProtocol) (progDesc "Initialize the DanaSwap protocol"))

