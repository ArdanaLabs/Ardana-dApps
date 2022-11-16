module DUsd.Cli.Parser
  ( parser
  ) where

import Prelude

import Contract.Address (NetworkId(..))
import Contract.Prelude (undefined)
import Control.Alt ((<|>))
import DUsd.Cli.Types (Options(..), Command(..))
import Data.UInt (UInt, fromInt)
import Node.Path (FilePath)
import Options.Applicative (CommandFields, Mod, Parser, ParserInfo, command, commandGroup, flag', fullDesc, header, help, helper, hsubparser, info, int, long, metavar, option, progDesc, short, str, strArgument, strOption)
import Options.Applicative.Types (optional)

parser :: ParserInfo Options
parser = info rawParser
  ( fullDesc
      <> progDesc "This comand-line tool can be used to automate transactions with the dUSD API"
      <> header "dUSD CLI"
  )

rawParser :: Parser Options
rawParser = (helper <*> _)
  $ map Options
  $ { protocolFilePath: _, walletConfigFilePath: _, networkId: _, command: _, ctlPort: _, ogmiosPort: _, odcPort: _ }
      <$> optional protocolFilePath
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

protocolFilePath :: Parser FilePath
protocolFilePath = strOption $
  long "protocol-config"
    <> short 'p'
    <> metavar "FILE_PATH"
    <> help "The path to the protocol configuration file to be used (JSON). In the case of initialize this options specifies the output filepath of the initialized protocol. In every other case this option specifies the location of the protocol that should be used."

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
initializeProtocol =
  command "init"
    ( info
        (InitializeProtocol <<< { paramsPath: _ } <$> strOption (long "init-params" <> short 'i' <> metavar "InitialParamsFile" <> help "the path to the params file"))
        (progDesc "Initialize the dUSD protocol")
    )

