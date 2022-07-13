module Parser
  (parser
  ) where

import Prelude

import Options.Applicative
  (CommandFields
  ,Mod
  ,Parser
  ,ParserInfo
  ,command
  ,info
  ,int
  ,long
  ,metavar
  ,option
  ,progDesc
  ,short
  ,strOption
  ,hsubparser
  ,fullDesc
  ,header
  ,commandGroup
  ,helper
  )
import Types(ParsedOptions(..),SubCommand(..))

parser :: ParserInfo ParsedOptions
parser = info rawParser
  (fullDesc
    <> progDesc "This command can be used to automate transactions with the hello-world api"
    <> header "Hello-World cli"
    )

rawParser :: Parser ParsedOptions
rawParser = (helper <*>  _)
  $ map ParsedOptions
  $ {configFile:_,statePath:_,subCommand:_}
  <$> config
  <*> stateFile
  <*> subCommand

config :: Parser String
config = strOption (long "config" <> short 'c' <> metavar "CONFIG_FILE")

stateFile :: Parser String
stateFile = strOption (long "state-file" <> short 's' <> metavar "STATE_FILE")

subCommand :: Parser SubCommand
subCommand = hsubparser $
  (commandGroup "Cli commands:") <>
  lock <> increment <> end

lock :: Mod CommandFields SubCommand
lock  = command "lock" (info (Lock <$> lockOptions) (progDesc "lock some ada with the contract"))
  where
    lockOptions :: Parser {contractParam :: Int,initialDatum :: Int}
    lockOptions =
      {contractParam:_,initialDatum:_}
        <$> option int (long "param" <> short 'p' <> metavar "CONTRACT_PARAMETER")
        <*> option int (long "init" <> short 'i' <> metavar "INITIAL_DATUM")

increment ::  Mod CommandFields SubCommand
increment = command "inc" (info (pure Increment) (progDesc "increment the datum"))

end ::  Mod CommandFields SubCommand
end = command "end" (info (pure End) (progDesc "redeem the value"))

