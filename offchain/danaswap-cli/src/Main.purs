module Main
  ( main
  ) where

import Contract.Prelude
import Effect.Aff (launchAff_)
import Options.Applicative (execParser)
import DanaSwap.Cli.Parser (parser)
import DanaSwap.Cli.Runners (runCli)

main :: Effect Unit
main = launchAff_ $ runCli =<< liftEffect (execParser parser)
