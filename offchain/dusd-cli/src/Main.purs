module Main
  ( main
  ) where

import Contract.Prelude
import Effect.Aff (launchAff_)
import Options.Applicative (execParser)
import DUsd.Cli.Parser (parser)
import DUsd.Cli.Runners (runCli)

main :: Effect Unit
main = launchAff_ $ runCli =<< liftEffect (execParser parser)
