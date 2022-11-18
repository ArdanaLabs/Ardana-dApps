module DUsd.Browser.Store where

import Contract.Monad (ConfigParams)
import Data.Maybe (Maybe(..))

type Store =
  { contractConfig :: Maybe (ConfigParams ())
  }

data Action = SetContractConfig (ConfigParams ())

reduce :: Store -> Action -> Store
reduce store = case _ of
  SetContractConfig contractConfig ->
    store { contractConfig = Just contractConfig }