module DUsd.Api
  ( module Types
  , module Params
  , module NFT
  , module Config
  , initProtocol
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import DUsd.Config (initConfigWith)
import DUsd.Config (initConfigWith, updateConfig) as Config
import DUsd.Nft (mintNft)
import DUsd.Nft (mintNft, seedTx) as NFT
import DUsd.Params (initParams)
import DUsd.Params (initParams, updateParams) as Params
import DUsd.Types (Params(..), ParamsInfo, Protocol(..)) as Types
import DUsd.Types (Params(..), Protocol(..))

initProtocol :: Contract () Protocol
initProtocol = do
  configNftCs <- mintNft
  params <- initParams (Params undefined)
  configUtxo <- initConfigWith configNftCs (toData unit) -- TODO real data here
  -- TODO this type is incomplete
  pure $ Protocol { params, configUtxo }
