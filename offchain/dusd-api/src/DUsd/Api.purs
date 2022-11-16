module DUsd.Api
  ( module Types
  , module Params
  , module NFT
  , module Config
  -- local
  , initProtocol
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (toData)
import DUsd.Config (initConfigWith)
import DUsd.Config (initConfigWith, updateConfig) as Config
import DUsd.Nft (lookupUtxo, mintNft, seedTx) as NFT
import DUsd.Nft (mintNft)
import DUsd.Params (initParams)
import DUsd.Params (initParams, updateParams) as Params
import DUsd.PriceOracle (startPriceOracle)
import DUsd.Types (Params(..), Protocol(..)) as Types
import DUsd.Types (Params, Protocol(..))

initProtocol :: Params -> Contract () Protocol
initProtocol initialParams = do
  configNftCs <- mintNft
  params <- initParams initialParams
  priceOracle <- startPriceOracle
  configUtxo <- initConfigWith configNftCs (toData unit) -- TODO real data here
  -- TODO this type is incomplete
  pure $ Protocol { params, configUtxo,priceOracle }
