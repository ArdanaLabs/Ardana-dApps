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
import DUsd.Nft (mintNft)
import DUsd.Config (initConfigUtxoWith)
import DUsd.Config (initConfigUtxoWith, updateConfigUtxo) as Config
import DUsd.Nft (lookupUtxo, mintNft, seedTx) as NFT
import DUsd.PriceOracle (startPriceOracle)
import DUsd.Params (initProtocolParams)
import DUsd.Params (initProtocolParams, updateDebtFloor, updateLiquidationDiscount, updateLiquidationFee, updateLiquidationRatio, updateProtocolParams) as Params
import DUsd.Types (Protocol(..), ProtocolParams)
import DUsd.Types (AssetClass, Protocol(..), ProtocolParams(..), UtxoId(..)) as Types

initProtocol :: ProtocolParams -> Contract () Protocol
initProtocol params = do
  configNftCS <- mintNft
  protocolParams <- initProtocolParams params
  priceOracle <- startPriceOracle
  configUtxo <- initConfigUtxoWith configNftCS (toData unit) -- TODO real data here
  -- TODO this type is incomplete
  pure $ Protocol { protocolParams, configUtxo,priceOracle }
