module DanaSwap.Cli.Types
  ( Command(..)
  , Options(..)
  , WalletConf(..)
  ) where

import Prelude

import Aeson (class DecodeAeson, decodeAeson)
import Contract.Address (NetworkId)
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.UInt (UInt)
import Node.Path (FilePath)

data Command = InitializeProtocol

data WalletConf
  = KeyWalletFiles { walletPath :: String, stakingPath :: Maybe String }
  | YubiHSM { useYubiHSM :: Boolean }

-- | The available Danaswap CLI options
data Options = Options
  { command :: Command
  , protocolFilePath :: FilePath
  , walletConfigFilePath :: FilePath
  , networkId :: NetworkId
  , ctlPort :: Maybe UInt
  , ogmiosPort :: Maybe UInt
  , odcPort :: Maybe UInt
  }

derive instance Generic Command _
instance Show Command where
  show = genericShow

derive instance Generic Options _
instance Show Options where
  show = genericShow

instance DecodeAeson WalletConf where
  decodeAeson a =
    (KeyWalletFiles <$> decodeAeson a) <|> (YubiHSM <$> decodeAeson a)

