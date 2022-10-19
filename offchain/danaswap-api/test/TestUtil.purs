module TestUtil
  ( getEnvRunner
  , runEnvSpec
  , useRunnerSimple
  , Mode(..)
  , EnvRunner
  , EnvSpec
  ) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (Contract, ContractEnv, withContractEnv)
import Contract.Test.Plutip (PlutipConfig, runContractInEnv, withKeyWallet, withPlutipContractEnv)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Data.BigInt as BigInt
import Data.Identity (Identity)
import Data.UInt as UInt
import Node.Process (lookupEnv)
import Test.Spec (Spec, SpecT, before)
import Ctl.Utils (withOurLogger)

data Mode = Local | Testnet
type EnvRunner = (ContractEnv () -> KeyWallet -> Aff Unit) -> Aff Unit
type EnvSpec = SpecT Aff EnvRunner Identity Unit

getEnvRunner :: Mode -> Aff EnvRunner
getEnvRunner Local = pure $ withPlutipContractEnv plutipConfig $ defaultWallet
getEnvRunner Testnet = do
  testResourcesDir <- liftEffect $ fromMaybe "./fixtures/" <$> lookupEnv "TEST_RESOURCES"
  key <- privatePaymentKeyFromFile $ testResourcesDir <> "/wallet.skey"
  stakeKey <- privateStakeKeyFromFile $ testResourcesDir <> "/staking.skey"
  let keyWallet = privateKeysToKeyWallet key (Just stakeKey)
  pure
    $ \f -> withContractEnv (testnetConfig { logLevel = Warn }) $ \env -> f (env :: ContractEnv ()) (keyWallet :: KeyWallet)

runEnvSpec :: EnvSpec -> EnvRunner -> Spec Unit
runEnvSpec s r = before (pure r) s

useRunnerSimple :: forall a. Contract () a -> EnvRunner -> Aff Unit
useRunnerSimple contract runner = do
  runner \env alice ->
    runContractInEnv (withOurLogger "apiTest.log" env)
      $ withKeyWallet alice
      $ void contract

defaultWallet :: Array BigInt.BigInt
defaultWallet = [ BigInt.fromInt 40_000_000, BigInt.fromInt 40_000_000 ]

plutipConfig :: PlutipConfig
plutipConfig =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8084
  , logLevel: Warn
  -- Server configs are used to deploy the corresponding services.
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ctlServerConfig: Just
      { port: UInt.fromInt 8083
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  , customLogger: Nothing -- TODO api logger here
  , suppressLogs: false
  }
