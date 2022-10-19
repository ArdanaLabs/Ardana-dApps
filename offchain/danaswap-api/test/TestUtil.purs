module TestUtil
  ( getEnvRunner
  , runEnvSpec
  , useRunnerSimple
  , Mode(..)
  , EnvRunner
  , EnvSpec
  , runOurSpec
  ) where

import Contract.Prelude

import Contract.Config (testnetConfig)
import Contract.Monad (Contract, ContractEnv, withContractEnv)
import Contract.Test.Plutip (PlutipConfig, runContractInEnv, withKeyWallet, withPlutipContractEnv)
import Contract.Wallet (KeyWallet, privateKeysToKeyWallet)
import Contract.Wallet.KeyFile (privatePaymentKeyFromFile, privateStakeKeyFromFile)
import Ctl.Internal.Plutip.PortCheck (isPortAvailable)
import Ctl.Util (withOurLogger)
import Data.BigInt as BigInt
import Data.Identity (Identity)
import Data.List.Lazy (replicateM, toUnfoldable)
import Data.UInt as UInt
import Effect.Exception (throw)
import Effect.Random (randomInt)
import Node.Process (lookupEnv)
import Test.Spec (Spec, SpecT, before, parallel, sequential)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

data Mode = Local | Testnet
type EnvRunner = (ContractEnv () -> KeyWallet -> Aff Unit) -> Aff Unit
type EnvSpec = SpecT Aff EnvRunner Identity Unit

derive instance Eq Mode

runOurSpec :: Mode -> Aff EnvRunner -> EnvSpec -> Aff Unit
runOurSpec mode runnerGetter s =
  runSpec'
    defaultConfig
      { timeout = Nothing }
    [ specReporter ]
    $ (_ `runEnvSpec` runnerGetter)
    $ (if mode == Local then parallel else sequential)
    $ s

-- | returns a contiunation that gets the EnvRunner
-- This is nesecary to allow control over which parts
-- run once vs each time
getEnvRunner :: Mode -> Aff (Aff EnvRunner)
getEnvRunner Local = pure $ do
  newCfg <- plutipConfig
  pure $ withPlutipContractEnv newCfg $ defaultWallet
getEnvRunner Testnet = do
  testResourcesDir <- liftEffect $ fromMaybe "./fixtures/" <$> lookupEnv "TEST_RESOURCES"
  key <- privatePaymentKeyFromFile $ testResourcesDir <> "/wallet.skey"
  stakeKey <- privateStakeKeyFromFile $ testResourcesDir <> "/staking.skey"
  let keyWallet = privateKeysToKeyWallet key (Just stakeKey)
  pure $ pure
    $ \f -> withContractEnv (testnetConfig { logLevel = Warn }) $ \env -> f (env :: ContractEnv ()) (keyWallet :: KeyWallet)

runEnvSpec :: EnvSpec -> Aff EnvRunner -> Spec Unit
runEnvSpec = flip before

useRunnerSimple :: forall a. Contract () a -> EnvRunner -> Aff Unit
useRunnerSimple contract runner = do
  runner \env alice ->
    runContractInEnv (withOurLogger "apiTest.log" env)
      $ withKeyWallet alice
      $ void contract

defaultWallet :: Array BigInt.BigInt
defaultWallet = [ BigInt.fromInt 40_000_000, BigInt.fromInt 40_000_000 ]

freePort :: Aff Int
freePort = do
  nextTry <- liftEffect $ randomInt 1024 0xFFFF
  isGood <- isPortAvailable (UInt.fromInt nextTry)
  if isGood then pure nextTry
  else freePort

plutipConfig :: Aff PlutipConfig
plutipConfig = do
  toUnfoldable <$> replicateM 5 freePort >>= case _ of
    [ p1, p2, p3, p4, p5 ] -> pure $
      { host: "127.0.0.1"
      , port: UInt.fromInt p1
      , logLevel: Warn
      -- Server configs are used to deploy the corresponding services.
      , ogmiosConfig:
          { port: UInt.fromInt p2
          , host: "127.0.0.1"
          , secure: false
          , path: Nothing
          }
      , ogmiosDatumCacheConfig:
          { port: UInt.fromInt p3
          , host: "127.0.0.1"
          , secure: false
          , path: Nothing
          }
      , ctlServerConfig: Just
          { port: UInt.fromInt p4
          , host: "127.0.0.1"
          , secure: false
          , path: Nothing
          }
      , postgresConfig:
          { host: "127.0.0.1"
          , port: UInt.fromInt p5
          , user: "ctxlib"
          , password: "ctxlib"
          , dbname: "ctxlib"
          }
      , customLogger: Nothing -- TODO api logger here
      , suppressLogs: false
      }
    _ -> liftEffect $ throw "replicateM returned list of the wrong length in plutipConfig"
