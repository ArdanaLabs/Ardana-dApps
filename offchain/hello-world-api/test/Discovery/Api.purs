module Test.HelloWorld.Discovery.Api
  ( spec
  ) where

import Contract.Prelude

import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Data.BigInt as BigInt
import Plutus.Types.Value as Value

import Contract.Address (getWalletAddress)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.Test.Plutip (runContractInEnv)
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (getUtxo, getWalletBalance)
import Contract.Value (adaToken, scriptCurrencySymbol)
import Contract.Wallet (withKeyWallet)
import Data.Time.Duration (Minutes(..))
import Effect.Exception (throw)
import HelloWorld.Discovery.Api
  ( makeNftPolicy
  , mintNft
  , seedTx
  , protocolInit
  , stealConfig
  , openVault
  , getVault
  , incrementVault
  )
import Test.HelloWorld.EnvRunner (EnvRunner, runEnvSpec)
import Test.Spec (Spec, describe, it, itOnly)
import Test.Spec.Assertions (expectError, shouldEqual)
import Types.PlutusData (PlutusData)
import Util (buildBalanceSignAndSubmitTx, waitForTx, withOurLogger, maxWait)

spec :: EnvRunner -> Spec Unit
spec = runEnvSpec do
  describe "HelloWorld.Discovery.Api" do
    describe "nft" do

      it "mint runs" $ useRunnerSimple do
        mintNft

      it "double minting fails on second mint" $ useRunnerSimple do
        txOut <- seedTx
        nftPolicy <- makeNftPolicy txOut
        cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
        let
          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.mintingPolicy nftPolicy

          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
        -- TODO same as mintNft talk to ctl about this
        -- <> Constraints.mustSpendPubKeyOutput txOut
        txId <- buildBalanceSignAndSubmitTx lookups constraints
        adr <- liftContractM "no wallet" =<< getWalletAddress
        _ <- waitForTx maxWait adr txId
        expectError $ buildBalanceSignAndSubmitTx lookups constraints

      it "seedTx is spent after mint" $ useRunnerSimple do
        txOut <- seedTx
        nftPolicy <- makeNftPolicy txOut
        cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
        logInfo' $ "NFT cs: " <> show cs
        let
          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.mintingPolicy nftPolicy

          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
        -- TODO talk to ctl about this too
        -- <> Constraints.mustSpendPubKeyOutput txOut
        txId <- buildBalanceSignAndSubmitTx lookups constraints
        adr <- liftContractM "no Address" =<< getWalletAddress
        _ <- liftContractM "wait timed out" =<< waitForTx waitTime adr txId
        getUtxo txOut >>= case _ of
          Nothing -> pure unit
          Just _ -> liftEffect $ throw "seed tx still existed"

      it "wallet has nft after mint" $ useRunnerSimple do
        cs <- mintNft
        bal <- liftContractM "no ballance" =<< getWalletBalance
        let nfts = Value.valueOf bal cs adaToken
        nfts `shouldEqual` (BigInt.fromInt 1)

      it "burning nft fails" $ useRunnerSimple do
        txOut <- seedTx
        nftPolicy <- makeNftPolicy txOut
        cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
        logInfo' $ "NFT cs: " <> show cs
        let
          mintLookups :: Lookups.ScriptLookups PlutusData
          mintLookups = Lookups.mintingPolicy nftPolicy

          mintConstraints :: TxConstraints Unit Unit
          mintConstraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
        -- TODO this one too
        -- <> Constraints.mustSpendPubKeyOutput txOut
        txid <- buildBalanceSignAndSubmitTx mintLookups mintConstraints
        adr <- liftContractM "no wallet" =<< getWalletAddress
        _ <- waitForTx waitTime adr txid
        let
          burnLookups :: Lookups.ScriptLookups PlutusData
          burnLookups = Lookups.mintingPolicy nftPolicy

          burnConstraints :: TxConstraints Unit Unit
          burnConstraints =
            Constraints.mustSpendPubKeyOutput txOut
              <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt $ -1))
        expectError
          $ void
          $ buildBalanceSignAndSubmitTx burnLookups burnConstraints

  describe "protocol" do

    it "initialize protocol" $ useRunnerSimple do
      protocolInit

    it "initialize protocol but fail to steal the utxo" $ useRunnerSimple do
      txin <- _.config <$> protocolInit
      expectError $ stealConfig txin

    it "init protocol and open a vault" $ useRunnerSimple do
      protocol <- protocolInit
      openVault protocol

    it "find a vault" $ useRunnerSimple do
      protocol <- protocolInit
      vault <- openVault protocol
      getVault protocol vault

    it "increment a vault" $ useRunnerSimple do
      protocol <- protocolInit
      vault <- openVault protocol
      incrementVault protocol vault

    it "find after inc" $ useRunnerSimple do
      protocol <- protocolInit
      vault <- openVault protocol
      incrementVault protocol vault
      getVault protocol vault

useRunnerSimple :: forall a. Contract () a -> EnvRunner -> Aff Unit
useRunnerSimple contract runner = do
  runner \env alice -> do
    runContractInEnv (withOurLogger "apiTest.log" env)
      $ withKeyWallet alice
      $ void contract

waitTime :: Minutes
waitTime = 5.0 # Minutes
