module Test.Main
  ( main
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson)
import CBOR as CBOR
import Contract.Address (getWalletAddress)
import Contract.Log (logDebug', logError', logInfo')
import Contract.Monad (Contract, launchAff_, liftContractM)
import Contract.PlutusData (PlutusData, toData)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), PlutusScript(..), applyArgs, applyArgsM, mintingPolicyHash)
import Contract.Transaction (plutusV2Script)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo, getWalletBalance)
import Contract.Value (adaToken, mkTokenName, mpsSymbol, scriptCurrencySymbol)
import Contract.Value as Value
import Ctl.Util (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import DanaSwap.Api (initProtocol, mintNft, seedTx)
import DanaSwap.CborTyped (simpleNft)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Test.Spec (describe, it, itOnly)
import Test.Spec.Assertions (expectError, shouldEqual)
import TestUtil (Mode(..), runWithMode, useRunnerSimple)

testToken :: BigInt -> Contract () MintingPolicy
testToken n = do
  logDebug' "Creating test token"
  logDebug' $ "index:" <> show n
  raw <- decodeCborMp CBOR.trivial
  applyArgsM raw [ toData n ] >>= liftContractM "apply args failed"

decodeCborMp :: String -> Contract () MintingPolicy
decodeCborMp cborHex = liftContractM "failed to decode cbor"
  $ MintingPolicy
  <<< plutusV2Script
  <$> hexToByteArray cborHex

main :: Effect Unit
main = launchAff_ $ do
  mode <- liftEffect $ lookupEnv "MODE" >>= case _ of
    Just "local" -> pure Local
    Just "testnet" -> do
      -- TODO add this back when we have an API key again
      -- topup "addr_test1qqevfdhu80jsmjhzkf8lkkv5rza9h6k0u6hmwwr0r7vyjt9j3f374a2all0hc6vzxa6v7sax7du2lk5fu5q592d5fhqswar4hc"
      pure Testnet
    Just e -> throw $ "expected local or testnet got: " <> e
    Nothing -> throw "expected MODE to be set"
  log "about to start"
  runWithMode mode $ do
    itOnly "ctl redeemer pointer issue" $ useRunnerSimple $ do
       testMp1 <- testToken zero
       testMp2 <- testToken one
       testCs1 <- mpsSymbol (mintingPolicyHash testMp1) # liftContractM "failed to hash mp"
       testCs2 <- mpsSymbol (mintingPolicyHash testMp2) # liftContractM "failed to hash mp"
       testTn1 <- (mkTokenName =<< hexToByteArray "aaaa") # liftContractM "bad hex string"
       testTn2 <- (mkTokenName =<< hexToByteArray "aabb") # liftContractM "bad hex string"
       adr <- getWalletAddress >>= liftContractM "no wallet"
       void $ waitForTx adr =<< buildBalanceSignAndSubmitTx
         ( Lookups.mintingPolicy testMp1
             <> Lookups.mintingPolicy testMp2
         )
         ( Constraints.mustMintCurrency
             (mintingPolicyHash testMp1)
             testTn1
             zero
             <> Constraints.mustMintCurrency
               (mintingPolicyHash testMp2)
               testTn2
               (BigInt.fromInt 1_000_000)
         )
       pure $ (testCs1 /\ testTn1) /\ (testCs2 /\ testTn2)

    describe "protocol init" $ do
      -- @Todo implement https://github.com/ArdanaLabs/Danaswap/issues/16
      it "init protocol doesn't error" $ useRunnerSimple $ do
        initProtocol
    describe "NFT" do

      it "mints an NFT with the seed UTxO an an input" $ useRunnerSimple do
        mintNft

      it "cannot mint with the seed UTxO as a reference input" $ useRunnerSimple do
        txOut <- seedTx
        adr <- liftContractM "no wallet" =<< getWalletAddress
        utxos <- getUtxos adr
        nftPolicy <- simpleNft txOut
        cs <- liftContractM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
        let
          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.mintingPolicy nftPolicy
            <> Lookups.unspentOutputs utxos

          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
            <> Constraints.mustReferenceOutput txOut
        expectError $ buildBalanceSignAndSubmitTx lookups constraints

      it "double minting fails on second mint" $ useRunnerSimple do
        txOut <- seedTx
        adr <- liftContractM "no wallet" =<< getWalletAddress
        utxos <- getUtxos adr
        nftPolicy <- simpleNft txOut
        cs <- liftContractM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
        let
          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.mintingPolicy nftPolicy
            <> Lookups.unspentOutputs utxos

          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
            <> Constraints.mustSpendPubKeyOutput txOut
        txId <- buildBalanceSignAndSubmitTx lookups constraints
        _ <- waitForTx adr txId
        expectError $ buildBalanceSignAndSubmitTx lookups constraints

      it "spends the seed UTxO after minting" $ useRunnerSimple do
        txOut <- seedTx
        adr <- liftContractM "no wallet" =<< getWalletAddress
        utxos <- getUtxos adr
        nftPolicy <- simpleNft txOut
        cs <- liftContractM "failed to hash MintingPolicy into CurrencySymbol" $ scriptCurrencySymbol nftPolicy
        logInfo' $ "NFT cs: " <> show cs
        let
          lookups :: Lookups.ScriptLookups PlutusData
          lookups = Lookups.mintingPolicy nftPolicy
            <> Lookups.unspentOutputs utxos

          constraints :: TxConstraints Unit Unit
          constraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
            <> Constraints.mustSpendPubKeyOutput txOut
        txId <- buildBalanceSignAndSubmitTx lookups constraints
        _ <- waitForTx adr txId
        getUtxo txOut >>= case _ of
          Nothing -> pure unit
          Just _ -> liftEffect $ throw "seed tx still existed"

      it "sends the NFT to the wallet after minting" $ useRunnerSimple do
        cs <- mintNft
        bal <- liftContractM "no ballance" =<< getWalletBalance
        let nfts = Value.valueOf bal cs adaToken
        nfts `shouldEqual` (BigInt.fromInt 1)

      it "cannot burn an NFT" $ useRunnerSimple do
        txOut <- seedTx
        nftPolicy <- simpleNft txOut
        cs <- liftContractM "hash failed" $ scriptCurrencySymbol nftPolicy
        adr <- liftContractM "no wallet" =<< getWalletAddress
        utxos <- getUtxos adr
        let
          mintLookups :: Lookups.ScriptLookups PlutusData
          mintLookups = Lookups.mintingPolicy nftPolicy
            <> Lookups.unspentOutputs utxos

          mintConstraints :: TxConstraints Unit Unit
          mintConstraints = Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt 1))
            <> Constraints.mustSpendPubKeyOutput txOut
        txid <- buildBalanceSignAndSubmitTx mintLookups mintConstraints
        _ <- waitForTx adr txid
        let
          burnLookups :: Lookups.ScriptLookups PlutusData
          burnLookups = Lookups.mintingPolicy nftPolicy
            <> Lookups.unspentOutputs utxos

          burnConstraints :: TxConstraints Unit Unit
          burnConstraints =
            Constraints.mustSpendPubKeyOutput txOut
              <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt $ -1))
        expectError
          $ void
          $ buildBalanceSignAndSubmitTx burnLookups burnConstraints
