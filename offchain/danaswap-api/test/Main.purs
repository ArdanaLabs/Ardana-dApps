module Test.Main
  ( main
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress, scriptHashAddress)
import Contract.Log (logInfo')
import Contract.Monad (launchAff_, liftContractM)
import Contract.PlutusData (Datum(..), PlutusData(..), Redeemer(..), toData)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (mintingPolicyHash, validatorHash)
import Contract.Test.Plutip (withKeyWallet)
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo, getWalletBalance)
import Contract.Value (adaToken, mkTokenName, mpsSymbol, scriptCurrencySymbol)
import Contract.Value as Value
import Ctl.Util (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import DanaSwap.Api (depositLiquidity, initProtocol, mintNft, openPool, seedTx)
import DanaSwap.CborTyped (configAddressValidator, simpleNft)
import Data.BigInt as BigInt
import Effect.Exception (throw)
import Node.Process (lookupEnv)
import Setup (prepTestTokens)
import Test.Attacks.Api (depositLiquiditySneaky, openPoolSneaky, regularDeposit, regularOpen)
import Test.Spec (describe, it)
import Test.Attacks.Api (depositLiquidityWrongTokenRightRedeemer, depositLiquidityWrongTokenWrongRedeemer, openPoolMultipleTokens, openPoolWrongTokenRightRedeemer, openPoolWrongTokenWrongRedeemer)
import Test.Spec (describe, it, parallel, sequential)
import Test.Spec.Assertions (expectError, shouldEqual)
import TestUtil (Mode(..), expectScriptError, runTwoWallets, runWithMode, useRunnerSimple)

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
  log "About to start tests"
  let maybePar = if mode == Local then parallel else sequential
  runWithMode mode $ do
    describe "Pool id minting Policy tests" $ maybePar$ do

      it "Allows minting id on pool open" $ useRunnerSimple $ do
        protocol <- initProtocol
        (ac1 /\ ac2) <- prepTestTokens
        openPool
          protocol
          ac1
          ac2
          (BigInt.fromInt 100)
          (BigInt.fromInt 100)
      -- This is the same as the liquidity test of the same name

      describe "Open a pool with zero liqudity" $ do
        it "Fails when both tokens are zero" $ useRunnerSimple $ do
          protocol <- initProtocol
          (ac1 /\ ac2) <- prepTestTokens
          expectScriptError $ openPool
            protocol
            ac1
            ac2
            (BigInt.fromInt 0)
            (BigInt.fromInt 0)

        it "fails when the first token is zero" $ useRunnerSimple $ do
          protocol <- initProtocol
          (ac1 /\ ac2) <- prepTestTokens
          expectScriptError $ openPool
            protocol
            ac1
            ac2
            (BigInt.fromInt 0)
            (BigInt.fromInt 100)

        it "fails when the second token is zero" $ useRunnerSimple $ do
          protocol <- initProtocol
          (ac1 /\ ac2) <- prepTestTokens
          expectScriptError $ openPool
            protocol
            ac1
            ac2
            (BigInt.fromInt 100)
            (BigInt.fromInt 0)

        it "Fails when neither is zero but token we report and mint zero anyway" $ useRunnerSimple $ do
          protocol <- initProtocol
          (ac1 /\ ac2) <- prepTestTokens
          expectScriptError $ openPoolSneaky
            regularOpen
              { reportIssued = Just zero
              , actuallyMint = Just $ \_ _ -> mempty
              }
            protocol
            ac1
            ac2
            (BigInt.fromInt 100)
            (BigInt.fromInt 100)

      describe "Under paying for liquidity" $ do
        it "Fails when reporting correctly" $ useRunnerSimple $ do
          protocol <- initProtocol
          (ac1 /\ ac2) <- prepTestTokens
          expectScriptError $ openPoolSneaky
            regularOpen
              { reportIssued = Just $ BigInt.fromInt 90
              , actuallyMint = Just $ \cs tn -> Value.singleton cs tn (BigInt.fromInt 100)
              }
            protocol
            ac1
            ac2
            (BigInt.fromInt 90)
            (BigInt.fromInt 90)

        it "Fails when reporting paying in full" $ useRunnerSimple $ do
          protocol <- initProtocol
          (ac1 /\ ac2) <- prepTestTokens
          expectScriptError $ openPoolSneaky
            regularOpen
              { reportIssued = Just $ BigInt.fromInt 100
              , actuallyMint = Just $ \cs tn -> Value.singleton cs tn (BigInt.fromInt 100)
              }
            protocol
            ac1
            ac2
            (BigInt.fromInt 90)
            (BigInt.fromInt 90)

      it "Fails when config utxo is emmited" $ useRunnerSimple $ do
        protocol <- initProtocol
        (ac1 /\ ac2) <- prepTestTokens
        expectScriptError $ openPoolSneaky
          regularOpen
            { hasConfig = false
            }
          protocol
          ac1
          ac2
          (BigInt.fromInt 90)
          (BigInt.fromInt 90)

      it "Fails when config utxo is invalid" $ useRunnerSimple $ do
        protocol <- initProtocol
        (ac1 /\ ac2) <- prepTestTokens
        configAdrVal <- configAddressValidator
        liquidityCS <- liftContractM "invalid hex string from mintingPolicyHash"
          $ mpsSymbol
          $ mintingPolicyHash protocol.liquidityMP
        badConfig <- waitForTx (scriptHashAddress $ validatorHash configAdrVal)
          =<< buildBalanceSignAndSubmitTx
            mempty
            ( Constraints.mustPayToScript
                (validatorHash configAdrVal)
                ( Datum $ List
                    [ toData (validatorHash $ protocol.poolAdrVal), toData liquidityCS ]
                )
                DatumInline
                mempty
            )
        expectScriptError $ openPoolSneaky
          regularOpen
            { badConfig = Just badConfig
            }
          protocol
          ac1
          ac2
          (BigInt.fromInt 90)
          (BigInt.fromInt 90)

      it "Fails when minting two id tokens" $ useRunnerSimple $ do
        protocol <- initProtocol
        (ac1 /\ ac2) <- prepTestTokens
        expectScriptError $ openPoolSneaky
          regularOpen
            { numberOfIdsToMint = Just (BigInt.fromInt 2)
            }
          protocol
          ac1
          ac2
          (BigInt.fromInt 90)
          (BigInt.fromInt 90)

      it "Fails when user keeps id token" $ useRunnerSimple $ do
        protocol <- initProtocol
        (ac1 /\ ac2) <- prepTestTokens
        expectScriptError $ openPoolSneaky
          regularOpen
            { keepId = true
            }
          protocol
          ac1
          ac2
          (BigInt.fromInt 90)
          (BigInt.fromInt 90)

      it "Fails when pool is opened with wrong id" $ useRunnerSimple $ do
        protocol <- initProtocol
        (ac1 /\ ac2) <- prepTestTokens
        badId <- (hexToByteArray "aaaa" >>= mkTokenName) # liftContractM "failed to make token name"
        expectScriptError $ openPoolSneaky
          regularOpen
            { idToMint = Just badId
            }
          protocol
          ac1
          ac2
          (BigInt.fromInt 90)
          (BigInt.fromInt 90)

      when (mode == Local) $ it "Fails when seed utxo is not spent" $ runTwoWallets $
        \alice bob -> do
          protocol <- withKeyWallet alice $ initProtocol
          (ac1 /\ ac2) <- withKeyWallet bob $ prepTestTokens
          expectScriptError $ withKeyWallet bob $ openPoolSneaky
            regularOpen
              { spendSeedTx = false
              }
            protocol
            ac1
            ac2
            (BigInt.fromInt 90)
            (BigInt.fromInt 90)

    describe "Liquidity Token Minting Policy" $ maybePar do

      -- TODO there are more liquidity tests but
      -- they depend partially on the pool address validator as well
      -- so they will be added in a later PR

      it "Allows minting on pool open" $ useRunnerSimple $ do
        protocol <- initProtocol
        (ac1 /\ ac2) <- prepTestTokens
        openPool
          protocol
          ac1
          ac2
          (BigInt.fromInt 100)
          (BigInt.fromInt 100)

      describe "Minting tokens for a different pool on pool open" $ do
        it "Fails to validate with the wrong redeemer" $ useRunnerSimple $ do
          protocol <- initProtocol
          wrongId <- liftContractM "bad hex string" $ mkTokenName =<< hexToByteArray "aabb"
          (ac1 /\ ac2) <- prepTestTokens
          expectScriptError $
            openPoolSneaky
              ( regularOpen
                  { actuallyMint = Just \lcs _poolId -> Value.singleton lcs wrongId (BigInt.fromInt 100)
                  , redeemer = Just $ \_poolId -> Redeemer $ List [ toData wrongId, Constr zero [] ]
                  }
              )
              protocol
              ac1
              ac2
              (BigInt.fromInt 100)
              (BigInt.fromInt 100)

        it "Fails to validate with the right redeemer" $ useRunnerSimple $ do
          protocol <- initProtocol
          wrongId <- liftContractM "bad hex string" $ mkTokenName =<< hexToByteArray "aabb"
          (ac1 /\ ac2) <- prepTestTokens
          expectScriptError $
            openPoolSneaky
              ( regularOpen
                  { actuallyMint = Just \lcs _poolId -> Value.singleton lcs wrongId (BigInt.fromInt 100)
                  }
              )
              protocol
              ac1
              ac2
              (BigInt.fromInt 100)
              (BigInt.fromInt 100)

      it "Fails to validate minting multiple token names on pool open" $ useRunnerSimple $ do
        protocol <- initProtocol
        wrongId <- liftContractM "bad hex string" $ mkTokenName =<< hexToByteArray "aabb"
        (ac1 /\ ac2) <- prepTestTokens
        expectScriptError $
          openPoolSneaky
            ( regularOpen
                { actuallyMint = Just \lcs poolId ->
                    Value.singleton lcs wrongId (BigInt.fromInt 100)
                      <> Value.singleton lcs poolId (BigInt.fromInt 100)
                }
            )
            protocol
            ac1
            ac2
            (BigInt.fromInt 100)
            (BigInt.fromInt 100)

      it "Allows Liquidity minting when spending pool" $ useRunnerSimple $ do
        protocol <- initProtocol
        (ac1 /\ ac2) <- prepTestTokens
        poolId <- openPool
          protocol
          ac1
          ac2
          (BigInt.fromInt 100)
          (BigInt.fromInt 100)
        depositLiquidity protocol poolId

      describe "Minting liquidity token for a pool other than the pool being spent" $ do
        it "Fails to validate with the right redeemer" $ useRunnerSimple $ do
          protocol <- initProtocol
          (ac1 /\ ac2) <- prepTestTokens
          poolId <- openPool
            protocol
            ac1
            ac2
            (BigInt.fromInt 100)
            (BigInt.fromInt 100)
          wrongId <- liftContractM "bad hex string" $ mkTokenName =<< hexToByteArray "aabb"
          expectScriptError $
            depositLiquiditySneaky
              regularDeposit
                { actuallyMint = Just \liquidityCs ->
                    Value.singleton liquidityCs wrongId $ BigInt.fromInt 10
                }
              protocol
              poolId

        it "Fails to validate with the wrong redeemer" $ useRunnerSimple $ do
          protocol <- initProtocol
          (ac1 /\ ac2) <- prepTestTokens
          poolId <- openPool
            protocol
            ac1
            ac2
            (BigInt.fromInt 100)
            (BigInt.fromInt 100)
          wrongId <- liftContractM "bad hex string" $ mkTokenName =<< hexToByteArray "aabb"
          expectScriptError $
            depositLiquiditySneaky
              regularDeposit
                { redeemer = Just $ Redeemer $ List [ toData wrongId, Constr zero [] ]
                , actuallyMint = Just \liquidityCs ->
                    Value.singleton liquidityCs wrongId $ BigInt.fromInt 10
                }
              protocol
              poolId

    describe "Protocol Initialization" $ do
      -- @Todo implement https://github.com/ArdanaLabs/Danaswap/issues/16
      it "Init protocol doesn't error" $ useRunnerSimple $ do
        initProtocol
    describe "NFT" $ maybePar $ do

      it "Mints an NFT with the seed UTxO as an input" $ useRunnerSimple do
        mintNft

      it "Cannot mint with the seed UTxO as a reference input" $ useRunnerSimple do
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
        expectScriptError $ buildBalanceSignAndSubmitTx lookups constraints

      it "Double minting fails on second mint" $ useRunnerSimple do
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
        -- It's fine and expected that this is not a script error
        expectError $ buildBalanceSignAndSubmitTx lookups constraints

      it "Spends the seed UTxO after minting" $ useRunnerSimple do
        txOut <- seedTx
        adr <- liftContractM "No wallet" =<< getWalletAddress
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

      it "Sends the NFT to the wallet after minting" $ useRunnerSimple do
        cs <- mintNft
        bal <- liftContractM "no ballance" =<< getWalletBalance
        let nfts = Value.valueOf bal cs adaToken
        nfts `shouldEqual` (BigInt.fromInt 1)

      it "Cannot burn an NFT" $ useRunnerSimple do
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

          burnConstraints :: TxConstraints Unit Unit
          burnConstraints =
            Constraints.mustSpendPubKeyOutput txOut
              <> Constraints.mustMintValue (Value.singleton cs adaToken (BigInt.fromInt $ -1))
        expectError
          $ buildBalanceSignAndSubmitTx burnLookups burnConstraints

