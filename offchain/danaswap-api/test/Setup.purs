module Setup
  ( prepTestTokens
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (mintingPolicyHash)
import Contract.TxConstraints as Constraints
import Contract.Value (mkTokenName, mpsSymbol)
import Ctl.Utils (buildBalanceSignAndSubmitTx, waitForTx)
import DanaSwap.Api (AssetClass)
import DanaSwap.CborTyped (testToken)
import Data.BigInt as BigInt

-- | Mints 1_000_000 each of two asset classes of tokens
-- leaving them in the current wallet
-- usefull for making sure the wallet has a few distinct tokens
-- to put in a pool
prepTestTokens :: Contract () (AssetClass /\ AssetClass)
prepTestTokens = do
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
        (BigInt.fromInt 1_000_000_000)
        <> Constraints.mustMintCurrency
          (mintingPolicyHash testMp2)
          testTn2
          (BigInt.fromInt 1_000_000_000)
    )
  pure $ (testCs1 /\ testTn1) /\ (testCs2 /\ testTn2)

