module DUsd.Nft
  ( mintNft
  , seedTx
  , lookupUtxo
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral, scriptHashAddress)
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (PlutusData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (TransactionInput, TransactionOutput)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol, adaToken, scriptCurrencySymbol, valueOf)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import DUsd.CborTyped (simpleNft)
import DUsd.Types (UtxoId(..))
import Data.BigInt as BigInt
import Data.List (head)
import Data.Map (keys)
import Data.Map as Map
import Data.Set (toUnfoldable)
import Effect.Exception (throw)

-- | lookup a utxo by id
lookupUtxo :: UtxoId -> Contract () (TransactionInput /\ TransactionOutput)
lookupUtxo (UtxoId { nft: (cs /\ tn), script, guess }) =
  case guess of
    Just txin -> do
      getUtxo txin >>=
        case _ of
          Just txout -> pure $ txin /\ txout
          Nothing -> fallback
    Nothing -> fallback
  where
  fallback =
    getUtxos (scriptHashAddress (validatorHash script) Nothing)
      <#> Map.filter
        ( \utxo ->
            valueOf (unwrap (unwrap utxo).output).amount cs tn
              > BigInt.fromInt 0
        )
      >>= Map.toUnfoldableUnordered
      >>> case _ of
        [ txin /\ txout ] -> pure $ txin /\ (unwrap txout).output
        [] -> liftEffect $ throw "utxo lookup failed no valid results"
        _ -> liftEffect $ throw "utxo lookup failed ambiguous"

-- | Mints an nft where the txid is a parameter of the contract and returns the currency symbol
mintNft :: Contract () CurrencySymbol
mintNft = do
  logInfo' "starting mint"
  txOut <- seedTx
  adr <- liftContractM "no wallet" =<< getWalletAddress
  utxos <- getUtxos adr
  logDebug' $ "seed tx id was:" <> show txOut
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
  logDebug' "about to submit"
  txId <- buildBalanceSignAndSubmitTx lookups constraints
  logDebug' "submitted"
  _ <- waitForTx adr txId
  pure $ cs

-- | Selects a utxo owned by the current wallet usefull for minting nfts
seedTx :: Contract () TransactionInput
seedTx = do
  adr <- liftContractM "no wallet" =<< getWalletAddress
  utxos <- getUtxos adr
  logInfo' $ show adr
  logInfo' $ "utxos: " <> show utxos
  col <- fromMaybe [] <$> getWalletCollateral
  logInfo' $ "col: " <> show col
  let colIns = (unwrap >>> _.input) <$> col
  logInfo' $ "colIns: " <> show colIns
  let nonColateralUtxOs = Map.filterKeys (\utxo -> utxo `notElem` colIns) utxos
  logInfo' $ "nonColUtxos: " <> show nonColateralUtxOs
  sending <- case head $ toUnfoldable $ keys nonColateralUtxOs of
    Just sending -> pure sending
    Nothing -> do
      logInfo' "all utxos were collateral using collateral utxo"
      liftContractM "no utxos at all" $ head $ toUnfoldable $ keys utxos
  logInfo' $ "sending: " <> show sending
  out <- liftContractM "no output" =<< getUtxo sending
  logInfo' $ "out: " <> show out
  pure sending

