module DanaSwap.Api
  ( initProtocol
  , mintNft
  , seedTx
  ) where

import Contract.Prelude

import Contract.Address (getWalletAddress, getWalletCollateral, scriptHashAddress)
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), PlutusData(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, Validator, mintingPolicyHash, validatorHash)
import Contract.Transaction (TransactionInput)
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol, adaToken, mpsSymbol, scriptCurrencySymbol)
import Contract.Value as Value
import Ctl.Utils (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import DanaSwap.CborTyped (configAddressValidator, liqudityTokenMintingPolicy, poolAddressValidator, poolIdTokenMintingPolicy, simpleNft)
import Data.BigInt as BigInt
import Data.List (head)
import Data.Map (keys)
import Data.Map as Map
import Data.Set (toUnfoldable)

type Protocol =
  { configUtxo :: TransactionInput
  , poolVal :: Validator
  , liquidityMP :: MintingPolicy
  , poolIdMP :: MintingPolicy
  }

initProtocol :: Contract () Protocol
initProtocol = do
  logDebug' "starting protocol init"
  nftCs <- mintNft
  logDebug' "nft minted"
  logDebug' $ "currency symbol:" <> show nftCs
  poolIdMP <- poolIdTokenMintingPolicy nftCs
  poolIdCS <- liftContractM "invalid hex string from mintingPolicyHash"
    $ mpsSymbol
    $ mintingPolicyHash poolIdMP
  liquidityMP <- liqudityTokenMintingPolicy poolIdCS
  liquidityCS <- liftContractM "invalid hex string from mintingPolicyHash"
    $ mpsSymbol
    $ mintingPolicyHash liquidityMP
  poolVal <- poolAddressValidator poolIdCS liquidityCS
  let poolVH = validatorHash poolVal
  let poolAdr = scriptHashAddress poolVH
  configAdrVal <- configAddressValidator
  logDebug' "about to submit config utxo"
  txid <- buildBalanceSignAndSubmitTx
    (mempty)
    ( Constraints.mustPayToScript
        (validatorHash configAdrVal)
        (Datum $ Constr zero [ toData poolAdr, toData liquidityCS ])
        -- This could have a data type with a ToData instance
        -- but it only gets used once so it seems unnesecary
        DatumInline
        (Value.singleton nftCs adaToken one)
    )
  logDebug' "config utxo submitted, waiting for confirmation"
  configUtxo <- waitForTx (scriptHashAddress $ validatorHash configAdrVal) txid
  logDebug' "protocol init complete"
  pure
    { configUtxo
    , poolVal
    , liquidityMP
    , poolIdMP
    }

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
