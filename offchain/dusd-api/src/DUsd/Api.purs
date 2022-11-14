module DUsd.Api
  ( initProtocolSimple
  , updateProtocol
  -- Types
  , Protocol(..)
  -- Testing
  , getWalletPubkeyhash
  , mintNft
  , seedTx
  ) where

import Contract.Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson')
import Contract.Address (PubKeyHash, getWalletAddress, getWalletCollateral, scriptHashAddress)
import Contract.Credential (Credential(..))
import Contract.Log (logDebug', logInfo')
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Datum(..), OutputDatum(..), PlutusData(..), Redeemer(..), toData)
import Contract.Prim.ByteArray (byteArrayToHex, hexToByteArrayUnsafe)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator, validatorHash)
import Contract.Transaction (TransactionHash(..), TransactionInput(..), TransactionOutput(..), TransactionOutputWithRefScript(..))
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol, adaToken, scriptCurrencySymbol)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Address (Address(..))
import Ctl.Utils (buildBalanceSignAndSubmitTx, getUtxos, waitForTx)
import Data.Array (cons)
import Data.BigInt as BigInt
import Data.List (head)
import Data.Map (keys, singleton)
import Data.Map as Map
import Data.Set (toUnfoldable)
import Data.UInt as UInt
import DUsd.CborTyped (configAddressValidator, simpleNft)
import Effect.Exception (throw)

newtype Protocol = Protocol { datum :: PlutusData, utxo :: TransactionInput, nftCs :: CurrencySymbol, configVal :: Validator }

derive instance Newtype Protocol _

type ProtocolJson = { datum :: PlutusData, utxo :: TransactionInputJson, nftCs :: CurrencySymbol, configVal :: Validator }
type TransactionInputJson = { index :: Int, transactionId :: String }

jsonifyTxIn :: TransactionInput -> TransactionInputJson
jsonifyTxIn (TransactionInput { index, transactionId }) =
  { index: UInt.toInt index
  , transactionId: byteArrayToHex $ unwrap transactionId
  }

unJsonifyTxIn :: TransactionInputJson -> TransactionInput
unJsonifyTxIn { index, transactionId } = TransactionInput
  { index: UInt.fromInt index
  , transactionId: TransactionHash $ hexToByteArrayUnsafe transactionId
  }

jsonifyProtocol :: Protocol -> ProtocolJson
jsonifyProtocol (Protocol { datum, utxo, nftCs, configVal }) = { datum, utxo: jsonifyTxIn utxo, nftCs, configVal }

unJsonifyProtocol :: ProtocolJson -> Protocol
unJsonifyProtocol { datum, utxo, nftCs, configVal } = Protocol { datum, utxo: unJsonifyTxIn utxo, nftCs, configVal }

instance EncodeAeson Protocol where
  encodeAeson' = jsonifyProtocol >>> encodeAeson'

instance DecodeAeson Protocol where
  decodeAeson = (unJsonifyProtocol <$> _) <<< decodeAeson

-- | Initializes the protocol with a given datum
initProtocolSimple :: PlutusData -> Contract () Protocol
initProtocolSimple datum = do
  logDebug' "starting protocol init"
  nftCs <- mintNft
  pkh <- getWalletPubkeyhash
  configVal <- configAddressValidator pkh nftCs
  txid <- buildBalanceSignAndSubmitTx
    (mempty)
    ( Constraints.mustPayToScript
        (validatorHash configVal)
        (Datum $ List [ datum ]) -- TODO real protocol datum
        DatumInline
        (Value.singleton nftCs adaToken one)
    )
  logDebug' "config utxo submitted, waiting for confirmation"
  utxo <- waitForTx (scriptHashAddress (validatorHash configVal) Nothing) txid
  logDebug' "protocol init complete"
  pure $ Protocol { datum, utxo, nftCs, configVal }

-- | technically not part of this version of the protocol
updateProtocol :: PlutusData -> Protocol -> Contract () Protocol
updateProtocol new (Protocol { utxo: oldUtxo, nftCs, configVal }) = do
  TransactionOutput { datum } <- getUtxo oldUtxo >>= liftContractM "lookup failed. Maybe config utxo was already spent"
  old <- case datum of
    (OutputDatum (Datum (List old))) -> pure old
    _ -> liftEffect $ throw "old datum was formatted incorectly or a datum hash or missing"
  oldOut <- getUtxo oldUtxo >>= liftContractM "lookup failed"
  pkh <- getWalletPubkeyhash
  txid <- buildBalanceSignAndSubmitTx
    ( Lookups.unspentOutputs
        ( singleton oldUtxo
            (TransactionOutputWithRefScript { output: oldOut, scriptRef: Nothing })
        )
        <> Lookups.validator configVal
    )
    ( Constraints.mustSpendScriptOutput
        oldUtxo
        (Redeemer $ toData unit)
        <> Constraints.mustPayToScript
          (validatorHash configVal)
          (Datum $ List $ cons new old)
          DatumInline
          (Value.singleton nftCs adaToken one)
        <> Constraints.mustBeSignedBy (wrap pkh)
    )
  logDebug' "config utxo submitted, waiting for confirmation"
  utxo <- waitForTx (scriptHashAddress (validatorHash configVal) Nothing) txid
  logDebug' "protocol init complete"
  pure $ Protocol { utxo, datum: new, nftCs, configVal }

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

getWalletPubkeyhash :: Contract () PubKeyHash
getWalletPubkeyhash = do
  (Address { addressCredential }) <- getWalletAddress >>= liftContractM "no wallet"
  case addressCredential of
    PubKeyCredential pkh -> pure pkh
    _ -> liftEffect $ throw "wallet was not a pubkey?"

