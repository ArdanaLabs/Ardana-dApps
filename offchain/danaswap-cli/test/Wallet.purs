module Test.Wallet
  ( withPlutipWalletFile
  , withFundedHsmWalletFile
  , withTmpDir
  ) where

import Contract.Prelude

import Aeson (encodeAeson)
import Contract.Address (PaymentPubKeyHash(..), StakePubKeyHash(..))
import Contract.Credential (Credential(..), StakingCredential(..))
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, runContractInEnv, ContractEnv)
import Contract.PlutusData (PlutusData)
import Contract.ScriptLookups as Lookups
import Contract.Test.Plutip (PlutipConfig, withPlutipContractEnv)
import Contract.TxConstraints (TxConstraint(..), TxConstraints, singleton)
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Contract.Value (lovelaceValueOf)
import Contract.Wallet (KeyWallet, getWalletAddress, withKeyWallet)
import Contract.Wallet.KeyFile (privatePaymentKeyToFile, privateStakeKeyToFile)
import Ctl.Internal.Wallet.Key (keyWalletPrivatePaymentKey, keyWalletPrivateStakeKey)
import Ctl.Utils (buildBalanceSignAndSubmitTx, waitForTx)
import Ctl.Utils.HsmWallet (makeHsmWallet)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.String (trim)
import Data.UInt as UInt
import Effect.Aff (bracket)
import Effect.Exception (throw)
import Node.Buffer (toString)
import Node.ChildProcess (execFileSync, defaultExecSyncOptions)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (rmdir, writeTextFile)
import Node.Path (FilePath, normalize)

-- | Utilizes "mktemp" to create a temporary directory, executes the action and deletes the temporary directory afterwards
withTmpDir :: forall a. (FilePath -> Aff a) -> Aff a
withTmpDir =
  bracket
    (trim <$> (liftEffect $ toString UTF8 =<< execFileSync "mktemp" [ "-d" ] defaultExecSyncOptions))
    rmdir

-- TODO: The Ctl.Internal.Wallet.Key seems unavoidable

withFundedHsmWalletFile :: forall a. PlutipConfig -> Array BigInt -> FilePath -> (String -> String -> Aff a) -> Aff a
withFundedHsmWalletFile config vals walletDir f = withPlutipContractEnv config vals \env wallet -> do
  runContractInEnv env $ do
    logInfo' "starting wallet funding phas"
    hsmWallet <- liftAff $ makeHsmWallet
    adr <- withKeyWallet hsmWallet getWalletAddress >>= liftContractM "no wallet"
    (key /\ skey) <- liftContractM "bad adr" =<< case unwrap adr of
      { addressCredential: PubKeyCredential key, addressStakingCredential: mskey } -> do
        skey <- case mskey of
          Nothing -> pure Nothing
          Just (StakingHash (PubKeyCredential skey)) -> pure $ Just skey
          _ -> liftEffect $ throw "bad staking credential"
        pure $ Just $ key /\ skey
      _ -> pure Nothing

    let
      lookups :: Lookups.ScriptLookups PlutusData
      lookups = mempty

      constraints :: TxConstraints Unit Unit
      constraints = singleton $
        MustPayToPubKeyAddress
          (PaymentPubKeyHash key)
          (StakePubKeyHash <$> skey)
          Nothing
          Nothing
          (lovelaceValueOf $ BigInt.fromInt 30_000_000)
    txid <- withKeyWallet wallet $ buildBalanceSignAndSubmitTx lookups constraints
    logInfo' "wallet funding finished"
    res <- waitForTx adr txid
    logInfo' $ "res: " <> show res
    logInfo' $ "hsm wallet adr: " <> show adr
    bal <- withKeyWallet hsmWallet $ getWalletBalance >>= liftContractM "no wallet"
    logInfo' $ "post funding bal: " <> show bal
    txs <- withKeyWallet hsmWallet $ getWalletUtxos >>= liftContractM "no wallet"
    logInfo' $ "hsm wallet txs: " <> show txs
  let
    portArgs = getPortArgs env
    walletPath = normalize $ walletDir <> "/hsmWalletCfg.json"
  writeTextFile UTF8 walletPath $ encodeAeson >>> show $
    { useYubiHSM: true }
  f portArgs (" " <> walletPath <> " ")

-- | Extracts the configured CTL runtime ports and greates an argstring
getPortArgs :: ContractEnv () -> String
getPortArgs env =
  fromMaybe "" ((" --ctl-port " <> _) <<< show <<< UInt.toInt <$> ((unwrap env).config.ctlServerConfig <#> _.port))
    <> " --ogmios-port "
    <> show (UInt.toInt (unwrap env).config.ogmiosConfig.port)
    <> " --odc-port "
    <> show (UInt.toInt (unwrap env).config.datumCacheConfig.port)
    <> " "

withPlutipWalletFile :: forall a. PlutipConfig -> Array BigInt -> (String -> String -> Aff a) -> Aff a
withPlutipWalletFile config vals f = withPlutipContractEnv config vals \env wallet -> do
  withTmpDir $ \tmpDir -> do
    w <- makeWalletConfig tmpDir "plutip" wallet
    let portArgs = getPortArgs env
    f portArgs (" " <> w <> " ")

-- | Creates a wallet config file in the given directory and name for a KeyWallet
makeWalletConfig :: FilePath -> String -> KeyWallet -> Aff FilePath
makeWalletConfig dir name wallet = do
  let cfgName = normalize $ dir <> "/" <> name <> "-cfg.json"
  let walletSkey = normalize $ dir <> "/" <> name <> "-wallet.skey"
  let stakeSkey = normalize $ dir <> "/" <> name <> "-staking.skey"
  writeTextFile UTF8 cfgName $ encodeAeson >>> show $
    { walletPath: walletSkey
    , stakingPath: walletSkey <$ keyWalletPrivateStakeKey wallet
    }
  privatePaymentKeyToFile walletSkey $ keyWalletPrivatePaymentKey wallet
  void $ traverse (privateStakeKeyToFile stakeSkey) $ keyWalletPrivateStakeKey wallet
  pure cfgName

