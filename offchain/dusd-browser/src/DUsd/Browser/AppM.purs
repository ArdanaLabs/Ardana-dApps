module DUsd.Browser.AppM where

import Contract.Prelude

import Contract.Config (NetworkId(..), WalletSpec(..), mainnetConfig, mainnetNamiConfig, testnetEternlConfig, testnetNamiConfig)
import Control.Monad.Writer (execWriterT, lift, tell)
import Ctl.Internal.Wallet (WalletExtension(..), isWalletAvailable)
import DUsd.Browser.Capability.CardanoApi (class CardanoApi)
import DUsd.Browser.FFI.Cardano as Cardano
import DUsd.Browser.Store as S
import Effect.Aff (Aff, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (error)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runStoreT, updateStore)
import Safe.Coerce (coerce)

newtype AppM a = AppM (StoreT S.Action S.Store Aff a)

runAppM :: forall q i o. S.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store S.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore S.Action S.Store AppM

instance cardanoApiAppM :: CardanoApi AppM where
  availableWallets = liftEffect $ execWriterT do
    lift (isWalletAvailable NamiWallet) >>= case _ of
      true -> tell [ NamiWallet ]
      false -> pure unit
    lift (isWalletAvailable EternlWallet) >>= case _ of
      true -> tell [ EternlWallet ]
      false -> pure unit
  enable = case _ of
    NamiWallet -> go "nami" (mainnetNamiConfig { logLevel = Warn }) (testnetNamiConfig { logLevel = Warn })
    EternlWallet -> go "eternl" (mainnetConfig { walletSpec = Just ConnectToEternl }) { logLevel = Warn } testnetEternlConfig { logLevel = Warn }
    _ -> pure $ Left $ error "wallet not supported"
    where
    go wallet mainnetConfig testnetConfig = do
      result <- liftAff $ try (Cardano.enable wallet)
      case result of
        Right conn -> do
          result' <- liftAff $ try (Cardano.getNetworkId conn)
          case result' of
            Left _ -> pure $ Left $ error "failed to fetch network ID"
            Right networkId -> do
              let
                contractConfig' = case networkId of
                  MainnetId -> mainnetConfig
                  TestnetId -> testnetConfig
              updateStore $ S.SetContractConfig contractConfig'
              pure $ Right unit
        Left error -> pure $ Left error