module DUsd.Browser.Components.ConnectWallet where

import Contract.Prelude

import Ctl.Internal.Wallet (WalletExtension(..), isEnabled)
import DUsd.Browser.Capability.CardanoApi (class CardanoApi, availableWallets, enable)
import DUsd.Browser.Constant (currentWalletKey)
import DUsd.Browser.Types.AvailableWallets as AvailableWallets
import DUsd.Browser.Types.CurrentWallet as CurrentWallet
import DUsd.Browser.Types.RoutingMessage (RoutingMessage)
import DUsd.Browser.Types.RoutingMessage as RoutingMessage
import DUsd.Browser.Utils (mkClass, mkImage, stringToWallet, walletToString)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (error, message)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

type State =
  { currentWallet :: CurrentWallet.State
  , availableWallets :: AvailableWallets.State
  }

data Action
  = LoadAvailableWallets
  | EnableWallet WalletExtension

component
  :: forall q i m
   . MonadAff m
  => CardanoApi m
  => H.Component q i RoutingMessage m
component =
  H.mkComponent
    { initialState: const
        { currentWallet: CurrentWallet.NotAsked
        , availableWallets: AvailableWallets.Loading
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just LoadAvailableWallets
        }
    }
  where
  handleAction :: forall slots. Action -> H.HalogenM State Action slots RoutingMessage m Unit
  handleAction = case _ of
    LoadAvailableWallets -> do
      H.modify_ _ { availableWallets = AvailableWallets.Loading }
      availableWallets >>= case _ of
        [] -> H.modify_ _ { availableWallets = AvailableWallets.Failure $ error "No wallets found, please install Nami or Eternl wallet extension." }
        wallets -> do
          (H.liftEffect $ getItem currentWalletKey =<< localStorage =<< window) >>= case _ of
            Just str -> do
              wallet <- H.liftEffect $ stringToWallet str
              isWalletEnabled <- H.liftAff $ isEnabled wallet
              if isWalletEnabled then
                H.raise RoutingMessage.ListAllVaults
              else
                H.modify_ _ { availableWallets = AvailableWallets.Success wallets }
            Nothing -> H.modify_ _ { availableWallets = AvailableWallets.Success wallets }

    EnableWallet wallet -> do
      H.modify_ _ { currentWallet = CurrentWallet.Enabling }
      enable wallet >>= case _ of
        Left err -> H.modify_ _ { currentWallet = CurrentWallet.Failure err }
        Right _ -> do
          str <- H.liftEffect $ walletToString wallet
          H.liftEffect $ setItem currentWalletKey str =<< localStorage =<< window
          H.raise RoutingMessage.ListAllVaults

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentWallet, availableWallets } =
    case availableWallets of
      AvailableWallets.Loading -> HH.section [ mkClass "section m-6 p-6" ]
        [ HH.text "Loading available wallets ..." ]
      AvailableWallets.Failure err -> HH.section [ mkClass "section m-6 p-6" ]
        [ HH.div [ mkClass "level" ]
            [ HH.div [ mkClass "level-item has-text-danger" ]
                [ HH.text $ message err
                ]
            ]
        , HH.button [ mkClass "button has-border is-large is-fullwidth is-responsive is-rounded", HE.onClick $ const LoadAvailableWallets ]
            [ HH.text "Retry" ]
        ]
      AvailableWallets.Success wallets -> HH.section [ mkClass "section m-6 p-6" ]
        [ HH.div [ mkClass "level" ]
            [ HH.div [ mkClass "level-item" ]
                [ HH.p [ mkClass "title" ]
                    [ HH.text "Collateral assets can be leveraged to mint dUSD, the Ardana Stablecoin." ]
                ]
            ]
        , HH.div [ mkClass "level" ]
            [ HH.div [ mkClass "level-item" ]
                [ HH.p [ mkClass "subtitle" ]
                    [ HH.text "Open a dUSD vault, deposit your collateral, and generate dUSD against it." ]
                ]
            ]
        , HH.div [ mkClass "level" ]
            [ HH.div [ mkClass "level-item" ]
                [ HH.div [ mkClass "card" ]
                    case currentWallet of
                      CurrentWallet.NotAsked ->
                        [ HH.div [ mkClass "level" ]
                            [ HH.div [ mkClass "level-item" ]
                                [ HH.p [ mkClass "title" ]
                                    [ HH.text "Connect Wallet" ]
                                ]
                            ]
                        ] <> (renderWallet <$> wallets)
                      CurrentWallet.Failure err ->
                        [ HH.div [ mkClass "level" ]
                            [ HH.div [ mkClass "level-item has-text-danger" ]
                                [ HH.text $ message err ]
                            ]
                        ] <> (renderWallet <$> wallets)
                      CurrentWallet.Enabling ->
                        [ HH.div [ mkClass "level" ]
                            [ HH.div [ mkClass "level-item" ]
                                [ HH.progress [ mkClass "progress is-info" ] []
                                ]
                            ]
                        ]
                ]
            ]
        ]

    where
    renderWallet wallet = HH.div [ mkClass "level" ]
      [ HH.div [ mkClass "level-item" ]
          [ HH.button [ mkClass "button has-border is-rounded is-large is-fullwidth", HE.onClick $ const (EnableWallet wallet) ]
              case wallet of
                NamiWallet ->
                  [ HH.span [ mkClass "icon" ] [ mkImage "/assets/images/Nami" ]
                  , HH.span [] [ HH.text "Nami" ]
                  ]
                EternlWallet ->
                  [ HH.span [ mkClass "icon" ] [ mkImage "/assets/images/Eternl" ]
                  , HH.span [] [ HH.text "Eternl" ]
                  ]
                _ -> [ HH.text "Unknown" ]

          ]
      ]