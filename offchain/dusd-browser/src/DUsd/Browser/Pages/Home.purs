module DUsd.Browser.Pages.Home where

import Contract.Prelude

import Ctl.Internal.Wallet (WalletExtension(..))
import DUsd.Browser.AppM (runAppM)
import DUsd.Browser.Capability.CardanoApi (class CardanoApi, availableWallets, enable)
import DUsd.Browser.Constant (currentWalletKey)
import DUsd.Browser.Types.AvailableWallets as AvailableWallets
import DUsd.Browser.Types.CurrentWallet as CurrentWallet
import DUsd.Browser.Utils (stringToWallet, walletToString)
import Effect (Effect)
import Effect.Aff (error, message, throwError)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (role)
import Halogen.Svg.Attributes (href)
import Halogen.Svg.Elements as SE
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

main :: Effect Unit
main = HA.runHalogenAff do
  _ <- HA.awaitLoad
  mbHome <- HA.selectElement $ QuerySelector "#home"
  case mbHome of
    Nothing -> throwError $ error "`#home` element to found. Dusd component unable to mount."
    Just elem -> do
      let
        store =
          { contractConfig: Nothing
          }
      homeComponent <- runAppM store component
      runUI homeComponent unit elem

type State =
  { currentWallet :: CurrentWallet.State
  , availableWallets :: AvailableWallets.State
  }

data Action
  = LoadAvailableWallets
  | EnableWallet WalletExtension

component
  :: forall q i o m
   . MonadAff m
  => CardanoApi m
  => H.Component q i o m
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
  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    LoadAvailableWallets -> do
      H.modify_ _ { availableWallets = AvailableWallets.Loading }
      availableWallets >>= case _ of
        [] -> H.modify_ _ { availableWallets = AvailableWallets.Failure $ error "No wallets found, please install Nami or Eternl wallet extension." }
        wallets -> do
          (H.liftEffect $ getItem currentWalletKey =<< localStorage =<< window) >>= case _ of
            Just str -> do
              wallet <- H.liftEffect $ stringToWallet str
              H.modify_ _ { currentWallet = CurrentWallet.Success wallet, availableWallets = AvailableWallets.Success wallets }
            _ -> H.modify_ _ { availableWallets = AvailableWallets.Success wallets }

    EnableWallet wallet -> do
      H.modify_ _ { currentWallet = CurrentWallet.Enabling }
      enable wallet >>= case _ of
        Left err -> H.modify_ _ { currentWallet = CurrentWallet.Failure err }
        Right _ -> do
          walletStr <- H.liftEffect $ walletToString wallet
          H.liftEffect $ setItem currentWalletKey walletStr =<< localStorage =<< window
          H.modify_ _ { currentWallet = CurrentWallet.Success wallet }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { currentWallet, availableWallets } =
    HH.section
      [ mkClass "section my-6 pb-6" ]
      case availableWallets of
        AvailableWallets.Loading -> [ HH.text "Loading available wallets ..." ]
        AvailableWallets.Failure err ->
          [ HH.div [ mkClass "level" ]
              [ HH.div [ mkClass "level-item has-text-danger" ]
                  [ HH.text $ message err
                  ]
              ]
          , HH.button [ mkClass "button has-border is-large is-responsive is-rounded", HE.onClick $ const LoadAvailableWallets ]
              [ HH.text "Retry" ]
          ]
        AvailableWallets.Success wallets -> case currentWallet of
          CurrentWallet.Success _ ->
            [ HH.text "Enabled"
            ]
          _ ->
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
            , connectWallet currentWallet wallets
            ]

connectWallet :: forall slots m. CurrentWallet.State -> Array WalletExtension -> H.ComponentHTML Action slots m
connectWallet currentWallet wallets = HH.div [ mkClass "level" ]
  [ HH.div [ mkClass "level-item" ]
      [ HH.div [ mkClass "connect-wallet" ]
          case currentWallet of
            CurrentWallet.NotAsked ->
              [ HH.div [ mkClass "level" ]
                  [ HH.div [ mkClass "level-item" ]
                      [ HH.p [ mkClass "title" ]
                          [ HH.text "Connect Wallet" ]
                      ]
                  ]
              ] <> availableWallets
            CurrentWallet.Failure err ->
              [ HH.div [ mkClass "level" ]
                  [ HH.div [ mkClass "level-item has-text-danger" ]
                      [ HH.text $ message err ]
                  ]
              ] <> availableWallets
            CurrentWallet.Enabling ->
              [ HH.div [ mkClass "level" ]
                  [ HH.div [ mkClass "level-item has-text-centered" ]
                      [ HH.progress [ mkClass "progress is-info" ] []
                      ]
                  ]
              ]
            CurrentWallet.Success _ -> []
      ]
  ]
  where
  availableWallets = wallets
    <#> \wallet -> HH.div [ mkClass "level" ]
      [ HH.div [ mkClass "level-item" ]
          [ HH.button [ mkClass "button is-rounded is-large is-fullwidth has-border", HE.onClick $ const (EnableWallet wallet) ]
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

mkImage :: forall slots m a. String -> H.ComponentHTML a slots m
mkImage image = HH.figure [ mkClass "image" ]
  [ HH.element (HH.ElemName "picture") []
      [ HH.source [ HP.attr (HH.AttrName "srcset") (image <> ".jxl"), HP.attr (HH.AttrName "type") "image/jxl" ]
      , HH.source [ HP.attr (HH.AttrName "srcset") (image <> ".webp"), HP.attr (HH.AttrName "type") "image/webp" ]
      , HH.img
          [ HP.src $ image <> ".png"
          , HP.alt ""
          , HP.attr (HH.AttrName "loading") "lazy"
          , HP.attr (HH.AttrName "decoding") "async"
          ]
      ]
  ]

mkClass :: forall (r :: Row Type) (i :: Type). String -> HP.IProp (class :: String | r) i
mkClass = HP.class_ <<< HH.ClassName

mkIcon :: forall slots m a. String -> H.ComponentHTML a slots m
mkIcon icon = HH.span
  [ mkClass "icon" ]
  [ HH.a [ HP.title icon ]
      [ SE.svg [ role "none" ]
          [ SE.use [ href ("/assets/images/font-awesome-sprite-solid.svg#" <> icon) ]
          ]
      ]
  ]
