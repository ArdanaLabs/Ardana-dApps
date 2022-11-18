module DUsd.Browser.Components.ListAllVaults where

import Contract.Prelude

import DUsd.Browser.Capability.DUsdApi (class DUsdApi, loadVaults)
import DUsd.Browser.Types.RoutingMessage (RoutingMessage)
import DUsd.Browser.Types.RoutingMessage as RoutingMessage
import DUsd.Browser.Types.Vault (collateralType)
import DUsd.Browser.Types.Vaults as Vaults
import DUsd.Browser.Utils (mkClass)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (message)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State = Vaults.State

data Action
  = LoadVaults
  | ClickNewVault

component
  :: forall q i m
   . MonadAff m
  => DUsdApi m
  => H.Component q i RoutingMessage m
component =
  H.mkComponent
    { initialState: const Vaults.Loading
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just LoadVaults
        }
    }
  where
  handleAction :: forall slots. Action -> H.HalogenM State Action slots RoutingMessage m Unit
  handleAction = case _ of
    LoadVaults -> loadVaults >>= case _ of
      Left err -> H.modify_ $ const (Vaults.Failure err)
      Right vaults -> H.modify_ $ const (Vaults.Success vaults)
    ClickNewVault -> H.raise RoutingMessage.CreateNewVault

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render = case _ of
    Vaults.Loading -> HH.section [ mkClass "section m-6 p-6" ]
      [ renderInformationBoard
      , HH.div [ mkClass "level" ]
          [ HH.div [ mkClass "level-item" ]
              [ HH.progress [ mkClass "progress is-info" ] []
              ]
          ]
      ]

    Vaults.Failure err -> HH.section [ mkClass "section m-6 p-6" ]
      [ renderInformationBoard
      , HH.div [ mkClass "level" ]
          [ HH.div [ mkClass "level-item has-text-danger" ]
              [ HH.text $ message err ]
          ]
      , HH.div [ mkClass "level" ]
          [ HH.div [ mkClass "level-item " ]
              [ HH.button [ mkClass "button has-border is-large is-fullwidth is-rounded", HE.onClick $ const LoadVaults ]
                  [ HH.text "Retry" ]
              ]
          ]
      ]
    Vaults.Success vaults' ->
      HH.section [ mkClass "section m-6 p-6" ]
        $
          [ renderInformationBoard
          , HH.div [ mkClass "columns is-mobile mt-6 pt-6" ]
              [ HH.div [ mkClass "column" ]
                  [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Collateral Type" ] ]
              , HH.div [ mkClass "column" ]
                  [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Liquidation Price" ] ]
              , HH.div [ mkClass "column" ]
                  [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Collateral" ] ]
              , HH.div [ mkClass "column" ]
                  [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Debt" ] ]
              , HH.div [ mkClass "column" ] []
              ]
          ]
        <> (renderVault <$> vaults')
    where
    renderInformationBoard = HH.div [ mkClass "level is-mobile mb-6" ]
      [ HH.div [ mkClass "level-left" ]
          [ HH.div [ mkClass "level-item" ]
              [ HH.div [ mkClass "columns is-multiline" ]
                  [ HH.div [ mkClass "column is-full" ]
                      [ HH.text "Current ADA Price "
                      , HH.span [ mkClass "title is-size-6" ] [ HH.text "$0.31" ]
                      ]
                  , HH.div [ mkClass "column is-full" ]
                      [ HH.text "Next ADA Price "
                      , HH.span [ mkClass "title is-size-6" ] [ HH.text "$0.33" ]
                      ]
                  ]
              ]
          ]
      , HH.div [ mkClass "level-right" ]
          [ HH.div [ mkClass "level-item" ]
              [ HH.button [ mkClass "button has-background is-rounded is-large is-fullwidth is-responsive is-uppercase", HE.onClick $ const ClickNewVault ]
                  [ HH.text "create vault" ]
              ]
          ]
      ]

    renderVault vault = HH.div [ mkClass "vault p-3 my-3" ]
      [ HH.div [ mkClass "columns is-mobile is-vcentered" ]
          [ HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch" ] [ HH.text $ collateralType vault ]
              ]
          , HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch" ] [ HH.text $ "$" <> show (unwrap vault.liquidationPrice) ]
              ]
          , HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch" ] [ HH.text $ show vault.collateral ]
              ]
          , HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch" ] [ HH.text $ show vault.debt ]
              ]
          , HH.div [ mkClass "column has-text-right" ]
              [ HH.button [ mkClass "button has-border is-large is-rounded is-responsive" ]
                  [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch is-uppercase" ]
                      [ HH.text "manage vault" ]
                  ]
              ]
          ]
      ]
