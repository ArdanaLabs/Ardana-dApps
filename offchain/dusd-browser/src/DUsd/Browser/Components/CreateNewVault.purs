module DUsd.Browser.Components.CreateNewVault where

import Contract.Prelude

import DUsd.Browser.Capability.DUsdApi (class DUsdApi, createVault)
import DUsd.Browser.Types.NewVault (NewVault(..))
import DUsd.Browser.Types.NewVault as NewVault
import DUsd.Browser.Types.RoutingMessage (RoutingMessage)
import DUsd.Browser.Types.RoutingMessage as RoutingMessage
import DUsd.Browser.Utils (mkClass)
import Data.Int (fromString)
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (message)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP

type State =
  { newVault :: NewVault.State
  , adaAmount :: Int
  }

data Action
  = ClickListAllVaults
  | ClickCreateVault
  | UpdateAdaAmount String

component
  :: forall q i m
   . MonadAff m
  => DUsdApi m
  => H.Component q i RoutingMessage m
component =
  H.mkComponent
    { initialState: const
        { newVault: NewVault.NotAsked
        , adaAmount: 0
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  handleAction :: forall slots. Action -> H.HalogenM State Action slots RoutingMessage m Unit
  handleAction = case _ of
    ClickCreateVault -> do
      H.modify_ _ { newVault = NewVault.Loading }
      { adaAmount } <- H.get
      createVault (Ada adaAmount) >>= case _ of
        Left err -> H.modify_ _ { newVault = NewVault.Failure err }
        Right _ -> H.raise RoutingMessage.ListAllVaults
    ClickListAllVaults -> H.raise RoutingMessage.ListAllVaults
    UpdateAdaAmount str -> do
      case fromString str of
        Nothing -> H.modify_ _ { adaAmount = 0 }
        Just amt -> H.modify_ _ { adaAmount = amt }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { newVault, adaAmount } =
    HH.section [ mkClass "section m-6 p-6" ]
      [ renderInformationBoard
      , HH.div [ mkClass "level" ]
          [ HH.div [ mkClass "level-item" ]
              [ HH.div [ mkClass "card" ]
                  $
                    [ HH.div [ mkClass "level" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.p [ mkClass "title" ]
                                [ HH.text "Create Vault" ]
                            ]
                        ]
                    , HH.div [ mkClass "level" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.p [ mkClass "subtitle" ]
                                [ HH.text "Deposit your ADA" ]
                            ]
                        ]
                    ]
                  <> case newVault of
                    NewVault.NotAsked ->
                      [ HH.div [ mkClass "level" ]
                          [ HH.div [ mkClass "level-item" ]
                              [ HH.form []
                                  [ HH.div [ mkClass "field" ]
                                      [ HH.div [ mkClass "control" ]
                                          [ HH.input [ mkClass "input", HP.placeholder "Enter ADA", HP.type_ InputNumber, HE.onValueInput UpdateAdaAmount ]
                                          ]
                                      ]
                                  ]
                              ]
                          ]
                      , HH.div [ mkClass "level" ]
                          [ HH.div [ mkClass "level-item" ]
                              [ HH.button [ mkClass "button has-background is-rounded is-large is-responsive", HE.onClick $ const ClickCreateVault, HP.disabled $ adaAmount == 0 ]
                                  [ HH.span [ mkClass "is-uppercase" ]
                                      [ HH.text "open vault" ]
                                  ]
                              ]
                          ]
                      ]
                    NewVault.Failure err ->
                      [ HH.div [ mkClass "level" ]
                          [ HH.div [ mkClass "level-item" ]
                              [ HH.form []
                                  [ HH.div [ mkClass "field" ]
                                      [ HH.div [ mkClass "control" ]
                                          [ HH.input [ mkClass "input", HP.placeholder "Enter ADA", HP.type_ InputNumber, HE.onValueInput UpdateAdaAmount ]
                                          ]
                                      ]
                                  ]
                              ]
                          ]
                      , HH.div [ mkClass "level" ]
                          [ HH.div [ mkClass "level-item" ]
                              [ HH.button [ mkClass "button has-background is-rounded is-large is-responsive", HE.onClick $ const ClickCreateVault, HP.disabled $ adaAmount == 0 ]
                                  [ HH.span [ mkClass "is-uppercase" ]
                                      [ HH.text "open vault" ]
                                  ]
                              ]
                          ]
                      , HH.div
                          [ mkClass "level" ]
                          [ HH.div [ mkClass "level-item has-text-danger" ]
                              [ HH.text $ message err ]
                          ]
                      ]
                    NewVault.Loading ->
                      [ HH.div [ mkClass "level" ]
                          [ HH.div [ mkClass "level-item" ]
                              [ HH.form []
                                  [ HH.div [ mkClass "field" ]
                                      [ HH.div [ mkClass "control" ]
                                          [ HH.input [ mkClass "input", HP.value (show adaAmount), HP.readOnly true ]
                                          ]
                                      ]
                                  ]
                              ]
                          ]
                      , HH.div [ mkClass "level" ]
                          [ HH.div [ mkClass "level-item" ]
                              [ HH.button [ mkClass "button has-background is-rounded is-large is-responsive", HP.disabled true ]
                                  [ HH.span [ mkClass "is-uppercase" ]
                                      [ HH.text "submitting ..." ]
                                  ]
                              ]
                          ]
                      ]
              ]
          ]
      ]
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
              [ HH.button [ mkClass "button has-background is-rounded is-large is-fullwidth is-responsive is-uppercase", HE.onClick $ const ClickListAllVaults ]
                  [ HH.text "manage vault" ]
              ]
          ]
      ]

