module Dusd.Browser.Home where

import Contract.Prelude

import Dusd.Browser.Types (Vault, Asset(..))
import Data.Array (concat)
import Effect (Effect)
import Effect.Aff (error, throwError)
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

main :: Effect Unit
main = HA.runHalogenAff do
  _ <- HA.awaitLoad
  mbHome <- HA.selectElement $ QuerySelector "#home"
  case mbHome of
    Nothing -> throwError $ error "`#home` element to found. Dusd component unable to mount."
    Just elem -> void $ runUI component unit elem

type State =
  { vaults :: Array Vault
  , filter :: Filter
  }

data Action = SetFilter Filter

data Filter
  = PopularAssets
  | AllAssets
  | StableCoins
  | LPToken

derive instance eqFilter :: Eq Filter

component
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
component =
  H.mkComponent
    { initialState: const
        { vaults:
            [ { asset: Asset "Wrapped Bitcoin", assetType: "WBTC-A", dUsdAvailable: 29.36, stablilityFee: 2.00, minCollRatio: 160.0 }
            , { asset: Asset "Wrapped Bitcoin", assetType: "WBTC-A", dUsdAvailable: 29.36, stablilityFee: 2.00, minCollRatio: 160.0 }
            , { asset: Asset "Wrapped Bitcoin", assetType: "WBTC-A", dUsdAvailable: 29.36, stablilityFee: 2.00, minCollRatio: 160.0 }
            , { asset: Asset "Wrapped Bitcoin", assetType: "WBTC-A", dUsdAvailable: 29.36, stablilityFee: 2.00, minCollRatio: 160.0 }
            , { asset: Asset "Wrapped Bitcoin", assetType: "WBTC-A", dUsdAvailable: 29.36, stablilityFee: 2.00, minCollRatio: 160.0 }
            , { asset: Asset "Wrapped Bitcoin", assetType: "WBTC-A", dUsdAvailable: 29.36, stablilityFee: 2.00, minCollRatio: 160.0 }
            , { asset: Asset "Wrapped Bitcoin", assetType: "WBTC-A", dUsdAvailable: 29.36, stablilityFee: 2.00, minCollRatio: 160.0 }
            , { asset: Asset "Wrapped Bitcoin", assetType: "WBTC-A", dUsdAvailable: 29.36, stablilityFee: 2.00, minCollRatio: 160.0 }
            , { asset: Asset "Wrapped Bitcoin", assetType: "WBTC-A", dUsdAvailable: 29.36, stablilityFee: 2.00, minCollRatio: 160.0 }
            ]
        , filter: PopularAssets
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    SetFilter filter -> H.modify_ _ { filter = filter }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { vaults, filter } =
    HH.section
      [ mkClass "section my-6 pb-6" ]
      $
        [ HH.div [ mkClass "columns my-6" ]
            [ HH.div [ mkClass "column coin link-a-coin mx-6 p-4" ]
                [ HH.div [ mkClass "level" ]
                    [ HH.div [ mkClass "level-left" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.div []
                                [ HH.p [ mkClass "heading" ] [ HH.text "new" ]
                                , HH.p [ mkClass "title" ] [ HH.text "LINK-A" ]
                                ]
                            ]
                        ]
                    , HH.div [ mkClass "level-right" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.figure [ mkClass "image" ]
                                [ HH.element (HH.ElemName "picture") []
                                    [ HH.source [ HP.attr (HH.AttrName "srcset") "/assets/images/link-a-coin.jxl", HP.attr (HH.AttrName "type") "image/jxl" ]
                                    , HH.source [ HP.attr (HH.AttrName "srcset") "/assets/images/link-a-coin.webp", HP.attr (HH.AttrName "type") "image/webp" ]
                                    , HH.img
                                        [ HP.src "/assets/images/link-a-coin.png"
                                        , HP.width 174
                                        , HP.height 187
                                        , HP.alt ""
                                        , HP.attr (HH.AttrName "loading") "lazy"
                                        , HP.attr (HH.AttrName "decoding") "async"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , HH.div [ mkClass "level" ]
                    [ HH.div [ mkClass "level-left" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.span [ mkClass "subtitle is-size-7" ]
                                [ HH.text "Stablity fee: 3.00%"
                                ]
                            ]
                        ]
                    , HH.div [ mkClass "level-right" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.span [ mkClass "subtitle is-size-7" ]
                                [ HH.text "Min Coll. Ratio: 165%"
                                ]
                            ]
                        ]
                    ]
                ]
            , HH.div [ mkClass "column coin uni-a-coin mx-6 p-4" ]
                [ HH.div [ mkClass "level" ]
                    [ HH.div [ mkClass "level-left" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.div []
                                [ HH.p [ mkClass "heading" ] [ HH.text "most popular" ]
                                , HH.p [ mkClass "title" ] [ HH.text "UNI-A" ]
                                ]
                            ]
                        ]
                    , HH.div [ mkClass "level-right" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.figure [ mkClass "image" ]
                                [ HH.element (HH.ElemName "picture") []
                                    [ HH.source [ HP.attr (HH.AttrName "srcset") "/assets/images/uni-a-coin.jxl", HP.attr (HH.AttrName "type") "image/jxl" ]
                                    , HH.source [ HP.attr (HH.AttrName "srcset") "/assets/images/uni-a-coin.webp", HP.attr (HH.AttrName "type") "image/webp" ]
                                    , HH.img
                                        [ HP.src "/assets/images/uni-a-coin.png"
                                        , HP.width 174
                                        , HP.height 187
                                        , HP.alt ""
                                        , HP.attr (HH.AttrName "loading") "lazy"
                                        , HP.attr (HH.AttrName "decoding") "async"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , HH.div [ mkClass "level" ]
                    [ HH.div [ mkClass "level-left" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.span [ mkClass "subtitle is-size-7" ]
                                [ HH.text "Stablity fee: 2.00%"
                                ]
                            ]
                        ]
                    , HH.div [ mkClass "level-right" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.span [ mkClass "subtitle is-size-7" ]
                                [ HH.text "Min Coll. Ratio: 145%"
                                ]
                            ]
                        ]
                    ]
                ]
            , HH.div [ mkClass "column coin gusd-a-coin mx-6 p-4" ]
                [ HH.div [ mkClass "level" ]
                    [ HH.div [ mkClass "level-left" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.div []
                                [ HH.p [ mkClass "heading" ] [ HH.text "cheapest" ]
                                , HH.p [ mkClass "title" ] [ HH.text "GUSD-A" ]
                                ]
                            ]
                        ]
                    , HH.div [ mkClass "level-right" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.figure [ mkClass "image" ]
                                [ HH.element (HH.ElemName "picture") []
                                    [ HH.source [ HP.attr (HH.AttrName "srcset") "/assets/images/gusd-a-coin.jxl", HP.attr (HH.AttrName "type") "image/jxl" ]
                                    , HH.source [ HP.attr (HH.AttrName "srcset") "/assets/images/gusd-a-coin.webp", HP.attr (HH.AttrName "type") "image/webp" ]
                                    , HH.img
                                        [ HP.src "/assets/images/gusd-a-coin.png"
                                        , HP.width 174
                                        , HP.height 187
                                        , HP.alt ""
                                        , HP.attr (HH.AttrName "loading") "lazy"
                                        , HP.attr (HH.AttrName "decoding") "async"
                                        ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , HH.div [ mkClass "level" ]
                    [ HH.div [ mkClass "level-left" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.span [ mkClass "subtitle is-size-7" ]
                                [ HH.text "Stablity fee: 0.00%"
                                ]
                            ]
                        ]
                    , HH.div [ mkClass "level-right" ]
                        [ HH.div [ mkClass "level-item" ]
                            [ HH.span [ mkClass "subtitle is-size-7" ]
                                [ HH.text "Min Coll. Ratio: 101%"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , HH.div [ mkClass "columns m-6 is-vcentered" ]
            [ HH.div [ mkClass "column" ]
                [ if filter == PopularAssets then
                    HH.button [ mkClass "button is-large is-rounded is-responsive dusd-btn-has-background" ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Popular Assets" ] ]
                  else
                    HH.button [ mkClass "button is-large is-rounded is-responsive", HE.onClick $ const (SetFilter PopularAssets) ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Popular Assets" ] ]
                ]
            , HH.div [ mkClass "column" ]
                [ if filter == AllAssets then
                    HH.button [ mkClass "button is-large is-rounded is-responsive dusd-btn-has-background" ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "All Assets" ] ]
                  else
                    HH.button [ mkClass "button is-large is-rounded is-responsive", HE.onClick $ const (SetFilter AllAssets) ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "All Assets" ] ]
                ]
            , HH.div [ mkClass "column" ]
                [ if filter == StableCoins then
                    HH.button [ mkClass "button is-large is-rounded is-responsive dusd-btn-has-background" ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Stablecoins" ] ]
                  else
                    HH.button [ mkClass "button is-large is-rounded is-responsive", HE.onClick $ const (SetFilter StableCoins) ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Stablecoins" ] ]
                ]
            , HH.div [ mkClass "column" ]
                [ if filter == LPToken then
                    HH.button [ mkClass "button is-large is-rounded is-responsive dusd-btn-has-background" ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "LP Token" ] ]
                  else
                    HH.button [ mkClass "button is-large is-rounded is-responsive", HE.onClick $ const (SetFilter LPToken) ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "LP Token" ] ]
                ]
            , HH.div [ mkClass "column" ]
                [ HH.form []
                    [ HH.div [ mkClass "field" ]
                        [ HH.div [ mkClass "control has-icons-left" ]
                            [ HH.span [ mkClass "icon is-left pt-2" ]
                                [ HH.a [ HP.title "search" ]
                                    [ SE.svg [ role "none" ]
                                        [ SE.use [ href ("/assets/images/font-awesome-sprite-solid.svg#magnifying-glass") ]
                                        ]
                                    ]
                                ]
                            , HH.input [ mkClass "input is-rounded search-input" ]
                            ]
                        ]
                    ]
                ]
            ]
        , HH.div
            [ mkClass "columns is-mobile" ]
            [ HH.div [ mkClass "column has-text-centered" ]
                [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Asset" ] ]
            , HH.div [ mkClass "column has-text-centered" ]
                [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Type" ] ]
            , HH.div [ mkClass "column" ]
                [ HH.div [ mkClass "columns is-mobile is-centered is-vcentered" ]
                    [ HH.div [ mkClass "column is-narrow" ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "dUSD Available" ] ]
                    , HH.div [ mkClass "column is-narrow" ] [ mkIcon "angle-down" ]
                    ]
                ]
            , HH.div [ mkClass "column" ]
                [ HH.div [ mkClass "columns is-mobile is-centered is-vcentered" ]
                    [ HH.div [ mkClass "column is-narrow" ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Stabiliity Fee" ] ]
                    , HH.div [ mkClass "column is-narrow" ] [ mkIcon "angle-down" ]
                    ]
                ]
            , HH.div [ mkClass "column" ]
                [ HH.div [ mkClass "columns is-mobile is-centered is-vcentered" ]
                    [ HH.div [ mkClass "column is-narrow" ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "Min Coll. Ratio" ] ]
                    , HH.div [ mkClass "column is-narrow" ] [ mkIcon "angle-down" ]
                    ]
                ]
            , HH.div [ mkClass "column" ] [ HH.text " " ]
            ]

        ]
      <> concat
        ( renderVault
            <$> vaults
        )
      <>
        [ HH.div [ mkClass "columns m-6" ]
            [ HH.div [ mkClass "column" ]
                [ HH.div [ mkClass "dana-coin info-card p-6" ]
                    [ HH.div [ mkClass "columns is-mobile" ]
                        [ HH.div [ mkClass "column" ]
                            [ HH.p [ mkClass "title" ] [ HH.text "Dana Coin" ]
                            , HH.p [ mkClass "subtitle is-size-7" ] [ HH.text "Buy, send and manage your Dana Coin all in one place. Grow your Dana, and access plenty of providers." ]
                            ]
                        , HH.div [ mkClass "column has-text-right" ]
                            [ HH.div [ mkClass "pt-6" ] [ mkIcon "circle-arrow-right" ]
                            ]
                        ]
                    ]
                ]
            , HH.div [ mkClass "column" ]
                [ HH.div [ mkClass "got-questions info-card p-6" ]
                    [ HH.div [ mkClass "columns is-mobile" ]
                        [ HH.div [ mkClass "column" ]
                            [ HH.p [ mkClass "title" ] [ HH.text "Got questions?" ]
                            , HH.p [ mkClass "subtitle is-size-7" ] [ HH.text "Learn more about Dana Coin, DanaSwap and Stablecoin Vaults by visiting our FAQs page." ]
                            ]
                        , HH.div [ mkClass "column has-text-right" ]
                            [ HH.div [ mkClass "pt-6" ] [ mkIcon "circle-arrow-right" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    where
    renderVault v =
      [ HH.div
          [ mkClass "vaults-table-row columns is-mobile is-vcentered p-3 my-3" ]

          [ HH.div [ mkClass "column" ]
              [ HH.div [ mkClass "columns is-mobile is-centered is-vcentered" ]
                  [ HH.div [ mkClass "column is-narrow" ]
                      [ HH.figure [ mkClass "image" ]
                          [ HH.element (HH.ElemName "picture")
                              [ HP.title "Bitcoin"
                              , role "presentation"
                              ]
                              [ HH.source [ HP.attr (HH.AttrName "srcset") "/assets/images/bitcoin.jxl", HP.attr (HH.AttrName "type") "image/jxl" ]
                              , HH.source [ HP.attr (HH.AttrName "srcset") "/assets/images/bitcoin.webp", HP.attr (HH.AttrName "type") "image/webp" ]
                              , HH.img [ HP.src "/assets/images/bitcoin.png", HP.alt "" ]
                              ]
                          ]
                      ]
                  , HH.p [ mkClass "column is-narrow is-size-5-desktop is-size-7-touch" ] [ HH.text $ unwrap v.asset ]
                  ]
              ]
          , HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch has-text-centered" ] [ HH.text v.assetType ]
              ]
          , HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch has-text-centered" ] [ HH.text $ show v.dUsdAvailable <> "M" ]
              ]
          , HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch has-text-centered" ] [ HH.text $ show v.stablilityFee <> "%" ]
              ]
          , HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch has-text-centered" ] [ HH.text $ show v.minCollRatio <> "%" ]
              ]
          , HH.div [ mkClass "column has-text-centered" ]
              [ HH.button [ mkClass "button is-large is-rounded is-responsive dusd-btn-has-background" ] [ HH.span [ mkClass "title is-size-4-desktop is-size-6-touch" ] [ HH.text "open vault" ] ]
              ]
          ]
      ]

mkClass :: forall (r :: Row Type) (i :: Type). String -> HP.IProp (class :: String | r) i
mkClass = HP.class_ <<< HH.ClassName

mkIcon :: forall slots m. String -> H.ComponentHTML Action slots m
mkIcon icon = HH.span
  [ mkClass "icon" ]
  [ HH.a [ HP.title icon ]
      [ SE.svg [ role "none" ]
          [ SE.use [ href ("/assets/images/font-awesome-sprite-solid.svg#" <> icon) ]
          ]
      ]
  ]
