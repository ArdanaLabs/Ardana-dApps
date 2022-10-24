module DanaSwapBrowser.Home where

import Contract.Prelude

import DanaSwapBrowser.Types (Pool(..))
import Data.Array (concat)
import Data.BigInt (fromInt, fromString)
import Data.BigInt as BigInt
import Data.Number.Format (toString)
import Effect (Effect)
import Effect.Aff (error, throwError)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  _ <- HA.awaitLoad
  mbHome <- HA.selectElement $ QuerySelector "#home"
  case mbHome of
    Nothing -> throwError $ error "Cannot find #home"
    Just elem -> void $ runUI component unit elem

type State =
  { pools :: Array Pool
  , currentPool :: Maybe Pool
  }

data Action
  = SetCurrentPool Pool
  | ResetCurrentPool

component
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
component =
  H.mkComponent
    { initialState: const
        { pools:
            [ Pool { id: 1, title: "sBTC", subTitle: "DAI + USDC + USDIT + sUSD", tvl: 3.44, tradingFeesApr: 3.44, lpFee: 2.55, totalLps: fromString "75,478,639,987,839" }
            , Pool { id: 2, title: "sBTC", subTitle: "DAI + USDC + USDIT + sUSD", tvl: 3.44, tradingFeesApr: 3.44, lpFee: 2.55, totalLps: fromString "75,478,639,987,839" }
            , Pool { id: 3, title: "sBTC", subTitle: "DAI + USDC + USDIT + sUSD", tvl: 3.44, tradingFeesApr: 3.44, lpFee: 2.55, totalLps: fromString "75,478,639,987,839" }
            , Pool { id: 4, title: "sBTC", subTitle: "DAI + USDC + USDIT + sUSD", tvl: 3.44, tradingFeesApr: 3.44, lpFee: 2.55, totalLps: fromString "75,478,639,987,839" }
            , Pool { id: 5, title: "sBTC", subTitle: "DAI + USDC + USDIT + sUSD", tvl: 3.44, tradingFeesApr: 3.44, lpFee: 2.55, totalLps: fromString "75,478,639,987,839" }
            , Pool { id: 6, title: "sBTC", subTitle: "DAI + USDC + USDIT + sUSD", tvl: 3.44, tradingFeesApr: 3.44, lpFee: 2.55, totalLps: fromString "75,478,639,987,839" }
            , Pool { id: 7, title: "sBTC", subTitle: "DAI + USDC + USDIT + sUSD", tvl: 3.44, tradingFeesApr: 3.44, lpFee: 2.55, totalLps: fromString "75,478,639,987,839" }
            , Pool { id: 8, title: "sBTC", subTitle: "DAI + USDC + USDIT + sUSD", tvl: 3.44, tradingFeesApr: 3.44, lpFee: 2.55, totalLps: fromString "75,478,639,987,839" }
            , Pool { id: 9, title: "sBTC", subTitle: "DAI + USDC + USDIT + sUSD", tvl: 3.44, tradingFeesApr: 3.44, lpFee: 2.55, totalLps: fromString "75,478,639,987,839" }
            ]
        , currentPool: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  handleAction :: forall slots. Action -> H.HalogenM State Action slots o m Unit
  handleAction = case _ of
    ResetCurrentPool -> H.modify_ _ { currentPool = Nothing }
    SetCurrentPool pool -> H.modify_ _ { currentPool = Just pool }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { pools, currentPool } =
    HH.table [ mkClass "table is-fullwidth" ]
      [ HH.thead_
          [ HH.tr_
              [ mkTableHeader "POOL"
              , mkTableHeader "TVL(ADA)"
              , mkTableHeader "TRADING FEES APR"
              , mkTableHeader "LP FEE"
              , mkTableHeader "TOTAL LPs"
              ]
          ]
      , HH.tbody_ $ concat
          ( renderPool currentPool
              <$> pools
          )
      , HH.tfoot_
          [ HH.tr_
              [ HH.th_ []
              , HH.th_ []
              , HH.th_ []
              , HH.th
                  [ HP.colSpan 2 ]
                  [ HH.button
                      [ mkClass "button is-rounded is-medium is-fullwidth danaswap-btn-has-background" ]
                      [ HH.text "SEE ALL POOLS" ]
                  ]
              ]
          ]
      ]

  renderPool :: forall slots. Maybe Pool -> Pool -> Array (H.ComponentHTML Action slots m)
  renderPool currentPool pool@(Pool p) =
    [ HH.tr
        [ HE.onClick $ \_ ->
            if currentPool == Just pool then
              ResetCurrentPool
            else
              SetCurrentPool pool
        ]

        [ HH.td_
            [ HH.div [ mkClass "columns is-vcentered" ]
                [ HH.div [ mkClass "column is-one-fifth" ]
                    [ HH.figure
                        [ mkClass "image is-48x48" ]
                        [ HH.img
                            [ mkClass "is-rounded"
                            , HP.src "../../assets/images/bitcoin.png"
                            ]
                        ]
                    ]
                , HH.div [ mkClass "column is-four-fifths" ]
                    [ HH.p [ mkClass "title is-5 has-text-white" ] [ HH.text p.title ]
                    , HH.p [ mkClass "subtitle is-6 has-text-white" ] [ HH.text p.subTitle ]
                    ]
                ]
            ]
        , HH.td [ mkClass "is-vcentered" ]
            [ HH.p [ mkClass "title is-5 has-text-white" ] [ HH.text $ "$" <> (toString p.tvl) <> "M" ]
            ]
        , HH.td [ mkClass "is-vcentered" ]
            [ HH.p [ mkClass "title is-5 has-text-white" ] [ HH.text $ "$" <> (toString p.tradingFeesApr) <> "M" ]
            ]
        , HH.td [ mkClass "is-vcentered" ]
            [ HH.p [ mkClass "title is-5 has-text-white" ] [ HH.text $ "$" <> (toString p.lpFee) <> "M" ]
            ]
        , HH.td [ mkClass "is-vcentered" ]
            [ HH.div [ mkClass "columns is-vcentered" ]
                [ HH.div [ mkClass "column is-narrow" ]
                    [ HH.p [ mkClass "title is-5 has-text-white" ] [ HH.text $ BigInt.toString (fromMaybe (fromInt 0) p.totalLps) ]
                    ]
                , HH.div [ mkClass "column" ]
                    [ if currentPool == Just pool then
                        mkIcon "angle-up"
                      else
                        mkIcon "angle-down"
                    ]
                ]
            ]
        ]

    ] <>
      if currentPool == Just pool then
        [ HH.tr_
            [ HH.td [ HP.colSpan 5 ]
                [ HH.div [ mkClass "box my-3" ]
                    [ HH.div
                        [ mkClass "columns is-vcentered" ]
                        [ HH.div [ mkClass "column is-three-fifths" ]
                            [ HH.div [ mkClass "columns" ]
                                [ HH.div [ mkClass "column" ]
                                    [ HH.p [ mkClass "subtitle is-6 has-text-white" ] [ HH.text "ADA Price" ]
                                    , HH.p [ mkClass "title is-5 has-text-white" ] [ HH.text "1 ADA = 0.00 DUSD" ]
                                    ]
                                , HH.div [ mkClass "column" ]
                                    [ HH.p [ mkClass "subtitle is-6 has-text-white" ] [ HH.text "MIN Price" ]
                                    , HH.p [ mkClass "title is-5 has-text-white" ] [ HH.text "1 DUSD = 27.3231 ADA" ]
                                    ]
                                ]
                            ]
                        , HH.div [ mkClass "column is-two-fifths" ]
                            [ HH.div [ mkClass "columns is-centered" ]
                                [ HH.div [ mkClass "column is-narrow" ]
                                    [ HH.button [ mkClass "button is-rounded is-medium danaswap-btn-has-border" ] [ HH.text "SWAP" ]
                                    ]
                                , HH.div [ mkClass "column is-narrow" ]
                                    [ HH.button [ mkClass "button is-rounded is-medium danaswap-btn-has-background" ] [ HH.text "ADD LIQUIDITY" ]
                                    ]
                                , HH.div [ mkClass "column is-narrow" ]
                                    [ HH.button [ mkClass "button is-rounded is-medium danaswap-btn-has-border" ] [ HH.text "WITHDRAW" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
      else []

mkClass :: forall (r :: Row Type) (i :: Type). String -> HP.IProp (class :: String | r) i
mkClass = HP.class_ <<< HH.ClassName

mkIcon :: forall slots m. String -> H.ComponentHTML Action slots m
mkIcon icon = HH.span
  [ mkClass "icon is-small has-text-white" ]
  [ HH.i [ mkClass $ "fas fa-" <> icon ] [] ]

mkTableHeader :: forall slots m. String -> H.ComponentHTML Action slots m
mkTableHeader title = HH.th_
  [ HH.div [ mkClass "columns is-vcentered" ]
      [ HH.div [ mkClass "column is-narrow has-text-white" ] [ HH.text title ]
      , HH.div [ mkClass "column mt-1" ] [ mkIcon "sort" ]
      ]
  ]
