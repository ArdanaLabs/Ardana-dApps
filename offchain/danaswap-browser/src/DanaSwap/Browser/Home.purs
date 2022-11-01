module DanaSwap.Browser.Home where

import Contract.Prelude

import DanaSwapBrowser.Types (Asset(..), Pool(..))
import Data.Array (concat)
import Data.BigInt (fromInt, fromString, toString)
import Effect (Effect)
import Effect.Aff (error, throwError)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  _ <- HA.awaitLoad
  mbHome <- HA.selectElement $ QuerySelector "#home"
  case mbHome of
    Nothing -> throwError $ error "`#home` element to found. DanaSwap component unable to mount."
    Just elem -> void $ runUI component unit elem

type State =
  { pools :: Array Pool
  , currentPool :: Maybe Pool
  , activity :: Activity
  }

data Action
  = SetCurrentPool Pool
  | ResetCurrentPool
  | SetActivity Activity

data Activity
  = Swap
  | AddLiquidity
  | WithdrawLiquidity

derive instance eqActivity :: Eq Activity

component
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
component =
  H.mkComponent
    { initialState: const
        { pools:
            [ Pool { assetA: Asset { name: "ADA", value: fromInt 1200 }, assetB: Asset { name: "DANA", value: fromInt 1000 }, fee: 4 }
            , Pool { assetA: Asset { name: "ADA", value: fromInt 10000 }, assetB: Asset { name: "USDC", value: fromInt 4000 }, fee: 4 }
            , Pool { assetA: Asset { name: "ADA", value: fromInt 120 }, assetB: Asset { name: "HOSKY", value: fromMaybe (fromInt 0) (fromString "3000000100") }, fee: 4 }
            , Pool { assetA: Asset { name: "DADA", value: fromInt 3000 }, assetB: Asset { name: "USDC", value: fromInt 1000 }, fee: 4 }
            ]
        , currentPool: Nothing
        , activity: Swap
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
    SetActivity activity -> H.modify_ _ { activity = activity }

  render :: forall slots. State -> H.ComponentHTML Action slots m
  render { pools, currentPool, activity } =
    HH.section
      [ mkClass "section my-6 pb-6" ]
      $
        [ HH.div
            [ mkClass "columns is-mobile" ]
            [ HH.div [ mkClass "pools-table-header column" ]
                [ HH.div [ mkClass $ "columns" ]
                    [ HH.div [ mkClass "column has-text-left" ] [ HH.text "Pool" ]
                    ]
                ]
            , HH.div [ mkClass "pools-table-header column" ]
                [ HH.div [ mkClass $ "columns" ]
                    [ HH.div [ mkClass "column has-text-centered" ] [ HH.text "Value" ]
                    ]
                ]
            , HH.div [ mkClass "pools-table-header column" ]
                [ HH.div [ mkClass $ "columns" ]
                    [ HH.div [ mkClass "column has-text-centered" ] [ HH.text "Fee" ]
                    ]
                ]
            , HH.div [ mkClass "column" ] [ HH.text " " ]
            ]

        ]
      <> concat
        ( renderPool
            <$> pools
        )
    where
    renderPool pool@(Pool p) =
      [ HH.div
          [ mkClass "pools-table-row columns is-mobile is-vcentered p-3 my-3"
          , HE.onClick $ \_ ->
              if currentPool == Just pool then
                ResetCurrentPool
              else
                SetCurrentPool pool
          ]

          [ HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch has-text-left" ] [ HH.text $ printPool pool ]
              ]
          , HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch has-text-centered" ] [ HH.text $ printAsset p.assetA <> " / " <> printAsset p.assetB ]
              ]
          , HH.div [ mkClass "column" ]
              [ HH.p [ mkClass "is-size-5-desktop is-size-7-touch has-text-centered" ] [ HH.text $ show p.fee <> "ADA" ]
              ]
          , HH.div [ mkClass "column has-text-centered" ]
              [ if currentPool == Just pool then mkIcon "angle-up" else mkIcon "angle-down"
              ]
          ]
      ] <> renderPoolDropdown pool

    renderPoolDropdown pool =
      if currentPool == Just pool then
        [ HH.div [ mkClass "pools-table-row-dropdown box my-6" ]
            [ HH.div [ mkClass "columns is-mobile" ]
                [ HH.div [ mkClass "column is-narrow" ]
                    [ HH.button [ HP.type_ ButtonButton, HE.onClick $ const (SetActivity Swap), mkClass $ "button is-rounded is-medium is-responsive " <> if (activity == Swap) then "danaswap-btn-has-background" else "danaswap-btn-has-border" ] [ HH.text "swap" ]
                    ]
                , HH.div [ mkClass "column is-narrow" ]
                    [ HH.button [ HP.type_ ButtonButton, HE.onClick $ const (SetActivity AddLiquidity), mkClass $ "button is-rounded is-medium is-responsive " <> if (activity == AddLiquidity) then "danaswap-btn-has-background" else "danaswap-btn-has-border" ] [ HH.text "add liquidity" ]
                    ]
                , HH.div [ mkClass "column is-narrow" ]
                    [ HH.button [ HP.type_ ButtonButton, HE.onClick $ const (SetActivity WithdrawLiquidity), mkClass $ "button is-rounded is-medium is-responsive " <> if (activity == WithdrawLiquidity) then "danaswap-btn-has-background" else "danaswap-btn-has-border" ] [ HH.text "withdraw" ]
                    ]
                ]
            , case activity of
                Swap -> renderSwapForm pool
                AddLiquidity -> renderAddLiquidityForm pool
                WithdrawLiquidity -> renderWithdrawLiquidityForm
            ]
        ]
      else []

    renderSwapForm (Pool ({ assetA: (Asset a), assetB: (Asset b) })) = HH.form [ mkClass "mt-6" ]
      [ HH.div [ mkClass "columns is-mobile is-vcentered" ]
          [ HH.div [ mkClass "column is-8-touch is-3-desktop" ]
              [ HH.div [ mkClass "field" ]
                  [ HH.div [ mkClass "control" ] [ HH.input [ HP.id "swap-asset-a", mkClass "input is-rounded" ] ]
                  ]
              ]
          , HH.div [ mkClass "column is-1" ]
              [ HH.label [ HP.for "swap-asset-a", mkClass "label is-normal" ] [ HH.text a.name ]
              ]
          ]
      , HH.div [ mkClass "columns is-mobile is-vcentered" ]
          [ HH.div [ mkClass "column is-offset-1-desktop is-offset-3-touch is-1 has-text-centered" ]
              [ HH.span
                  [ mkClass "icon is-medium" ]
                  [ HH.i [ mkClass $ "fas fa-exchange-alt" ] [] ]
              ]
          ]
      , HH.div [ mkClass "columns is-mobile is-vcentered" ]
          [ HH.div [ mkClass "column is-8-touch is-3-desktop" ]
              [ HH.div [ mkClass "field" ]
                  [ HH.div [ mkClass "control" ] [ HH.input [ HP.id "swap-asset-b", mkClass "input is-rounded" ] ]
                  ]
              ]
          , HH.div [ mkClass "column is-1" ]
              [ HH.label [ HP.for "swap-asset-b", mkClass "label is-normal" ] [ HH.text b.name ]
              ]
          ]
      , HH.p [ mkClass "subtitle is-6 my-0" ]
          [ HH.text "Slippage: 4%"
          ]
      , HH.p [ mkClass "subtitle is-6" ]
          [ HH.text "Fee: 2.3 ADA"
          ]
      , HH.div [ mkClass "columns" ]
          [ HH.div [ mkClass "column is-narrow" ]
              [ HH.button [ HP.type_ ButtonSubmit, mkClass $ "button is-rounded is-medium is-responsive danaswap-btn-has-border" ] [ HH.text "submit" ]
              ]
          ]
      ]

    renderAddLiquidityForm (Pool ({ assetA: (Asset a), assetB: (Asset b) })) = HH.form [ mkClass "mt-6" ]
      [ HH.div [ mkClass "columns is-mobile is-vcentered" ]
          [ HH.div [ mkClass "column is-8-touch is-3-desktop" ]
              [ HH.div [ mkClass "field" ]
                  [ HH.div [ mkClass "control" ] [ HH.input [ HP.id "add-liquidity-asset-a", mkClass "input is-rounded" ] ]
                  ]
              ]
          , HH.div [ mkClass "column is-1" ]
              [ HH.label [ HP.for "add-liquidity-asset-a", mkClass "label is-normal" ] [ HH.text a.name ]
              ]
          ]
      , HH.div [ mkClass "columns is-mobile is-vcentered" ]
          [ HH.div [ mkClass "column is-8-touch is-3-desktop" ]
              [ HH.div [ mkClass "field" ]
                  [ HH.div [ mkClass "control" ] [ HH.input [ HP.id "add-liquidity-asset-b", mkClass "input is-rounded" ] ]
                  ]
              ]
          , HH.div [ mkClass "column is-1" ]
              [ HH.label [ HP.for "add-liquidity-asset-b", mkClass "label is-normal" ] [ HH.text b.name ]
              ]
          ]
      , HH.p [ mkClass "subtitle is-6 my-0" ]
          [ HH.text "Liquidity Tokens: 0"
          ]
      , HH.p [ mkClass "subtitle is-6" ]
          [ HH.text "Fee: 2.3 ADA"
          ]
      , HH.div [ mkClass "columns" ]
          [ HH.div [ mkClass "column is-narrow" ]
              [ HH.button [ HP.type_ ButtonSubmit, mkClass $ "button is-rounded is-medium is-responsive danaswap-btn-has-border" ] [ HH.text "submit" ]
              ]
          ]
      ]

    renderWithdrawLiquidityForm = HH.form [ mkClass "mt-6" ]
      [ HH.div [ mkClass "columns is-mobile is-vcentered" ]
          [ HH.div [ mkClass "column is-8-touch is-3-desktop" ]
              [ HH.div [ mkClass "field" ]
                  [ HH.div [ mkClass "control" ] [ HH.input [ HP.id "withdraw-liquidity-tokens", mkClass "input is-rounded" ] ]
                  ]
              ]
          , HH.div [ mkClass "column is-1" ]
              [ HH.label [ HP.for "withdraw-liquidity-tokens", mkClass "label is-normal" ] [ HH.text "Liquidity Tokens" ]
              ]
          ]
      , HH.p [ mkClass "subtitle is-6 my-0" ]
          [ HH.text "ADA Payout: 0"
          ]
      , HH.p [ mkClass "subtitle is-6 my-0" ]
          [ HH.text "DANA Payout: 0"
          ]
      , HH.p [ mkClass "subtitle is-6 my-0" ]
          [ HH.text "Slippage: 4%"
          ]
      , HH.p [ mkClass "subtitle is-6" ]
          [ HH.text "Fee: 2.3 ADA"
          ]
      , HH.div [ mkClass "columns" ]
          [ HH.div [ mkClass "column is-narrow" ]
              [ HH.button [ HP.type_ ButtonSubmit, mkClass $ "button is-rounded is-medium is-responsive danaswap-btn-has-border" ] [ HH.text "submit" ]
              ]
          ]
      ]

mkClass :: forall (r :: Row Type) (i :: Type). String -> HP.IProp (class :: String | r) i
mkClass = HP.class_ <<< HH.ClassName

mkIcon :: forall slots m. String -> H.ComponentHTML Action slots m
mkIcon icon = HH.span
  [ mkClass "icon is-small" ]
  [ HH.i [ mkClass $ "fas fa-" <> icon ] [] ]

printAsset :: Asset -> String
printAsset (Asset { name, value }) = (toString value) <> " " <> name

printPool :: Pool -> String
printPool (Pool { assetA: (Asset a), assetB: (Asset b) }) = a.name <> " + " <> b.name