module Main where

import Contract.Prelude

import Effect (Effect)
import Effect.Aff (error, throwError)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = HA.runHalogenAff do
  _ <- HA.awaitLoad
  mbHome <- HA.selectElement $ QuerySelector "#home"
  case mbHome of
    Nothing -> pure unit
    Just elem -> void $ runUI homeComponent unit elem

  mbPools <- HA.selectElement $ QuerySelector "#pools"
  case mbPools of
    Nothing -> pure unit
    Just elem -> void $ runUI poolsComponent unit elem

homeComponent
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
homeComponent =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where
  render _ =
    HH.div_
      [ HH.text "this is Home"
      ]

poolsComponent
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
poolsComponent =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
    }
  where
  render _ =
    HH.div_
      [ HH.text "this is Pools"
      ]
