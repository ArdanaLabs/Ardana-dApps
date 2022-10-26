module DanaSwap.Browser.Home where

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
    Nothing -> throwError $ error "Cannot find #home"
    Just elem -> void $ runUI component unit elem

component
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
component =
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
