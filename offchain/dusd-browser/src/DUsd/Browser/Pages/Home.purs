module DUsd.Browser.Pages.Home where

import Contract.Prelude

import DUsd.Browser.AppM (runAppM)
import DUsd.Browser.Capability.CardanoApi (class CardanoApi)
import DUsd.Browser.Capability.DUsdApi (class DUsdApi)
import DUsd.Browser.Components.ConnectWallet as ConnectWallet
import DUsd.Browser.Components.CreateNewVault as CreateNewVault
import DUsd.Browser.Components.ListAllVaults as ListAllVaults
import DUsd.Browser.Types.RoutingMessage (RoutingMessage)
import DUsd.Browser.Types.RoutingMessage as RoutingMessage
import Data.Const (Const)
import Effect (Effect)
import Effect.Aff (error, throwError)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Type.Proxy (Proxy(..))
import Web.DOM.ParentNode (QuerySelector(..))

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

data State
  = ConnectWallet
  | ListAllVaults
  | CreateNewVault

data Action = HandleRoutingMessage RoutingMessage

type ChildSlots =
  ( connectWallet :: H.Slot (Const Void) RoutingMessage Unit
  , listAllVaults :: H.Slot (Const Void) RoutingMessage Unit
  , createNewVault :: H.Slot (Const Void) RoutingMessage Unit
  )

component
  :: forall q i o m
   . MonadAff m
  => CardanoApi m
  => DUsdApi m
  => H.Component q i o m
component =
  H.mkComponent
    { initialState: const ConnectWallet
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction = case _ of
    HandleRoutingMessage msg -> case msg of
      RoutingMessage.ConnectWallet -> H.modify_ $ const ConnectWallet
      RoutingMessage.ListAllVaults -> H.modify_ $ const ListAllVaults
      RoutingMessage.CreateNewVault -> H.modify_ $ const CreateNewVault

  render :: State -> H.ComponentHTML Action ChildSlots m
  render = case _ of
    ConnectWallet -> HH.slot (Proxy :: _ "connectWallet") unit ConnectWallet.component unit HandleRoutingMessage
    ListAllVaults -> HH.slot (Proxy :: _ "listAllVaults") unit ListAllVaults.component unit HandleRoutingMessage
    CreateNewVault -> HH.slot (Proxy :: _ "createNewVault") unit CreateNewVault.component unit HandleRoutingMessage