module DUsd.Browser.Utils where

import Contract.Prelude

import Ctl.Internal.Wallet (WalletExtension(..))
import Effect.Exception (throw)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

walletToString :: WalletExtension -> Effect String
walletToString = case _ of
  NamiWallet -> pure "nami"
  EternlWallet -> pure "eternl"
  _ -> throw "unknown wallet"

stringToWallet :: String -> Effect WalletExtension
stringToWallet = case _ of
  "nami" -> pure NamiWallet
  "eternl" -> pure EternlWallet
  _ -> throw "unknown wallet"

mkClass :: forall (r :: Row Type) (i :: Type). String -> HP.IProp (class :: String | r) i
mkClass = HP.class_ <<< HH.ClassName

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