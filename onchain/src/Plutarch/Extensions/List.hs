module Plutarch.Extensions.List (
  unsingleton,
) where

import Plutarch.Extra.TermCont (pmatchC, ptraceC)
import Plutarch.Prelude

-- | Enforces that a list has only one element and returns it
unsingleton :: (PLift a, PShow a) => Term s (PBuiltinList a) -> Term s a
unsingleton list = unTermCont $ do
  ptraceC $ "unslingleton called on:" <> pshow list
  PCons x xs <- pmatchC list
  PNil <- pmatchC xs
  pure x
