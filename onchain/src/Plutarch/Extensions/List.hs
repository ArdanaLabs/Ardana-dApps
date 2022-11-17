module Plutarch.Extensions.List (
  unsingleton,
  ptake,
) where

import Plutarch.Prelude
import Plutarch.Extra.TermCont (pmatchC)
import Plutarch.List (puncons)

-- | Enforces that a list has only one element and returns it
unsingleton :: PLift a => Term s (PBuiltinList a) -> Term s a
unsingleton list = unTermCont $ do
  PCons x xs <- pmatchC list
  PNil <- pmatchC xs
  pure x

ptake :: forall (l :: PType -> PType) (p :: PType).
    PIsListLike l p => ClosedTerm (PInteger :--> l p :--> l p)
ptake = phoistAcyclic $ pfix #$
  plam $ \self n xs ->
    pif (n #== 0)
      (pnil :: Term s (l p))
      (unTermCont $ do
        pmatchC (puncons # xs) >>= \case
          PNothing -> pure pnil
          PJust pair -> do
            PPair y ys <- pmatchC pair
            pure $ pcons # y # (self # (n-1) # ys)
      )
