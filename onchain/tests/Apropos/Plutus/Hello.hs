module Apropos.Plutus.Hello (
  HelloProp (..),
  spec,
) where

import Apropos
import Apropos.Script

import Test.Syd
import Test.Syd.Hedgehog

import Plutarch (compile, (#))
import Plutarch.Prelude qualified as PPrelude

import Hello (helloLogic)

type HelloModel = (Integer, Integer)

data HelloProp
  = IsValid
  | IsInvalid
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable, Enumerable)

instance LogicalModel HelloProp where
  logic = ExactlyOne [Var IsValid, Var IsInvalid]

instance HasLogicalModel HelloProp HelloModel where
  satisfiesProperty IsValid (i, j) = i + 1 == j
  satisfiesProperty IsInvalid p = not $ satisfiesProperty IsValid p

instance HasPermutationGenerator HelloProp HelloModel where
  sources =
    [ Source
        { sourceName = "baseGen"
        , covers = Yes
        , gen =
            (,)
              <$> (fromIntegral <$> int (linear (-10) 10))
              <*> (fromIntegral <$> int (linear (-10) 10))
        }
    ]
  generators =
    [ Morphism
        { name = "MakeValid"
        , match = Not $ Var IsValid
        , contract = clear >> addAll [IsValid]
        , morphism = \(i, _) -> pure (i, i + 1)
        }
    , Morphism
        { name = "MakeInvalid"
        , match = Not $ Var IsInvalid
        , contract = clear >> addAll [IsInvalid]
        , morphism = \(i, _) -> do
            j <- genFilter (/= (i + 1)) (fromIntegral <$> int (linear (-10) 10))
            pure (i, j)
        }
    ]

instance HasParameterisedGenerator HelloProp HelloModel where
  parameterisedGenerator = buildGen

instance ScriptModel HelloProp HelloModel where
  expect = Var IsValid
  script (i, j) = compile $ helloLogic # PPrelude.pconstant i # PPrelude.pconstant j

spec :: Spec
spec = do
  describe "helloGenSelfTest" $
    mapM_ fromHedgehogGroup $
      [ runGeneratorTestsWhere @HelloProp "Hello Generator" Yes
      ]
  describe "helloLogicTests" $
    mapM_ fromHedgehogGroup $
      [ runScriptTestsWhere @HelloProp "AcceptsValid" Yes
      ]
