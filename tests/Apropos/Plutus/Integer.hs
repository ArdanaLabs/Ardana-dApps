module Apropos.Plutus.Integer
  ( IntegerProp
  , integerGenSelfTest
  ) where

import Apropos
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (fromGroup)

data IntegerProp
  = IsNegative
  | IsPositive
  | IsZero
  | IsLarge
  | IsSmall
  deriving stock (Show,Eq,Ord,Enum,Bounded)

instance Enumerable IntegerProp where
  enumerated = [minBound..maxBound]


instance LogicalModel IntegerProp where
  logic =
    ExactlyOne [Var IsNegative, Var IsPositive, Var IsZero]
      :&&: ExactlyOne [Var IsLarge, Var IsSmall]
      :&&: (Var IsZero :->: Var IsSmall)


instance HasLogicalModel IntegerProp Integer where
  satisfiesProperty IsNegative i = i < 0
  satisfiesProperty IsPositive i = i > 0
  satisfiesProperty IsZero     i = i == 0
  satisfiesProperty IsLarge    i = i > 10 || i < -10
  satisfiesProperty IsSmall    i = i <= 10 && i >= -10


instance HasPermutationGenerator IntegerProp Integer where
  generators =
    [ Morphism
        { name = "MakeZero"
        , match = Not $ Var IsZero
        , contract = clear >> addAll [IsZero, IsSmall]
        , morphism = \_ -> pure 0
        }
    , Morphism
        { name = "MakeLarge"
        , match = Not $ Var IsLarge
        , contract = clear >> addAll [IsLarge, IsPositive]
        , morphism = \_ -> do
            i <- int (linear 11 (maxBound -1))
            pure $ fromIntegral i
        }
    , Morphism
        { name = "MakeSmall"
        , match = Not $ Var IsSmall
        -- TODO filtered clear might be nicer than addIf
        , contract = clear >> addAll [IsSmall, IsPositive]
        , morphism = \_ -> do
            i <- int (linear 1 10)
            pure $ fromIntegral i
        }
    , Morphism
        { name = "Negate"
        , match = Not $ Var IsZero
        , contract =
            branches
              [ has IsNegative >> remove IsNegative >> add IsPositive
              , has IsPositive >> remove IsPositive >> add IsNegative
              ]
        , morphism = pure . negate
        }
    ]

integerGenSelfTest :: TestTree
integerGenSelfTest =
  testGroup "integerGenSelfTest" $
    fromGroup
      <$> permutationGeneratorSelfTest
        True
        (\(_ :: Morphism IntegerProp integerGenSelfTest) -> True)
        (pure (0 :: Integer))
