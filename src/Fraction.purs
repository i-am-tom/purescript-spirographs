module Fraction
  ( Fraction
  , over
  , simplify

  , numerator
  , denominator

  , from
  , to
  ) where

import Control.MonadZero (guard)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Data.Ord (abs)
import Prelude
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

data Fraction = Over Int Int

instance arbitraryFraction :: Arbitrary Fraction where
  arbitrary
    = (\x y -> Over (abs x + 1) y)
        <$> arbitrary <*> arbitrary

over :: Int -> Int -> Maybe Fraction
over x y = map simplify (guard (y > 0) $> Over x y)

simplify :: Fraction -> Fraction
simplify fraction@(Over a b)
  = case gcd a b of
      1 -> fraction
      d -> Over (a / d) (b / d)

numerator :: Fraction -> Int
numerator (Over x _) = x

denominator :: Fraction -> Int
denominator (Over _ x) = x

to :: Int -> Fraction
to = (_ `Over` 1)

from :: Fraction -> Number
from (Over a b) = toNumber a / toNumber b

instance eqFraction :: Eq Fraction where
  eq x y = case simplify x, simplify y of
    Over a b, Over c d -> a == b && c == d

instance ordFraction :: Ord Fraction where
  compare (Over a b) (Over c d)
    = compare (a * (z / d)) (c * (z / d))
    where z = lcm b d

instance showFraction :: Show Fraction where
  show (Over a b) = show a <> " / " <> show b

instance semiringFraction :: Semiring Fraction where
  mul (Over a b) (Over c d) = Over (a * c) (b * d)

  add (Over a b) (Over c d)
    = Over (a * (z / d) + c * (z / d)) z
    where z = lcm b d

  one  = Over 1 1
  zero = Over 0 1

instance ringFraction :: Ring Fraction where
  sub x (Over a b) = add x (Over (-a) b)
