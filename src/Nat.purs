module Nat
  ( Nat(..)
  , n0
  , n1
  , n2
  , n3
  , addNat
  , natToInt
  ) where

import Prelude

-- A custom type with a Zero constructor which represents value 0 and
-- a recursive constructor SuccessorOf n which represents a value
-- 1 whole number greater than any given Nat value n.
data Nat = Zero | SuccessorOf Nat

derive instance Eq Nat

instance Show Nat where
  show = case _ of
    Zero →
      "Z"
    SuccessorOf n →
      "S(" <> show n <> ")"

-- This function adds two natural numbers.
addNat ∷ Nat → Nat → Nat
addNat = case _, _ of
  Zero, other →
    other
  other, Zero →
    other
  l, SuccessorOf r →
    SuccessorOf l `addNat` r

-- This functions converts a natural number into an integer.
natToInt ∷ Nat → Int
natToInt = case _ of
  Zero →
    0
  SuccessorOf n →
    1 + natToInt n

n0 ∷ Nat
n0 = Zero

n1 ∷ Nat
n1 = SuccessorOf n0

n2 ∷ Nat
n2 = SuccessorOf n1

n3 ∷ Nat
n3 = SuccessorOf n2
