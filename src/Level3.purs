module Level3
  ( class IsNatKind
  , class NatKindsAddUpTo
  , N0
  , N1
  , N2
  , N3
  , NatKind
  , NatKindZero
  , NatKindSuccessorOf
  , Vector
  , fillVector
  , mergeVectors
  , reflectNatKind
  , vectorSize
  , zipVectors
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Nat (Nat(..))
import Type.Proxy (Proxy(..))

-- A custom kind which represents type-level natural number values.
data NatKind

-- Countrary to the Nat value-level type, where all possible
-- constructors were defined during the custom type declaration,
-- it is possible to create additional type-level values of NatKind.
foreign import data NatKindZero ∷ NatKind
foreign import data NatKindSuccessorOf ∷ NatKind → NatKind

-- Any type-level value (not only of NatKind kind), for which instance
-- of this class exists, can be reflected into a value-level
-- value of Nat type during runtime.
-- 'Proxy' acts here as carrier for the type-level value as such cannot
-- be constructed and passed to functions.
class IsNatKind ∷ ∀ k. k → Constraint
class IsNatKind a where
  reflectNatKind ∷ Proxy a → Nat

instance IsNatKind NatKindZero where
  -- Because Proxy is a singleton, its value does not carry any useful
  -- information at runtime, therefore '_' wildcard is used to signify
  -- its unimportance.
  reflectNatKind _ = Zero

instance (IsNatKind n) ⇒ IsNatKind (NatKindSuccessorOf n) where
  -- Using recursion at the type checker level, we can reflect any
  -- type-value natural number into a value-level natural number.
  reflectNatKind _ = SuccessorOf $ reflectNatKind (Proxy ∷ Proxy n)

-- This class does not require instances to provide implementation
-- of any function. It's solely used for determining if a combination
-- of 3 type-level values of NatKind kind are allowed.
-- The 'mergeVectors' function uses such a constraint to make sure
-- than produced value type is compatible with types of its inputs.
class
  NatKindsAddUpTo (n ∷ NatKind) (m ∷ NatKind) (nm ∷ NatKind)
  | n m → nm

-- This configuration is the terminal step of type-level recursion.
-- It can be thought of as 0 + a = a which holds true for any 'a'.
instance NatKindsAddUpTo NatKindZero a a

-- This configuration is a recursive step of type-level recursion.
-- It can be thought of a + b = c if (a - 1) + b = (c - 1)
instance
  NatKindsAddUpTo a b c ⇒
  NatKindsAddUpTo (NatKindSuccessorOf a) b (NatKindSuccessorOf c)

type N0 = NatKindZero
type N1 = NatKindSuccessorOf N0
type N2 = NatKindSuccessorOf N1
type N3 = NatKindSuccessorOf N2

-- Similar to the one in Level 1 and 2 but additionally,
-- its type is parametrized with a type-level value of kind 'NatKind'.
-- This additional parameter is erased during compilation and it is
-- not present during runtime but compiler can use it for checking
-- additional constraints. Such a type parameter is called 'phantom.
newtype Vector (n ∷ NatKind) (a ∷ Type) = Vector (Array a)

-- Same as the in Level1
instance Show a ⇒ Show (Vector n a) where
  show (Vector arrayOfItems) = "Vec" <> show arrayOfItems

-- Improvements over Level 2 version:
-- - it does not require to pass vector's length as it is already
--   known at the compilation time
fillVector ∷ ∀ a n. IsNatKind n ⇒ (Nat → a) → Vector n a
fillVector indexToItem =
  Vector $ indexToItem <$> indices
  where
  indices ∷ Array Nat
  -- The array of indices has to be reversed as unwrapping of the
  -- natural number value dictates the decscending order.
  indices = Array.reverse $ unfoldr
    ( case _ of
        Zero →
          Nothing
        SuccessorOf n →
          Just $ n /\ n
    )
    (reflectNatKind (Proxy ∷ Proxy n))

-- Improvements over Level 2 version:
-- - always succeeds as compatibility of the inputs can be determined
--   at the compilation time
zipVectors
  ∷ ∀ a b c n. (a → b → c) → Vector n a → Vector n b → Vector n c
zipVectors
  mergeItems
  (Vector leftArrayOfItems)
  (Vector rightArrayOfItems) =
  Vector $ leftArrayOfItems `Array.zipWith mergeItems` rightArrayOfItems

-- Improvements over Level 2 version:
-- - apart from the runtime value, the type-level value is computed
--   so the resulting vector's length is known at the compilatio time
mergeVectors
  ∷ ∀ a m n nm
  . NatKindsAddUpTo n m nm
  ⇒ Vector n a
  → Vector m a
  → Vector nm a
mergeVectors (Vector leftArrayOfItems) (Vector rightArrayOfItems) =
  Vector $ leftArrayOfItems <> rightArrayOfItems

-- Although possible, size calculation does not have to involve runtime
-- value as it can be calculated even before program execution.
vectorSize ∷ ∀ a n. IsNatKind n ⇒ Vector n a → Nat
vectorSize _ = reflectNatKind (Proxy ∷ Proxy n)
