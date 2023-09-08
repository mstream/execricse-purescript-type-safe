module Level2
  ( Vector
  , fillVector
  , mergeVectors
  , vectorSize
  , zipVectors
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Nat (Nat(..))

-- Same as the in Level1
newtype Vector (a ∷ Type) = Vector (Array a)

-- Same as the in Level 1
instance Show a ⇒ Show (Vector a) where
  show (Vector arrayOfItems) = "Vec" <> show arrayOfItems

-- Improvements over Level 1 version:
-- - constructing a vector is guaranteed to succeed
-- - indices are guaranteed to be non-negative during mapping
fillVector ∷ ∀ a. (Nat → a) → Nat → Vector a
fillVector indexToItem size =
  Vector $ indexToItem <$> indices
  where
  indices ∷ Array Nat
  indices = Array.reverse $ unfoldr
    ( case _ of
        Zero →
          Nothing
        SuccessorOf n →
          Just $ n /\ n
    )
    size

-- Same as the in Level 1
zipVectors
  ∷ ∀ a b c. (a → b → c) → Vector a → Vector b → String \/ Vector c
zipVectors
  mergeItems
  (Vector leftArrayOfItems)
  (Vector rightArrayOfItems) =
  if Array.length leftArrayOfItems == Array.length rightArrayOfItems then
    Right
      $ Vector
      $ leftArrayOfItems `Array.zipWith mergeItems` rightArrayOfItems
  else Left "cannot zip vectors of different sizes"

-- Same as the in Level 1
mergeVectors ∷ ∀ a. Vector a → Vector a → Vector a
mergeVectors (Vector leftArrayOfItems) (Vector rightArrayOfItems) =
  Vector $ leftArrayOfItems <> rightArrayOfItems

-- Improvements over Level 1 version:
-- - returnet value is guaranteed to be non-negative
vectorSize ∷ ∀ a. Vector a → Nat
vectorSize (Vector arrayOfItems) = case Array.uncons arrayOfItems of
  Just { tail } →
    SuccessorOf $ vectorSize $ Vector tail
  Nothing →
    Zero
