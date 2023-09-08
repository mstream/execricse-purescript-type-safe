module Level1
  ( Vector
  , fillVector
  , mergeVectors
  , vectorSize
  , zipVectors
  ) where

import Prelude

import Data.Array ((..))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))

-- The newtype keyword introduces a compile-level wrapper around
-- the Array type.
-- The purpose of it is to prevent the user of that library from
-- creating Array instances directly. They are restricted to use
-- provided API functions such as 'fillVector'
newtype Vector (a ∷ Type) = Vector (Array a)

-- This instance provides a representation for a Vector instance
-- in a form of a String. This is usually used for debugging purposes.
-- The 'Show a' constraint allows to show only vectors containing
-- elements which can be shown themselves. This way we can include
-- vector contents.
instance Show a ⇒ Show (Vector a) where
  show (Vector arrayOfItems) = "Vec" <> show arrayOfItems

-- This is a way of constructing a vector by providing its desired
-- size and a function which converts its indices to values.
-- This way we make sure that every element in the vector is present.
-- Creating vectors of negative size is impossible.
fillVector ∷ ∀ a. (Int → a) → Int → String \/ (Vector a)
fillVector indexToItem size =
  if size < 0 then Left "cannot create a vector of negative size"
  else Right $ Vector $ indexToItem <$> 0 .. (size - 1)

-- This function takes two vectors, a merging function and produces
-- a vector which is a result of applying the merging function to
-- elements with corresponding indices.
-- It is possible to merge vectors of different types and the resulting
-- vector may have elements of type different from type of the input
-- vectors as well.
-- To successfully zip two vectors, their sizes must be equal.
zipVectors
  ∷ ∀ a b c. (a → b → c) → Vector a → Vector b → String \/ Vector c
zipVectors
  mergeItems
  (Vector leftArrayOfItems)
  (Vector rightArrayOfItems) =
  if Array.length leftArrayOfItems == Array.length rightArrayOfItems then
    Right $ Vector $ leftArrayOfItems `Array.zipWith mergeItems`
      rightArrayOfItems
  else Left "cannot zip vectors of different sizes"

-- This function takes two vectors and append them so the size of the
-- resulting vector is the sum of the input vectors.
-- This operation always succeeds but the types of vectors must be
-- the same. This constraint can be enforced by the compiler.
mergeVectors ∷ ∀ a. Vector a → Vector a → Vector a
mergeVectors (Vector leftArrayOfItems) (Vector rightArrayOfItems) =
  Vector $ leftArrayOfItems <> rightArrayOfItems

-- This function returns a vector size.
vectorSize ∷ ∀ a. Vector a → Int
vectorSize (Vector arrayOfItems) = Array.length arrayOfItems
