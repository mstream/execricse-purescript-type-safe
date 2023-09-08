module Main (main) where

import Prelude

import Data.Either.Nested (type (\/))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console as Console
import Level1 as L1
import Level2 as L2
import Level3 as L3
import Nat as Nat

main ∷ Effect Unit
main = do
  logLevel1
  logLevel2
  logLevel3

logLevel1 ∷ Effect Unit
logLevel1 = do
  logLevel 1

  log labels.vec2
    l1.vec2

  log labels.sizeOfVec2
    $ L1.vectorSize <$> l1.vec2

  log labels.vec3
    l1.vec3

  log labels.sizeOfVec3
    $ L1.vectorSize <$> l1.vec3

  log labels.vecNeg2
    l1.vecNeg2

  log labels.sizeOfVecNeg2
    $ L1.vectorSize <$> l1.vecNeg2

  log labels.vec2MergeVec3 do
    leftVector ← l1.vec2
    rightVector ← l1.vec3
    pure $ leftVector `L1.mergeVectors` rightVector

  log labels.sizeOfVec2MergeVec3 do
    leftVector ← l1.vec2
    rightVector ← l1.vec3
    pure $ L1.vectorSize $ leftVector `L1.mergeVectors` rightVector

  log labels.vec3ZipVec3 do
    leftVector ← l1.vec3
    rightVector ← l1.vec3
    leftVector `L1.zipVectors (+)` rightVector

  log labels.vec2ZipVec3 do
    leftVector ← l1.vec2
    rightVector ← l1.vec3
    leftVector `L1.zipVectors (+)` rightVector

  log labels.vec2MergeVec3ZipVec2MergeVec3 do
    vec2 ← l1.vec2
    vec3 ← l1.vec3
    (vec2 `L1.mergeVectors` vec3)
      `L1.zipVectors (+)`
        (vec2 `L1.mergeVectors` vec3)

  log labels.vec2MergeVec3ZipVec3MergeVec3 do
    vec2 ← l1.vec2
    vec3 ← l1.vec3
    (vec2 `L1.mergeVectors` vec3)
      `L1.zipVectors (+)`
        (vec3 `L1.mergeVectors` vec3)

logLevel2 ∷ Effect Unit
logLevel2 = do
  logLevel 2

  log labels.vec2
    l2.vec2

  log labels.sizeOfVec2
    $ L2.vectorSize l2.vec2

  log labels.vec3
    l2.vec3

  log labels.sizeOfVec3
    $ L2.vectorSize l2.vec3

  log labels.vecNeg2
    "<impossible to instantiate>"

  log labels.vec2MergeVec3
    $ l2.vec2 `L2.mergeVectors` l2.vec3

  log labels.sizeOfVec2MergeVec3
    $ L2.vectorSize
    $ l2.vec2 `L2.mergeVectors` l2.vec3

  log labels.vec3ZipVec3
    $ l2.vec3 `L2.zipVectors (+)` l2.vec3

  log labels.vec2ZipVec3
    $ l2.vec2 `L2.zipVectors (+)` l2.vec3

  log labels.vec2MergeVec3ZipVec2MergeVec3
    $ (l2.vec2 `L2.mergeVectors` l2.vec3)
        `L2.zipVectors (+)`
          (l2.vec2 `L2.mergeVectors` l2.vec3)

  log labels.vec2MergeVec3ZipVec3MergeVec3
    $ (l2.vec2 `L2.mergeVectors` l2.vec3)
        `L2.zipVectors (+)`
          (l2.vec3 `L2.mergeVectors` l2.vec3)

logLevel3 ∷ Effect Unit
logLevel3 = do
  logLevel 3

  log labels.vec2
    $ l3.vec2

  log labels.sizeOfVec2
    $ L3.vectorSize l3.vec2

  log labels.vec3
    $ l3.vec3

  log labels.sizeOfVec3
    $ L3.vectorSize l3.vec3

  log labels.vecNeg2
    "<impossible to instantiate>"

  log labels.vec2MergeVec3
    $ l3.vec2 `L3.mergeVectors` l3.vec3

  log labels.sizeOfVec2MergeVec3
    $ L3.vectorSize
    $ l3.vec2 `L3.mergeVectors` l3.vec3

  log labels.vec3ZipVec3
    $ l3.vec3 `L3.zipVectors (+)` l3.vec3

  log labels.vec2ZipVec3
    "<impossible to compute>"

  {- this will not compile
  log labels.vec2ZipVec3
    $ l3.vec2 `L3.zipVectors (+)` l3.vec3
  -}

  log labels.vec2MergeVec3ZipVec2MergeVec3
    $ (l3.vec2 `L3.mergeVectors` l3.vec3)
        `L3.zipVectors (+)`
          (l3.vec2 `L3.mergeVectors` l3.vec3)

  {- this will not compile
  log labels.vec2MergeVec3ZipVec3MergeVec3
    $ (l3.vec2 `L3.mergeVectors` l3.vec3)
        `L3.zipVectors (+)`
          (l3.vec3 `L3.mergeVectors` l3.vec3)
  -}

  pure unit

l1
  ∷ { vec2 ∷ String \/ (L1.Vector Int)
    , vec3 ∷ String \/ (L1.Vector Int)
    , vecNeg2 ∷ String \/ (L1.Vector Int)
    }
l1 =
  { vec2: L1.fillVector identity 2
  , vec3: L1.fillVector identity 3
  , vecNeg2: L1.fillVector identity (-2)
  }

l2
  ∷ { vec2 ∷ L2.Vector Int
    , vec3 ∷ L2.Vector Int
    }
l2 =
  { vec2: L2.fillVector Nat.natToInt Nat.n2
  , vec3: L2.fillVector Nat.natToInt Nat.n3
  }

l3
  ∷ { vec2 ∷ L3.Vector L3.N2 Int
    , vec3 ∷ L3.Vector L3.N3 Int
    }
l3 =
  { vec2: L3.fillVector Nat.natToInt
  , vec3: L3.fillVector Nat.natToInt
  }

logLevel ∷ ∀ m. MonadEffect m ⇒ Int → m Unit
logLevel level = do
  Console.log "\n---"
  Console.log $ "Level " <> show level
  Console.log "---\n"

log ∷ ∀ m a. MonadEffect m ⇒ String → Show a ⇒ a → m Unit
log label value = Console.log $ label <> ":\n  " <> show value <> "\n"

labels
  ∷ { sizeOfVec2 ∷ String
    , sizeOfVec2MergeVec3 ∷ String
    , sizeOfVec3 ∷ String
    , sizeOfVecNeg2 ∷ String
    , vec2 ∷ String
    , vec2MergeVec3 ∷ String
    , vec2MergeVec3ZipVec2MergeVec3 ∷ String
    , vec2MergeVec3ZipVec3MergeVec3 ∷ String
    , vec2ZipVec3 ∷ String
    , vec3 ∷ String
    , vec3ZipVec3 ∷ String
    , vecNeg2 ∷ String
    }
labels =
  { vec2: "vec2"
  , vec3: "vec3"
  , vecNeg2: "vec(-2)"
  , sizeOfVec2: "sizeOf(vec2)"
  , sizeOfVec3: "sizeOf(vec3)"
  , sizeOfVecNeg2: "sizeOf(vec(-2))"
  , sizeOfVec2MergeVec3: "sizeOf(vec2 `merge` vec3)"
  , vec2MergeVec3: "vec2 `merge` vec3"
  , vec2ZipVec3: "vec2 `zip` vec3"
  , vec3ZipVec3: "vec3 `zip` vec3"
  , vec2MergeVec3ZipVec2MergeVec3:
      "(vec2 `merge` vec3) `zip` (vec2 `merge` vec3)"
  , vec2MergeVec3ZipVec3MergeVec3:
      "(vec2 `merge` vec3) `zip` (vec3 `merge` vec3)"
  }
