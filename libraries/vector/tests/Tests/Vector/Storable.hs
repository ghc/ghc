{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Storable (tests) where

import Test.Tasty
import qualified Data.Vector.Storable
import Tests.Vector.Property

import GHC.Exts (inline)

testGeneralStorableVector :: forall a. (CommonContext a Data.Vector.Storable.Vector, Data.Vector.Storable.Storable a, Ord a, Data a) => Data.Vector.Storable.Vector a -> [Test]
testGeneralStorableVector dummy = concatMap ($ dummy)
  [
    testSanity
  , inline testPolymorphicFunctions
  , testOrdFunctions
  , testMonoidFunctions
  , testDataFunctions
  ]

testNumericStorableVector :: forall a. (CommonContext a Data.Vector.Storable.Vector, Data.Vector.Storable.Storable a, Ord a, Num a, Enum a, Random a, Data a) => Data.Vector.Storable.Vector a -> [Test]
testNumericStorableVector dummy = concatMap ($ dummy)
  [
    testGeneralStorableVector
  , testNumFunctions
  , testEnumFunctions
  ]

tests =
  [ testGroup "Data.Vector.Storable.Vector (Int)" $
    testNumericStorableVector (undefined :: Data.Vector.Storable.Vector Int)
  , testGroup "Data.Vector.Storable.Vector (Double)" $
    testNumericStorableVector (undefined :: Data.Vector.Storable.Vector Double)
  ]
