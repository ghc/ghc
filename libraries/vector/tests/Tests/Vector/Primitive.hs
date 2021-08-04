{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Primitive (tests) where

import Test.Tasty
import qualified Data.Vector.Primitive
import Tests.Vector.Property

import GHC.Exts (inline)

testGeneralPrimitiveVector :: forall a. (CommonContext a Data.Vector.Primitive.Vector, Data.Vector.Primitive.Prim a, Ord a, Data a) => Data.Vector.Primitive.Vector a -> [Test]
testGeneralPrimitiveVector dummy = concatMap ($ dummy)
  [
    testSanity
  , inline testPolymorphicFunctions
  , testOrdFunctions
  , testMonoidFunctions
  , testDataFunctions
  ]

testNumericPrimitiveVector :: forall a. (CommonContext a Data.Vector.Primitive.Vector, Data.Vector.Primitive.Prim a, Ord a, Num a, Enum a, Random a, Data a) => Data.Vector.Primitive.Vector a -> [Test]
testNumericPrimitiveVector dummy = concatMap ($ dummy)
  [
    testGeneralPrimitiveVector
  , testNumFunctions
  , testEnumFunctions
  ]

tests =
  [ testGroup "Int" $
    testNumericPrimitiveVector (undefined :: Data.Vector.Primitive.Vector Int)
  , testGroup "Double" $
    testNumericPrimitiveVector
      (undefined :: Data.Vector.Primitive.Vector Double)
  ]
