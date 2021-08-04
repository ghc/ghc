{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Unboxed (tests) where

import Test.Tasty
import qualified Data.Vector.Unboxed
import Tests.Vector.Property



testGeneralUnboxedVector :: forall a. (CommonContext a Data.Vector.Unboxed.Vector, Data.Vector.Unboxed.Unbox a, Ord a, Data a) => Data.Vector.Unboxed.Vector a -> [Test]
testGeneralUnboxedVector dummy = concatMap ($ dummy)
  [
    testSanity
  , testPolymorphicFunctions
  , testOrdFunctions
  , testTuplyFunctions
  , testMonoidFunctions
  , testDataFunctions
  ]

testUnitUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

testBoolUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testBoolFunctions
  ]

testNumericUnboxedVector :: forall a. (CommonContext a Data.Vector.Unboxed.Vector, Data.Vector.Unboxed.Unbox a, Ord a, Num a, Enum a, Random a, Data a) => Data.Vector.Unboxed.Vector a -> [Test]
testNumericUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  , testNumFunctions
  , testEnumFunctions
  ]

testTupleUnboxedVector :: forall a. (CommonContext a Data.Vector.Unboxed.Vector, Data.Vector.Unboxed.Unbox a, Ord a, Data a) => Data.Vector.Unboxed.Vector a -> [Test]
testTupleUnboxedVector dummy = concatMap ($ dummy)
  [
    testGeneralUnboxedVector
  ]

tests =
  [ testGroup "()" $
    testUnitUnboxedVector (undefined :: Data.Vector.Unboxed.Vector ())
  , testGroup "(Bool)" $
    testBoolUnboxedVector (undefined :: Data.Vector.Unboxed.Vector Bool)
  , testGroup "(Int)" $
    testNumericUnboxedVector (undefined :: Data.Vector.Unboxed.Vector Int)
  , testGroup "(Float)" $
  testNumericUnboxedVector (undefined :: Data.Vector.Unboxed.Vector Float)
  , testGroup "(Double)" $
    testNumericUnboxedVector (undefined :: Data.Vector.Unboxed.Vector Double)
  , testGroup "(Int,Bool)" $
    testTupleUnboxedVector (undefined :: Data.Vector.Unboxed.Vector (Int, Bool))
  , testGroup "(Int,Bool,Int)" $
    testTupleUnboxedVector
      (undefined :: Data.Vector.Unboxed.Vector (Int, Bool, Int))
  ]
