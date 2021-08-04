{-# LANGUAGE ConstraintKinds #-}
module Tests.Vector.Boxed (tests) where

import Test.Tasty
import qualified Data.Vector
import Tests.Vector.Property

import GHC.Exts (inline)


testGeneralBoxedVector :: forall a. (CommonContext a Data.Vector.Vector, Ord a, Data a) => Data.Vector.Vector a -> [Test]
testGeneralBoxedVector dummy = concatMap ($ dummy)
  [
    testSanity
  , inline testPolymorphicFunctions
  , testOrdFunctions
  , testTuplyFunctions
  , testNestedVectorFunctions
  , testMonoidFunctions
  , testFunctorFunctions
  , testMonadFunctions
  , testApplicativeFunctions
  , testAlternativeFunctions
  , testSequenceFunctions
  , testDataFunctions
  ]

testBoolBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testBoolFunctions
  ]

testNumericBoxedVector :: forall a. (CommonContext a Data.Vector.Vector, Ord a, Num a, Enum a, Random a, Data a) => Data.Vector.Vector a -> [Test]
testNumericBoxedVector dummy = concatMap ($ dummy)
  [
    testGeneralBoxedVector
  , testNumFunctions
  , testEnumFunctions
  ]

tests =
  [ testGroup "Bool" $
    testBoolBoxedVector (undefined :: Data.Vector.Vector Bool)
  , testGroup "Int" $
    testNumericBoxedVector (undefined :: Data.Vector.Vector Int)
  ]
