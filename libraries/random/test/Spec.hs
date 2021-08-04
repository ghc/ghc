{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.ByteString.Short as SBS
import Data.Int
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Numeric.Natural (Natural)
import System.Random
import Test.SmallCheck.Series as SC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck as SC

import qualified Spec.Range as Range
import qualified Spec.Run as Run

main :: IO ()
main =
  defaultMain $
  testGroup
    "Spec"
    [ floatingSpec (Proxy :: Proxy Double)
    , floatingSpec (Proxy :: Proxy Float)
    , floatingSpec (Proxy :: Proxy CDouble)
    , floatingSpec (Proxy :: Proxy CFloat)
    , integralSpec (Proxy :: Proxy Word8)
    , integralSpec (Proxy :: Proxy Word16)
    , integralSpec (Proxy :: Proxy Word32)
    , integralSpec (Proxy :: Proxy Word64)
    , integralSpec (Proxy :: Proxy Word)
    , integralSpec (Proxy :: Proxy Int8)
    , integralSpec (Proxy :: Proxy Int16)
    , integralSpec (Proxy :: Proxy Int32)
    , integralSpec (Proxy :: Proxy Int64)
    , integralSpec (Proxy :: Proxy Int)
    , integralSpec (Proxy :: Proxy Char)
    , integralSpec (Proxy :: Proxy Bool)
#if __GLASGOW_HASKELL >= 802
    , integralSpec (Proxy :: Proxy CBool)
#endif
    , integralSpec (Proxy :: Proxy CChar)
    , integralSpec (Proxy :: Proxy CSChar)
    , integralSpec (Proxy :: Proxy CUChar)
    , integralSpec (Proxy :: Proxy CShort)
    , integralSpec (Proxy :: Proxy CUShort)
    , integralSpec (Proxy :: Proxy CInt)
    , integralSpec (Proxy :: Proxy CUInt)
    , integralSpec (Proxy :: Proxy CLong)
    , integralSpec (Proxy :: Proxy CULong)
    , integralSpec (Proxy :: Proxy CPtrdiff)
    , integralSpec (Proxy :: Proxy CSize)
    , integralSpec (Proxy :: Proxy CWchar)
    , integralSpec (Proxy :: Proxy CSigAtomic)
    , integralSpec (Proxy :: Proxy CLLong)
    , integralSpec (Proxy :: Proxy CULLong)
    , integralSpec (Proxy :: Proxy CIntPtr)
    , integralSpec (Proxy :: Proxy CUIntPtr)
    , integralSpec (Proxy :: Proxy CIntMax)
    , integralSpec (Proxy :: Proxy CUIntMax)
    , integralSpec (Proxy :: Proxy Integer)
    , integralSpec (Proxy :: Proxy Natural)
    , runSpec
    , floatTests
    , byteStringSpec
    , SC.testProperty "uniformRangeWithinExcludedF" $ seeded Range.uniformRangeWithinExcludedF
    , SC.testProperty "uniformRangeWithinExcludedD" $ seeded Range.uniformRangeWithinExcludedD
    ]

floatTests :: TestTree
floatTests = testGroup "(Float)"
  [ -- Check that https://github.com/haskell/random/issues/53 does not regress

    testCase "Subnormal generation not above upper bound" $
    [] @?= filter (>4.0e-45) (take 100000 $ randomRs (0, 4.0e-45::Float) $ mkStdGen 0)

  , testCase "Subnormal generation includes upper bound" $
    1.0e-45 `elem` take 100 (randomRs (0, 1.0e-45::Float) $ mkStdGen 0) @?
    "Does not contain 1.0e-45"
  ]

showsType :: forall t . Typeable t => Proxy t -> ShowS
showsType px = showsTypeRep (typeRep px)

byteStringSpec :: TestTree
byteStringSpec =
  testGroup
    "ByteString"
    [ SC.testProperty "genShortByteString" $ \(seed, n8) ->
        let n = fromIntegral (n8 :: Word8) -- no need to generate huge collection of bytes
         in SBS.length (fst (seeded (genShortByteString n) seed)) == n
    , SC.testProperty "genByteString" $ \(seed, n8) ->
        let n = fromIntegral (n8 :: Word8)
         in SBS.toShort (fst (seeded (genByteString n) seed)) ==
            fst (seeded (genShortByteString n) seed)
    ]


rangeSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, UniformRange a, Show a)
  => Proxy a -> TestTree
rangeSpec px =
  testGroup ("Range (" ++ showsType px ")")
  [ SC.testProperty "uniformR" $ seeded $ Range.uniformRangeWithin px
  ]

integralSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Ord a, UniformRange a, Show a)
  => Proxy a -> TestTree
integralSpec px =
  testGroup ("(" ++ showsType px ")")
  [ SC.testProperty "symmetric" $ seeded $ Range.symmetric px
  , SC.testProperty "bounded" $ seeded $ Range.bounded px
  , SC.testProperty "singleton" $ seeded $ Range.singleton px
  , rangeSpec px
  -- TODO: Add more tests
  ]

floatingSpec ::
     forall a.
     (SC.Serial IO a, Typeable a, Num a, Ord a, Random a, UniformRange a, Show a)
  => Proxy a -> TestTree
floatingSpec px =
  testGroup ("(" ++ showsType px ")")
  [ SC.testProperty "uniformR" $ seeded $ Range.uniformRangeWithin px
  -- TODO: Add more tests
  ]

runSpec :: TestTree
runSpec = testGroup "runGenState_ and runPrimGenIO_"
    [ SC.testProperty "equal outputs" $ seeded $ \g -> monadic $ Run.runsEqual g ]

-- | Create a StdGen instance from an Int and pass it to the given function.
seeded :: (StdGen -> a) -> Int -> a
seeded f = f . mkStdGen
