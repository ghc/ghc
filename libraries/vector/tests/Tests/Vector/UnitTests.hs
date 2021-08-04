{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Vector.UnitTests (tests) where

import Control.Applicative as Applicative
import Control.Exception
import Control.Monad.Primitive
import Control.Monad.Fix (mfix)
import Data.Int
import Data.Word
import Data.Typeable
import qualified Data.List as List
import qualified Data.Vector.Generic  as Generic
import qualified Data.Vector as Boxed
import qualified Data.Vector.Mutable as MBoxed
import qualified Data.Vector.Primitive as Primitive
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit (testCase, Assertion, assertBool, assertEqual, (@=?), assertFailure)


newtype Aligned a = Aligned { getAligned :: a }

instance (Storable a) => Storable (Aligned a) where
  sizeOf _    = sizeOf (undefined :: a)
  alignment _ = 128
  peek ptr    = Aligned Applicative.<$> peek (castPtr ptr)
  poke ptr    = poke (castPtr ptr) . getAligned

checkAddressAlignment :: forall a. (Storable a) => Storable.Vector a -> Assertion
checkAddressAlignment xs = Storable.unsafeWith xs $ \ptr -> do
  let ptr'  = ptrToWordPtr ptr
      msg   = printf "Expected pointer with alignment %d but got 0x%08x" (toInteger align) (toInteger ptr')
      align :: WordPtr
      align = fromIntegral $ alignment dummy
  assertBool msg $ (ptr' `mod` align) == 0
  where
    dummy :: a
    dummy = undefined

tests :: [TestTree]
tests =
  [ testGroup "Data.Vector.Storable.Vector Alignment"
      [ testCase "Aligned Double" $
          checkAddressAlignment alignedDoubleVec
      , testCase "Aligned Int" $
          checkAddressAlignment alignedIntVec
      ]
  , testGroup "Regression tests"
    [ testGroup "enumFromTo crash #188"
      [ regression188 ([] :: [Word8])
      , regression188 ([] :: [Word16])
      , regression188 ([] :: [Word32])
      , regression188 ([] :: [Word64])
      , regression188 ([] :: [Word])
      , regression188 ([] :: [Int8])
      , regression188 ([] :: [Int16])
      , regression188 ([] :: [Int32])
      , regression188 ([] :: [Int64])
      , regression188 ([] :: [Int])
      , regression188 ([] :: [Char])
      ]
    ]
  , testGroup "Negative tests"
    [ testGroup "slice out of bounds #257"
      [ testGroup "Boxed" $ testsSliceOutOfBounds Boxed.slice
      , testGroup "Primitive" $ testsSliceOutOfBounds Primitive.slice
      , testGroup "Storable" $ testsSliceOutOfBounds Storable.slice
      , testGroup "Unboxed" $ testsSliceOutOfBounds Unboxed.slice
      ]
    , testGroup "take #282"
      [ testCase "Boxed" $ testTakeOutOfMemory Boxed.take
      , testCase "Primitive" $ testTakeOutOfMemory Primitive.take
      , testCase "Storable" $ testTakeOutOfMemory Storable.take
      , testCase "Unboxed" $ testTakeOutOfMemory Unboxed.take
      ]
    ]
  , testGroup "Data.Vector"
    [ testCase "MonadFix" checkMonadFix
    , testCase "toFromArray" toFromArray
    , testCase "toFromMutableArray" toFromMutableArray
    ]
  ]

testsSliceOutOfBounds ::
     (Show (v Int), Generic.Vector v Int) => (Int -> Int -> v Int -> v Int) -> [TestTree]
testsSliceOutOfBounds sliceWith =
  [ testCase "Negative ix" $ sliceTest sliceWith (-2) 2 xs
  , testCase "Negative size" $ sliceTest sliceWith 2 (-2) xs
  , testCase "Negative ix and size" $ sliceTest sliceWith (-2) (-1) xs
  , testCase "Too large ix" $ sliceTest sliceWith 6 2 xs
  , testCase "Too large size" $ sliceTest sliceWith 2 6 xs
  , testCase "Too large ix and size" $ sliceTest sliceWith 6 6 xs
  , testCase "Overflow" $ sliceTest sliceWith 1 maxBound xs
  , testCase "OutOfMemory" $ sliceTest sliceWith 1 (maxBound `div` intSize) xs
  ]
  where
    intSize = sizeOf (undefined :: Int)
    xs = [1, 2, 3, 4, 5] :: [Int]
{-# INLINE testsSliceOutOfBounds #-}

sliceTest ::
     (Show (v Int), Generic.Vector v Int)
  => (Int -> Int -> v Int -> v Int)
  -> Int
  -> Int
  -> [Int]
  -> Assertion
sliceTest sliceWith i m xs = do
  let vec = Generic.fromList xs
  eRes <- try (pure $! sliceWith i m vec)
  case eRes of
    Right v ->
      assertFailure $
      "Data.Vector.Internal.Check.checkSlice failed to check: " ++ show v
    Left (ErrorCall err) ->
      let assertMsg =
            List.concat
              [ "Expected slice function to produce an 'error' ending with: \""
              , errSuffix
              , "\" instead got: \""
              , err
              ]
       in assertBool assertMsg (errSuffix `List.isSuffixOf` err)
  where
    errSuffix =
      "(slice): invalid slice (" ++
      show i ++ "," ++ show m ++ "," ++ show (List.length xs) ++ ")"
{-# INLINE sliceTest #-}

testTakeOutOfMemory ::
     (Show (v Int), Eq (v Int), Generic.Vector v Int) => (Int -> v Int -> v Int) -> Assertion
testTakeOutOfMemory takeWith =
  takeWith (maxBound `div` intSize) (Generic.fromList xs) @=? Generic.fromList xs
  where
    intSize = sizeOf (undefined :: Int)
    xs = [1, 2, 3, 4, 5] :: [Int]
{-# INLINE testTakeOutOfMemory #-}

regression188
  :: forall proxy a. (Typeable a, Enum a, Bounded a, Eq a, Show a)
  => proxy a -> TestTree
regression188 _ = testCase (show (typeOf (undefined :: a)))
  $ Boxed.fromList [maxBound::a] @=? Boxed.enumFromTo maxBound maxBound
{-# INLINE regression188 #-}

alignedDoubleVec :: Storable.Vector (Aligned Double)
alignedDoubleVec = Storable.fromList $ map Aligned [1, 2, 3, 4, 5]

alignedIntVec :: Storable.Vector (Aligned Int)
alignedIntVec = Storable.fromList $ map Aligned [1, 2, 3, 4, 5]

#if __GLASGOW_HASKELL__ >= 800
-- Ensure that Mutable is really an injective type family by typechecking a
-- function which relies on injectivity.
_f :: (Generic.Vector v a, Generic.Vector w a, PrimMonad f)
   => Generic.Mutable v (PrimState f) a -> f (w a)
_f v = Generic.convert `fmap` Generic.unsafeFreeze v
#endif

checkMonadFix :: Assertion
checkMonadFix = assertBool "checkMonadFix" $
    Boxed.toList fewV == fewL &&
    Boxed.toList none == []
  where
    facty _ 0 = 1; facty f n = n * f (n - 1)
    fewV :: Boxed.Vector Int
    fewV = fmap ($ 12) $ mfix (\i -> Boxed.fromList [facty i, facty (+1), facty (+2)])
    fewL :: [Int]
    fewL = fmap ($ 12) $ mfix (\i -> [facty i, facty (+1), facty (+2)])
    none :: Boxed.Vector Int
    none = mfix (const Boxed.empty)

mkArrayRoundtrip :: (String -> Boxed.Vector Integer -> Assertion) -> Assertion
mkArrayRoundtrip mkAssertion =
  sequence_
    [ mkAssertion name v
    | (name, v) <-
        [ ("full", vec)
        , ("slicedTail", Boxed.slice 0 (n - 3) vec)
        , ("slicedHead", Boxed.slice 2 (n - 2) vec)
        , ("slicedBoth", Boxed.slice 2 (n - 4) vec)
        ]
    ]
  where
    vec = Boxed.fromList [0 .. 10]
    n = Boxed.length vec

toFromArray :: Assertion
toFromArray =
  mkArrayRoundtrip $ \name v ->
    assertEqual name v $ Boxed.fromArray (Boxed.toArray v)

toFromMutableArray :: Assertion
toFromMutableArray = mkArrayRoundtrip assetRoundtrip
  where
    assetRoundtrip assertionName vec = do
      mvec <- Boxed.unsafeThaw vec
      mvec' <- MBoxed.fromMutableArray =<< MBoxed.toMutableArray mvec
      vec' <- Boxed.unsafeFreeze mvec'
      assertEqual assertionName vec vec'
