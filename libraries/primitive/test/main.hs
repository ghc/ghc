{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeInType #-}
#endif

import Control.Monad
import Control.Monad.ST
import Data.Primitive
import Data.Word
import Data.Proxy (Proxy(..))
import GHC.Int
import GHC.IO
import GHC.Exts
import Data.Function (on)
import Control.Applicative (Const(..))
import PrimLaws (primLaws)

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
#if MIN_VERSION_base(4,8,0)
import Data.Functor.Identity (Identity(..))
import qualified Data.Monoid as Monoid
#endif
#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
#else
import GHC.Exts (Down(..))
#endif
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (stimes)
import qualified Data.Semigroup as Semigroup
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
#if __GLASGOW_HASKELL__ >= 805
import Foreign.Storable (Storable)
#endif
import Data.Orphans ()

import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.QuickCheck (Arbitrary,Arbitrary1,Gen,CoArbitrary,Function,(===),(==>))
import qualified Test.Tasty.QuickCheck as TQC
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Classes.Base as QCC
import qualified Test.QuickCheck.Classes.Base.IsList as QCCL
import qualified Data.List as L

main :: IO ()
main = do
  testArray
  testByteArray
  defaultMain $ testGroup "properties"
    [ testGroup "Array"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (Array Int)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (Array Int)))
      , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (Array Int)))
      , lawsToTest (QCC.showReadLaws (Proxy :: Proxy (Array Int)))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
      , lawsToTest (QCC.functorLaws (Proxy1 :: Proxy1 Array))
      , lawsToTest (QCC.applicativeLaws (Proxy1 :: Proxy1 Array))
      , lawsToTest (QCC.monadLaws (Proxy1 :: Proxy1 Array))
      , lawsToTest (QCC.foldableLaws (Proxy1 :: Proxy1 Array))
      , lawsToTest (QCC.traversableLaws (Proxy1 :: Proxy1 Array))
#endif
#if MIN_VERSION_base(4,7,0)
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (Array Int)))
      , TQC.testProperty "mapArray'" (QCCL.mapProp int16 int32 mapArray')
#endif
      ]
    , testGroup "SmallArray"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (SmallArray Int)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (SmallArray Int)))
      , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (SmallArray Int)))
      , lawsToTest (QCC.showReadLaws (Proxy :: Proxy (Array Int)))
#if MIN_VERSION_base(4,9,0) || MIN_VERSION_transformers(0,4,0)
      , lawsToTest (QCC.functorLaws (Proxy1 :: Proxy1 SmallArray))
      , lawsToTest (QCC.applicativeLaws (Proxy1 :: Proxy1 SmallArray))
      , lawsToTest (QCC.monadLaws (Proxy1 :: Proxy1 SmallArray))
      , lawsToTest (QCC.foldableLaws (Proxy1 :: Proxy1 SmallArray))
      , lawsToTest (QCC.traversableLaws (Proxy1 :: Proxy1 SmallArray))
#endif
#if MIN_VERSION_base(4,7,0)
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (SmallArray Int)))
      , TQC.testProperty "mapSmallArray'" (QCCL.mapProp int16 int32 mapSmallArray')
#endif
      ]
    , testGroup "ByteArray"
      [ testGroup "Ordering"
        [ TQC.testProperty "equality" byteArrayEqProp
        , TQC.testProperty "compare" byteArrayCompareProp
      , testGroup "Filling"
        [ TQC.testProperty "Int8" (setByteArrayProp (Proxy :: Proxy Int8))
        , TQC.testProperty "Int16" (setByteArrayProp (Proxy :: Proxy Int16))
        , TQC.testProperty "Int32" (setByteArrayProp (Proxy :: Proxy Int32))
        , TQC.testProperty "Int64" (setByteArrayProp (Proxy :: Proxy Int64))
        , TQC.testProperty "Int" (setByteArrayProp (Proxy :: Proxy Int))
        , TQC.testProperty "Word8" (setByteArrayProp (Proxy :: Proxy Word8))
        , TQC.testProperty "Word16" (setByteArrayProp (Proxy :: Proxy Word16))
        , TQC.testProperty "Word32" (setByteArrayProp (Proxy :: Proxy Word32))
        , TQC.testProperty "Word64" (setByteArrayProp (Proxy :: Proxy Word64))
        , TQC.testProperty "Word" (setByteArrayProp (Proxy :: Proxy Word))
        ]
      ]
      , testGroup "Resize"
        [ TQC.testProperty "shrink" byteArrayShrinkProp
        , TQC.testProperty "grow" byteArrayGrowProp
        ]
      , lawsToTest (QCC.eqLaws (Proxy :: Proxy ByteArray))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy ByteArray))
      , lawsToTest (QCC.showReadLaws (Proxy :: Proxy (Array Int)))
#if MIN_VERSION_base(4,7,0)
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy ByteArray))
      , TQC.testProperty "foldrByteArray" (QCCL.foldrProp word8 foldrByteArray)
#endif
      ]
    , testGroup "PrimArray"
      [ lawsToTest (QCC.eqLaws (Proxy :: Proxy (PrimArray Word16)))
      , lawsToTest (QCC.ordLaws (Proxy :: Proxy (PrimArray Word16)))
      , lawsToTest (QCC.monoidLaws (Proxy :: Proxy (PrimArray Word16)))
#if MIN_VERSION_base(4,7,0)
      , lawsToTest (QCC.isListLaws (Proxy :: Proxy (PrimArray Word16)))
      , TQC.testProperty "foldrPrimArray" (QCCL.foldrProp int16 foldrPrimArray)
      , TQC.testProperty "foldrPrimArray'" (QCCL.foldrProp int16 foldrPrimArray')
      , TQC.testProperty "foldlPrimArray" (QCCL.foldlProp int16 foldlPrimArray)
      , TQC.testProperty "foldlPrimArray'" (QCCL.foldlProp int16 foldlPrimArray')
      , TQC.testProperty "foldlPrimArrayM'" (QCCL.foldlMProp int16 foldlPrimArrayM')
      , TQC.testProperty "mapPrimArray" (QCCL.mapProp int16 int32 mapPrimArray)
      , TQC.testProperty "traversePrimArray" (QCCL.traverseProp int16 int32 traversePrimArray)
      , TQC.testProperty "traversePrimArrayP" (QCCL.traverseProp int16 int32 traversePrimArrayP)
      , TQC.testProperty "imapPrimArray" (QCCL.imapProp int16 int32 imapPrimArray)
      , TQC.testProperty "itraversePrimArray" (QCCL.imapMProp int16 int32 itraversePrimArray)
      , TQC.testProperty "itraversePrimArrayP" (QCCL.imapMProp int16 int32 itraversePrimArrayP)
      , TQC.testProperty "generatePrimArray" (QCCL.generateProp int16 generatePrimArray)
      , TQC.testProperty "generatePrimArrayA" (QCCL.generateMProp int16 generatePrimArrayA)
      , TQC.testProperty "generatePrimArrayP" (QCCL.generateMProp int16 generatePrimArrayP)
      , TQC.testProperty "replicatePrimArray" (QCCL.replicateProp int16 replicatePrimArray)
      , TQC.testProperty "replicatePrimArrayA" (QCCL.replicateMProp int16 replicatePrimArrayA)
      , TQC.testProperty "replicatePrimArrayP" (QCCL.replicateMProp int16 replicatePrimArrayP)
      , TQC.testProperty "filterPrimArray" (QCCL.filterProp int16 filterPrimArray)
      , TQC.testProperty "filterPrimArrayA" (QCCL.filterMProp int16 filterPrimArrayA)
      , TQC.testProperty "filterPrimArrayP" (QCCL.filterMProp int16 filterPrimArrayP)
      , TQC.testProperty "mapMaybePrimArray" (QCCL.mapMaybeProp int16 int32 mapMaybePrimArray)
      , TQC.testProperty "mapMaybePrimArrayA" (QCCL.mapMaybeMProp int16 int32 mapMaybePrimArrayA)
      , TQC.testProperty "mapMaybePrimArrayP" (QCCL.mapMaybeMProp int16 int32 mapMaybePrimArrayP)
#endif
      ]



     ,testGroup "DefaultSetMethod"
      [ lawsToTest (primLaws (Proxy :: Proxy DefaultSetMethod))
      ]
#if __GLASGOW_HASKELL__ >= 805
    ,testGroup "PrimStorable"
      [ lawsToTest (QCC.storableLaws (Proxy :: Proxy Derived))
      ]
#endif
     ,testGroup "Prim"
      [ renameLawsToTest "Word" (primLaws (Proxy :: Proxy Word))
      , renameLawsToTest "Word8" (primLaws (Proxy :: Proxy Word8))
      , renameLawsToTest "Word16" (primLaws (Proxy :: Proxy Word16))
      , renameLawsToTest "Word32" (primLaws (Proxy :: Proxy Word32))
      , renameLawsToTest "Word64" (primLaws (Proxy :: Proxy Word64))
      , renameLawsToTest "Int" (primLaws (Proxy :: Proxy Int))
      , renameLawsToTest "Int8" (primLaws (Proxy :: Proxy Int8))
      , renameLawsToTest "Int16" (primLaws (Proxy :: Proxy Int16))
      , renameLawsToTest "Int32" (primLaws (Proxy :: Proxy Int32))
      , renameLawsToTest "Int64" (primLaws (Proxy :: Proxy Int64))
      , renameLawsToTest "Const" (primLaws (Proxy :: Proxy (Const Int16 Int16)))
      , renameLawsToTest "Down" (primLaws (Proxy :: Proxy (Down Int16)))
#if MIN_VERSION_base(4,8,0)
      , renameLawsToTest "Identity" (primLaws (Proxy :: Proxy (Identity Int16)))
      , renameLawsToTest "Dual" (primLaws (Proxy :: Proxy (Monoid.Dual Int16)))
      , renameLawsToTest "Sum" (primLaws (Proxy :: Proxy (Monoid.Sum Int16)))
      , renameLawsToTest "Product" (primLaws (Proxy :: Proxy (Monoid.Product Int16)))
#endif
#if MIN_VERSION_base(4,9,0)
      , renameLawsToTest "First" (primLaws (Proxy :: Proxy (Semigroup.First Int16)))
      , renameLawsToTest "Last" (primLaws (Proxy :: Proxy (Semigroup.Last Int16)))
      , renameLawsToTest "Min" (primLaws (Proxy :: Proxy (Semigroup.Min Int16)))
      , renameLawsToTest "Max" (primLaws (Proxy :: Proxy (Semigroup.Max Int16)))
#endif

      ]

    ]

deriving instance Arbitrary a => Arbitrary (Down a)
-- Const, Dual, Sum, Product: all have Arbitrary instances defined
-- in QuickCheck itself
#if MIN_VERSION_base(4,9,0)
deriving instance Arbitrary a => Arbitrary (Semigroup.First a)
deriving instance Arbitrary a => Arbitrary (Semigroup.Last a)
deriving instance Arbitrary a => Arbitrary (Semigroup.Min a)
deriving instance Arbitrary a => Arbitrary (Semigroup.Max a)
#endif

word8 :: Proxy Word8
word8 = Proxy

int16 :: Proxy Int16
int16 = Proxy

int32 :: Proxy Int32
int32 = Proxy


setByteArrayProp :: forall a. (Prim a, Eq a, Arbitrary a, Show a) => Proxy a -> QC.Property
setByteArrayProp _ = QC.property $ \(QC.NonNegative (n :: Int)) (QC.NonNegative (off :: Int)) (QC.NonNegative (len :: Int)) (x :: a) (y :: a) ->
  (off < n && off + len <= n) ==>
  -- We use PrimArray in this test because it makes it easier to
  -- get the element-vs-byte distinction right.
  let actual = runST $ do
        m <- newPrimArray n
        forM_ (enumFromTo 0 (n - 1)) $ \ix -> writePrimArray m ix x
        setPrimArray m off len y
        unsafeFreezePrimArray m
      expected = runST $ do
        m <- newPrimArray n
        forM_ (enumFromTo 0 (n - 1)) $ \ix -> writePrimArray m ix x
        forM_ (enumFromTo off (off + len - 1)) $ \ix -> writePrimArray m ix y
        unsafeFreezePrimArray m
   in expected === actual


-- Tests that using resizeByteArray to shrink a byte array produces
-- the same results as calling Data.List.take on the list that the
-- byte array corresponds to.
byteArrayShrinkProp :: QC.Property
byteArrayShrinkProp = QC.property $ \(QC.NonNegative (n :: Int)) (QC.NonNegative (m :: Int)) ->
  let large = max n m
      small = min n m
      xs = intsLessThan large
      ys = byteArrayFromList xs
      largeBytes = large * sizeOf (undefined :: Int)
      smallBytes = small * sizeOf (undefined :: Int)
      expected = byteArrayFromList (L.take small xs)
      actual = runST $ do
        mzs0 <- newByteArray largeBytes
        copyByteArray mzs0 0 ys 0 largeBytes
        mzs1 <- resizeMutableByteArray mzs0 smallBytes
        unsafeFreezeByteArray mzs1
   in expected === actual

-- Tests that using resizeByteArray with copyByteArray (to fill in the
-- new empty space) to grow a byte array produces the same results as
-- calling Data.List.++ on the lists corresponding to the original
-- byte array and the appended byte array.
byteArrayGrowProp :: QC.Property
byteArrayGrowProp = QC.property $ \(QC.NonNegative (n :: Int)) (QC.NonNegative (m :: Int)) ->
  let large = max n m
      small = min n m
      xs1 = intsLessThan small
      xs2 = intsLessThan (large - small)
      ys1 = byteArrayFromList xs1
      ys2 = byteArrayFromList xs2
      largeBytes = large * sizeOf (undefined :: Int)
      smallBytes = small * sizeOf (undefined :: Int)
      expected = byteArrayFromList (xs1 ++ xs2)
      actual = runST $ do
        mzs0 <- newByteArray smallBytes
        copyByteArray mzs0 0 ys1 0 smallBytes
        mzs1 <- resizeMutableByteArray mzs0 largeBytes
        copyByteArray mzs1 smallBytes ys2 0 ((large - small) * sizeOf (undefined :: Int))
        unsafeFreezeByteArray mzs1
   in expected === actual

-- Tests that writing stable ptrs to a PrimArray, reading them back
-- out, and then dereferencing them gives correct results.
--stablePtrPrimProp :: QC.Property
--stablePtrPrimProp = QC.property $ \(xs :: [Integer]) -> unsafePerformIO $ do
--  ptrs <- mapM newStablePtr xs
--  let ptrs' = primArrayToList (primArrayFromList ptrs)
--  ys <- mapM deRefStablePtr ptrs'
--  mapM_ freeStablePtr ptrs'
--  return (xs === ys)

--stablePtrPrimBlockProp :: QC.Property
--stablePtrPrimBlockProp = QC.property $ \(x :: Word) (QC.NonNegative (len :: Int)) -> unsafePerformIO $ do
--  ptr <- newStablePtr x
--  let ptrs' = replicatePrimArray len ptr
--  let go ix = if ix < len
--        then do
--          n <- deRefStablePtr (indexPrimArray ptrs' ix)
--          ns <- go (ix + 1)
--          return (n : ns)
--        else return []
--  ys <- go 0
--  freeStablePtr ptr
--  return (L.replicate len x === ys)



-- Provide the non-negative integers up to the bound. For example:
--
-- >>> intsLessThan 5
-- [0,1,2,3,4]
intsLessThan :: Int -> [Int]
intsLessThan i = if i < 1
  then []
  else (i - 1) : intsLessThan (i - 1)

byteArrayCompareProp :: QC.Property
byteArrayCompareProp = QC.property $ \(xs :: [Word8]) (ys :: [Word8]) ->
  compareLengthFirst xs ys === compare (byteArrayFromList xs) (byteArrayFromList ys)

byteArrayEqProp :: QC.Property
byteArrayEqProp = QC.property $ \(xs :: [Word8]) (ys :: [Word8]) ->
  (compareLengthFirst xs ys == EQ) === (byteArrayFromList xs == byteArrayFromList ys)

compareLengthFirst :: [Word8] -> [Word8] -> Ordering
compareLengthFirst xs ys = (compare `on` length) xs ys <> compare xs ys

-- on GHC 7.4, Proxy is not polykinded, so we need this instead.
data Proxy1 (f :: * -> *) = Proxy1

lawsToTest :: QCC.Laws -> TestTree
lawsToTest (QCC.Laws name pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

renameLawsToTest :: String -> QCC.Laws -> TestTree
renameLawsToTest name (QCC.Laws _ pairs) = testGroup name (map (uncurry TQC.testProperty) pairs)

testArray :: IO ()
testArray = do
    arr <- newArray 1 'A'
    let unit =
            case writeArray arr 0 'B' of
                IO f ->
                    case f realWorld# of
                        (# _, _ #) -> ()
    c1 <- readArray arr 0
    return $! unit
    c2 <- readArray arr 0
    if c1 == 'A' && c2 == 'B'
        then return ()
        else error $ "Expected AB, got: " ++ show (c1, c2)

testByteArray :: IO ()
testByteArray = do
    let arr1 = mkByteArray ([0xde, 0xad, 0xbe, 0xef] :: [Word8])
        arr2 = mkByteArray ([0xde, 0xad, 0xbe, 0xef] :: [Word8])
        arr3 = mkByteArray ([0xde, 0xad, 0xbe, 0xee] :: [Word8])
        arr4 = mkByteArray ([0xde, 0xad, 0xbe, 0xdd] :: [Word8])
        arr5 = mkByteArray ([0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, 0xbe, 0xdd] :: [Word8])
    when (show arr1 /= "[0xde, 0xad, 0xbe, 0xef]") $
        fail $ "ByteArray Show incorrect: "++show arr1
    when (compareByteArrays arr3 1 arr4 1 3 /= GT) $
        fail $ "arr3[1,3] should be greater than arr4[1,3]"
    when (compareByteArrays arr3 0 arr4 1 3 /= GT) $
        fail $ "arr3[0,3] should be greater than arr4[1,3]"
    when (compareByteArrays arr5 1 arr2 1 3 /= EQ) $
        fail $ "arr3[1,3] should be equal to than arr4[1,3]"
    unless (arr1 > arr3) $
        fail $ "ByteArray Ord incorrect"
    unless (arr1 == arr2) $
        fail $ "ByteArray Eq incorrect"
    unless (mappend arr1 arr4 == arr5) $
        fail $ "ByteArray Monoid mappend incorrect"
    unless (mappend arr1 (mappend arr3 arr4) == mappend (mappend arr1 arr3) arr4) $
        fail $ "ByteArray Monoid mappend not associative"
    unless (mconcat [arr1,arr2,arr3,arr4,arr5] == (arr1 <> arr2 <> arr3 <> arr4 <> arr5)) $
        fail $ "ByteArray Monoid mconcat incorrect"
#if MIN_VERSION_base(4,9,0)
    unless (stimes (3 :: Int) arr4 == (arr4 <> arr4 <> arr4)) $
        fail $ "ByteArray Semigroup stimes incorrect"
#endif

mkByteArray :: Prim a => [a] -> ByteArray
mkByteArray xs = runST $ do
    marr <- newByteArray (length xs * sizeOf (head xs))
    sequence_ $ zipWith (writeByteArray marr) [0..] xs
    unsafeFreezeByteArray marr

instance Arbitrary1 Array where
  liftArbitrary elemGen = fmap fromList (QC.liftArbitrary elemGen)

instance Arbitrary a => Arbitrary (Array a) where
  arbitrary = fmap fromList QC.arbitrary

instance Arbitrary1 SmallArray where
  liftArbitrary elemGen = fmap smallArrayFromList (QC.liftArbitrary elemGen)

instance Arbitrary a => Arbitrary (SmallArray a) where
  arbitrary = fmap smallArrayFromList QC.arbitrary

instance Arbitrary ByteArray where
  arbitrary = do
    xs <- QC.arbitrary :: Gen [Word8]
    return $ runST $ do
      a <- newByteArray (L.length xs)
      iforM_ xs $ \ix x -> do
        writeByteArray a ix x
      unsafeFreezeByteArray a

instance (Arbitrary a, Prim a) => Arbitrary (PrimArray a) where
  arbitrary = do
    xs <- QC.arbitrary :: Gen [a]
    return $ runST $ do
      a <- newPrimArray (L.length xs)
      iforM_ xs $ \ix x -> do
        writePrimArray a ix x
      unsafeFreezePrimArray a



instance (Prim a, CoArbitrary a) => CoArbitrary (PrimArray a) where
  coarbitrary x = QC.coarbitrary (primArrayToList x)

instance (Prim a, Function a) => Function (PrimArray a) where
  function = QC.functionMap primArrayToList primArrayFromList

iforM_ :: Monad m => [a] -> (Int -> a -> m b) -> m ()
iforM_ xs0 f = go 0 xs0 where
  go !_ [] = return ()
  go !ix (x : xs) = f ix x >> go (ix + 1) xs

newtype DefaultSetMethod = DefaultSetMethod Int16
  deriving (Eq,Show,Arbitrary)

instance Prim DefaultSetMethod where
  sizeOf# _ = sizeOf# (undefined :: Int16)
  alignment# _ = alignment# (undefined :: Int16)
  indexByteArray# arr ix = DefaultSetMethod (indexByteArray# arr ix)
  readByteArray# arr ix s0 = case readByteArray# arr ix s0 of
    (# s1, n #) -> (# s1, DefaultSetMethod n #)
  writeByteArray# arr ix (DefaultSetMethod n) s0 = writeByteArray# arr ix n s0
  setByteArray# = defaultSetByteArray#
  indexOffAddr# addr off = DefaultSetMethod (indexOffAddr# addr off)
  readOffAddr# addr off s0 = case readOffAddr# addr off s0 of
    (# s1, n #) -> (# s1, DefaultSetMethod n #)
  writeOffAddr# addr off (DefaultSetMethod n) s0 = writeOffAddr# addr off n s0
  setOffAddr# = defaultSetOffAddr#

#if __GLASGOW_HASKELL__ >= 805
newtype Derived = Derived Int16
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary, Prim)
  deriving Storable via (PrimStorable Derived)
#endif
