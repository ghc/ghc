{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-} 
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module GHC.Event.IntTable
    (
      IntTable
    , new
    , lookup
    , reset
    , delete
    , updateWithM
    , insertCons
    ) where

import Data.Bits ((.&.), shiftL, shiftR)
import Data.Maybe (Maybe(..))
import GHC.Base (Monad(..), (=<<), ($), ($!), undefined, const, liftM, otherwise, when, pure, error)
import GHC.Classes (Eq(..), Ord(..))
import GHC.Num (Num(..))
import GHC.Prim (RealWorld)
import GHC.Types (IO(..), Int(..))
import GHC.Primitive.Array (Array,MutableArray)
import GHC.Primitive.SmallArray (SmallArray,SmallMutableArray)
import GHC.Primitive.UnliftedArray (MutableUnliftedArray)
import qualified Data.Foldable as F
import qualified GHC.Primitive.Array as PM
import qualified GHC.Primitive.SmallArray as PM
import qualified GHC.Primitive.UnliftedArray as PM

-- A very simple chained integer-keyed mutable hash table. We use
-- power-of-two sizing, grow at a load factor of 0.75, and never
-- shrink. The "hash function" is the identity function.
--
-- Each value is an array of values, not a single value. This
-- specilization is performed because the event manager is the
-- only user of this module. It makes it possible to write more
-- specialized functions in this module, and it allows the Array
-- to unpack into the Bucket data constructor.
--
-- Invariant: the array value shall never be empty. If this would
-- happened, the key is deleted instead.

type Arr = SmallMutableArray RealWorld
type UArr = MutableUnliftedArray RealWorld

data IntTable a = IntTable {
      tabArr  :: {-# UNPACK #-} !(UArr (Arr (Bucket a)))
    , tabSize :: {-# UNPACK #-} !Int
    }

data Bucket a = Bucket {
      bucketKey   :: {-# UNPACK #-} !Int
    , bucketValue :: {-# UNPACK #-} !(SmallArray a)
    }

-- Returns the value array associated with the key. Returns the
-- empty array if the key is not present.
lookup :: Int -> IntTable a -> IO (SmallArray a)
lookup k (IntTable {tabArr}) = do
  let go !bktIx !bkts = if bktIx >= 0
        then do
          Bucket{bucketKey,bucketValue} <- PM.readSmallArray bkts bktIx
          if k == bucketKey
            then pure bucketValue
            else go (bktIx - 1) bkts
        else pure PM.emptySmallArray
  bkts <- PM.readUnliftedArray tabArr (hashIndex k (PM.sizeofMutableUnliftedArray tabArr))
  go (PM.sizeofSmallMutableArray bkts - 1) bkts

-- Invariant: The capacity must be a power of two.
new :: forall a. Int -> IO (IntTable a)
new capacity = do
  (buckets :: Arr (Bucket a)) <- PM.newSmallArray 0 uninitialized
  -- It is safe to alias the same buckets array multiple times
  -- because it is empty. Any inserts to will result in a new
  -- buckets array being allocated for the corresponding hash
  -- index.
  arr <- PM.newUnliftedArray capacity buckets
  return IntTable
    { tabArr = arr
    , tabSize = 0
    }

{-# NOINLINE uninitialized #-}
uninitialized :: a
uninitialized = error "GHC.Event.IntTable: uninitialized element"

-- The implementation has two steps. First, we build a
-- SmallArray [Bucket]. Then, we traverse the array, converting
-- all the bucket lists to arrays.
grow :: forall a. IntTable a -> IO (IntTable a)
grow (IntTable {tabArr,tabSize}) = do
  let newArrSize = twice (PM.sizeofMutableUnliftedArray tabArr)
  (newArr :: Arr [Bucket a]) <- PM.newSmallArray newArrSize []
  -- In this loop, n is the number of remaining values to
  -- copy and i is the current slot index.
  let copySlot :: Int -> Int -> IO ()
      copySlot !n !i
        | n == 0 = return ()
        | otherwise = do
          let copyBucket !m !srcBkts !srcBktIx = if srcBktIx >= 0
                then do
                  b@Bucket{bucketKey} <- PM.readSmallArray srcBkts srcBktIx
                  let idx = hashIndex bucketKey newArrSize
                  (dstBkts :: [Bucket a]) <- PM.readSmallArray newArr idx
                  let !dstBkts' = b : dstBkts
                  PM.writeSmallArray newArr idx dstBkts'
                  copyBucket (m-1) srcBkts (srcBktIx - 1)
                else copySlot m (i+1)
          (buckets :: Arr (Bucket a)) <- PM.readUnliftedArray tabArr i
          copyBucket n buckets (PM.sizeofSmallMutableArray buckets - 1)
  copySlot tabSize 0
  -- This array-to-list-to-unlifted-array business is a little goofy.
  -- There is a more efficient way to do this, but growing happens
  -- very infrequently, so it doesn't matter.
  liftedFinalArr <- PM.unsafeFreezeSmallArray =<< PM.traverseSmallMutableArray PM.smallMutableArrayFromList newArr
  finalNewArr <- PM.mutableUnliftedArrayFromList (F.toList liftedFinalArr)
  pure IntTable
    { tabArr = finalNewArr
    , tabSize = tabSize
    }

scanBuckets :: Int -> Arr (Bucket a) -> IO (Maybe (Int,Bucket a))
scanBuckets !k !bkts = go (PM.sizeofSmallMutableArray bkts - 1) where
  go !ix = if ix >= 0
    then do
      b@Bucket{bucketKey} <- PM.readSmallArray bkts ix
      if bucketKey == k
        then pure (Just (ix,b))
        else go (ix - 1)
    else pure Nothing

-- | @insertCons k v table@ inserts @k@ into @table@ with:
-- 
-- * @v@ in a singleton array if the key was not previously present.
-- * @v@ consed onto the previous array if the key was already present.
-- 
-- This returns the previous value. If the value was not present, it
-- is the empty array.
insertCons :: Int -> a -> IntTable a -> IO (IntTable a,SmallArray a)
insertCons !k !v (IntTable {tabSize,tabArr}) = do
  let idx = hashIndex k (PM.sizeofMutableUnliftedArray tabArr)
  bkts <- PM.readUnliftedArray tabArr idx
  scanBuckets k bkts >>= \case
    Nothing -> do
      newTable <- insertHelper k idx (PM.singletonSmallArray v) bkts (IntTable {tabSize,tabArr})
      case PM.emptySmallArray of
        PM.SmallArray u -> pure (newTable, PM.SmallArray u)
    Just (bktIx,Bucket{bucketValue}) -> do
      let !newVal = PM.consSmallArray v bucketValue
          !newBkt = Bucket
            { bucketKey = k
            , bucketValue = newVal
            }
      PM.writeSmallArray bkts bktIx newBkt
      pure (IntTable{tabSize,tabArr},bucketValue)

-- | Remove the given key from the table and return its associated value.
-- Returns the empty array for the associated value when the key was not
-- present.
delete :: Int -> IntTable a -> IO (IntTable a,SmallArray a)
delete !k (IntTable {tabSize,tabArr}) = do
  let idx = hashIndex k (PM.sizeofMutableUnliftedArray tabArr)
  bkts <- PM.readUnliftedArray tabArr idx
  scanBuckets k bkts >>= \case
    Nothing -> pure (IntTable{tabSize,tabArr},PM.emptySmallArray)
    Just (bktIx,Bucket{bucketValue}) -> do
      newBkts <- PM.deleteIndexSmallMutableArray bkts bktIx
      PM.writeUnliftedArray tabArr idx newBkts
      pure (IntTable{tabSize = tabSize - 1,tabArr},bucketValue)

delete_ :: Int -> IntTable a -> IO (IntTable a)
delete_ !k (IntTable {tabSize,tabArr}) = do
  let idx = hashIndex k (PM.sizeofMutableUnliftedArray tabArr)
  bkts <- PM.readUnliftedArray tabArr idx
  scanBuckets k bkts >>= \case
    Nothing -> pure (IntTable{tabSize,tabArr})
    Just (bktIx,_) -> do
      newBkts <- PM.deleteIndexSmallMutableArray bkts bktIx
      PM.writeUnliftedArray tabArr idx newBkts
      pure (IntTable{tabSize = tabSize - 1,tabArr})


-- This is only used internally in contexts where we already know
-- that lookup failed. The bkts arguments is the buckets at the
-- key, already looked up before calling this function.
insertHelper :: Int -> Int -> SmallArray a -> Arr (Bucket a) -> IntTable a -> IO (IntTable a)
{-# INLINE insertHelper #-}
insertHelper !k !idx !v !bkts (IntTable {tabSize,tabArr}) = do
  let !newBkt = Bucket
        { bucketKey = k
        , bucketValue = v
        }
  newBkts <- PM.consSmallMutableArray newBkt bkts
  PM.writeUnliftedArray tabArr idx newBkts
  let newTable = IntTable{tabSize = tabSize + 1,tabArr}
  -- If we are at least 75% full, grow.
  if tabSize + 1 >= PM.sizeofMutableUnliftedArray tabArr - (PM.sizeofMutableUnliftedArray tabArr `shiftR` 2)
    then grow newTable
    else pure newTable

replace :: Int -> SmallArray a -> IntTable a -> IO (IntTable a)
replace !k !v (IntTable {tabSize,tabArr}) = do
  let idx = hashIndex k (PM.sizeofMutableUnliftedArray tabArr)
  bkts <- PM.readUnliftedArray tabArr idx
  scanBuckets k bkts >>= \case
    Nothing -> do
      newTable <- insertHelper k idx v bkts (IntTable {tabSize,tabArr})
      pure newTable
    Just (bktIx,_) -> do
      let !newBkt = Bucket { bucketKey = k , bucketValue = v }
      PM.writeSmallArray bkts bktIx newBkt
      pure (IntTable{tabSize,tabArr})

-- Updates the value array with the callback. This will not use
-- the callback if the key is not found. That is, it will never
-- cause the table to grow. The old and new arrays are returned.
--
-- We inline this function because it is only ever called in
-- two places in the event manager.
updateWithM :: (SmallArray a -> IO (SmallArray a)) -> Int -> IntTable a -> IO (IntTable a, SmallArray a, SmallArray a)
{-# INLINE updateWithM #-}
updateWithM f !k (IntTable {tabSize,tabArr}) = do
  let idx = hashIndex k (PM.sizeofMutableUnliftedArray tabArr)
  bkts <- PM.readUnliftedArray tabArr idx
  scanBuckets k bkts >>= \case
    Nothing ->
      pure (IntTable{tabSize,tabArr},PM.emptySmallArray,PM.emptySmallArray)
    Just (bktIx,Bucket{bucketValue}) -> do
      newVal <- f bucketValue
      if PM.sizeofSmallArray newVal == 0
        then do
          newBkts <- PM.deleteIndexSmallMutableArray bkts bktIx
          PM.writeUnliftedArray tabArr idx newBkts
          pure (IntTable{tabSize = tabSize - 1,tabArr},bucketValue,newVal)
        else do
          let !newBkt = Bucket
                { bucketKey = k
                , bucketValue = newVal
                }
          PM.writeSmallArray bkts bktIx newBkt
          pure (IntTable{tabSize,tabArr},bucketValue,newVal)

-- | Used to undo the effect of a prior insertWith.
reset :: Int -> SmallArray a -> IntTable a -> IO (IntTable a)
reset k v tbl = if PM.sizeofSmallArray v == 0
  then delete_ k tbl
  else replace k v tbl

twice :: Int -> Int
twice x = x `shiftL` 1


hashIndex ::  
     Int -- key
  -> Int -- size of array, must be base 2
  -> Int
hashIndex k sz = k .&. (sz - 1)
