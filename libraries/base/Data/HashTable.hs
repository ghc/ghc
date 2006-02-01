{-# OPTIONS_GHC -fno-implicit-prelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.HashTable
-- Copyright   :  (c) The University of Glasgow 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An implementation of extensible hash tables, as described in
-- Per-Ake Larson, /Dynamic Hash Tables/, CACM 31(4), April 1988,
-- pp. 446--457.  The implementation is also derived from the one
-- in GHC's runtime system (@ghc\/rts\/Hash.{c,h}@).
--
-----------------------------------------------------------------------------

module Data.HashTable (
	-- * Basic hash table operations
	HashTable, new, insert, delete, lookup, update,
	-- * Converting to and from lists
	fromList, toList,
	-- * Hash functions
	-- $hash_functions
	hashInt, hashString,
	prime,
	-- * Diagnostics
	longestChain
 ) where

-- This module is imported by Data.Dynamic, which is pretty low down in the
-- module hierarchy, so don't import "high-level" modules

#ifdef __GLASGOW_HASKELL__
import GHC.Base
#else
import Prelude	hiding	( lookup )
#endif
import Data.Tuple	( fst )
import Data.Bits
import Data.Maybe
import Data.List	( maximumBy, filter, length, concat, foldl, reverse )
import Data.Int		( Int32 )

#if defined(__GLASGOW_HASKELL__)
import GHC.Num
import GHC.Real		( Integral(..), fromIntegral )

import GHC.IOBase	( IO, IOArray, newIOArray, readIOArray, writeIOArray,
			  unsafeReadIOArray, unsafeWriteIOArray,
			  IORef, newIORef, readIORef, writeIORef )
import GHC.Err		( undefined )
#else
import Data.Char	( ord )
import Data.IORef	( IORef, newIORef, readIORef, writeIORef )
#  if defined(__HUGS__)
import Hugs.IOArray	( IOArray, newIOArray, readIOArray, writeIOArray,
			  unsafeReadIOArray, unsafeWriteIOArray )
#  elif defined(__NHC__)
import NHC.IOExtras	( IOArray, newIOArray, readIOArray, writeIOArray)
#  endif
#endif
import Control.Monad	( when, mapM, sequence_ )


-----------------------------------------------------------------------
myReadArray  :: IOArray Int32 a -> Int32 -> IO a
myWriteArray :: IOArray Int32 a -> Int32 -> a -> IO ()
#if defined(DEBUG) || defined(__NHC__)
myReadArray  = readIOArray
myWriteArray = writeIOArray
#else
myReadArray arr i = unsafeReadIOArray arr (fromIntegral i)
myWriteArray arr i x = unsafeWriteIOArray arr (fromIntegral i) x
#endif

-- | A hash table mapping keys of type @key@ to values of type @val@.
--
-- The implementation will grow the hash table as necessary, trying to
-- maintain a reasonable average load per bucket in the table.
--
newtype HashTable key val = HashTable (IORef (HT key val))
-- TODO: the IORef should really be an MVar.

data HT key val
  = HT {
	split  :: !Int32, -- Next bucket to split when expanding
	max_bucket :: !Int32, -- Max bucket of smaller table
	mask1  :: !Int32, -- Mask for doing the mod of h_1 (smaller table)
	mask2  :: !Int32, -- Mask for doing the mod of h_2 (larger table)
	kcount :: !Int32, -- Number of keys
	bcount :: !Int32, -- Number of buckets
	dir    :: !(IOArray Int32 (IOArray Int32 [(key,val)])),
	hash_fn :: key -> Int32,
	cmp    :: key -> key -> Bool
   }

{-
ALTERNATIVE IMPLEMENTATION:

This works out slightly slower, because there's a tradeoff between
allocating a complete new HT structure each time a modification is
made (in the version above), and allocating new Int32s each time one
of them is modified, as below.  Using FastMutInt instead of IORef
Int32 helps, but yields an implementation which has about the same
performance as the version above (and is more complex).

data HashTable key val
  = HashTable {
	split  :: !(IORef Int32), -- Next bucket to split when expanding
	max_bucket :: !(IORef Int32), -- Max bucket of smaller table
	mask1  :: !(IORef Int32), -- Mask for doing the mod of h_1 (smaller table)
	mask2  :: !(IORef Int32), -- Mask for doing the mod of h_2 (larger table)
	kcount :: !(IORef Int32), -- Number of keys
	bcount :: !(IORef Int32), -- Number of buckets
	dir    :: !(IOArray Int32 (IOArray Int32 [(key,val)])),
	hash_fn :: key -> Int32,
	cmp    :: key -> key -> Bool
   }
-}


-- -----------------------------------------------------------------------------
-- Sample hash functions

-- $hash_functions
--
-- This implementation of hash tables uses the low-order /n/ bits of the hash
-- value for a key, where /n/ varies as the hash table grows.  A good hash
-- function therefore will give an even distribution regardless of /n/.
--
-- If your keyspace is integrals such that the low-order bits between
-- keys are highly variable, then you could get away with using 'id'
-- as the hash function.
--
-- We provide some sample hash functions for 'Int' and 'String' below.

-- | A sample hash function for 'Int', implemented as simply @(x `mod` P)@
-- where P is a suitable prime (currently 1500007).  Should give
-- reasonable results for most distributions of 'Int' values, except
-- when the keys are all multiples of the prime!
--
hashInt :: Int -> Int32
hashInt = (`rem` prime) . fromIntegral

-- | A sample hash function for 'String's.  The implementation is:
--
-- >    hashString = fromIntegral . foldr f 0
-- >      where f c m = ord c + (m * 128) `rem` 1500007
--
-- which seems to give reasonable results.
--
hashString :: String -> Int32
hashString = fromIntegral . foldl f 0
  where f m c = ord c + (m * 128) `rem` fromIntegral prime

-- | A prime larger than the maximum hash table size
prime :: Int32
prime = 1500007

-- -----------------------------------------------------------------------------
-- Parameters

sEGMENT_SIZE  = 1024  :: Int32  -- Size of a single hash table segment
sEGMENT_SHIFT = 10    :: Int  -- derived
sEGMENT_MASK  = 0x3ff :: Int32  -- derived

dIR_SIZE = 1024  :: Int32  -- Size of the segment directory
	-- Maximum hash table size is sEGMENT_SIZE * dIR_SIZE

hLOAD = 4 :: Int32 -- Maximum average load of a single hash bucket

-- -----------------------------------------------------------------------------
-- Creating a new hash table

-- | Creates a new hash table.  The following property should hold for the @eq@
-- and @hash@ functions passed to 'new':
--
-- >   eq A B  =>  hash A == hash B
--
new
  :: (key -> key -> Bool)    -- ^ @eq@: An equality comparison on keys
  -> (key -> Int32)	     -- ^ @hash@: A hash function on keys
  -> IO (HashTable key val)  -- ^ Returns: an empty hash table

new cmp hash_fn = do
  -- make a new hash table with a single, empty, segment
  dir     <- newIOArray (0,dIR_SIZE-1) undefined
  segment <- newIOArray (0,sEGMENT_SIZE-1) []
  myWriteArray dir 0 segment

  let
    split  = 0
    max    = sEGMENT_SIZE
    mask1  = (sEGMENT_SIZE - 1)
    mask2  = (2 * sEGMENT_SIZE - 1)
    kcount = 0
    bcount = sEGMENT_SIZE

    ht = HT {  dir=dir, split=split, max_bucket=max, mask1=mask1, mask2=mask2,
	       kcount=kcount, bcount=bcount, hash_fn=hash_fn, cmp=cmp
	  }
  
  table <- newIORef ht
  return (HashTable table)

-- -----------------------------------------------------------------------------
-- Inserting a key\/value pair into the hash table

-- | Inserts an key\/value mapping into the hash table.  
--
-- Note that 'insert' doesn't remove the old entry from the table -
-- the behaviour is like an association list, where 'lookup' returns
-- the most-recently-inserted mapping for a key in the table.  The
-- reason for this is to keep 'insert' as efficient as possible.  If
-- you need to update a mapping, then we provide 'update'.
--
insert :: HashTable key val -> key -> val -> IO ()

insert (HashTable ref) key val = do
  table@HT{ kcount=k, bcount=b, dir=dir } <- readIORef ref
  let table1 = table{ kcount = k+1 }
  table2 <-
	if (k > hLOAD * b)
	   then expandHashTable table1
	   else return table1
  writeIORef ref table2
  (segment_index,segment_offset) <- tableLocation table2 key
  segment <- myReadArray dir segment_index
  bucket <- myReadArray segment segment_offset
  myWriteArray segment segment_offset ((key,val):bucket)
  return ()

bucketIndex :: HT key val -> key -> IO Int32
bucketIndex HT{ hash_fn=hash_fn,
		split=split,
		mask1=mask1,
		mask2=mask2 } key = do
  let
    h = fromIntegral (hash_fn key)
    small_bucket = h .&. mask1
    large_bucket = h .&. mask2
  --
  if small_bucket < split
	then return large_bucket
  	else return small_bucket

tableLocation :: HT key val -> key -> IO (Int32,Int32)
tableLocation table key = do
  bucket_index <- bucketIndex table key
  let
    segment_index  = bucket_index `shiftR` sEGMENT_SHIFT
    segment_offset = bucket_index .&. sEGMENT_MASK
  --
  return (segment_index,segment_offset)

expandHashTable :: HT key val -> IO (HT key val)
expandHashTable
      table@HT{ dir=dir,
		split=split,
		max_bucket=max,
		bcount=bcount,
		mask2=mask2 } = do
  let
      oldsegment = split `shiftR` sEGMENT_SHIFT
      oldindex   = split .&. sEGMENT_MASK

      newbucket  = max + split
      newsegment = newbucket `shiftR` sEGMENT_SHIFT
      newindex   = newbucket .&. sEGMENT_MASK
  --
  if newsegment >= dIR_SIZE	-- make sure we don't overflow the table.
	then return table
	else do
  --
   when (newindex == 0) $
	do segment <- newIOArray (0,sEGMENT_SIZE-1) []
	   writeIOArray dir newsegment segment
	   -- doesn't happen very often, so we might as well use a safe
	   -- array index here.
  --
   let table' =
  	if (split+1) < max
     	    then table{ split = split+1,
			bcount = bcount+1 }
     		-- we've expanded all the buckets in this table, so start from
		-- the beginning again.
     	    else table{ split = 0,
			bcount = bcount+1,
			max_bucket = max * 2,
			mask1 = mask2,
			mask2 = mask2 `shiftL` 1 .|. 1 }
   let
    split_bucket old new [] = do
	segment <- myReadArray dir oldsegment
	myWriteArray segment oldindex (reverse old)
	segment <- myReadArray dir newsegment
	myWriteArray segment newindex (reverse new)
    split_bucket old new ((k,v):xs) = do
	h <- bucketIndex table' k
	if h == newbucket
		then split_bucket old ((k,v):new) xs
		else split_bucket ((k,v):old) new xs
  --
   segment <- myReadArray dir oldsegment
   bucket <- myReadArray segment oldindex
   split_bucket [] [] bucket
   return table'

-- -----------------------------------------------------------------------------
-- Deleting a mapping from the hash table

-- | Remove an entry from the hash table.
delete :: HashTable key val -> key -> IO ()

delete (HashTable ref) key = do
  table@HT{ dir=dir, cmp=cmp } <- readIORef ref
  (segment_index,segment_offset) <- tableLocation table key
  segment <- myReadArray dir segment_index
  bucket <- myReadArray segment segment_offset
  myWriteArray segment segment_offset (filter (not.(key `cmp`).fst) bucket)
  return ()

-- -----------------------------------------------------------------------------
-- Deleting a mapping from the hash table

-- | Updates an entry in the hash table, returning 'True' if there was
-- already an entry for this key, or 'False' otherwise.  After 'update'
-- there will always be exactly one entry for the given key in the table.
--
-- 'insert' is more efficient than 'update' if you don't care about
-- multiple entries, or you know for sure that multiple entries can't
-- occur.  However, 'update' is more efficient than 'delete' followed
-- by 'insert'.
update :: HashTable key val -> key -> val -> IO Bool

update (HashTable ref) key val = do
  table@HT{ kcount=k, bcount=b, dir=dir, cmp=cmp } <- readIORef ref
  let table1 = table{ kcount = k+1 }
  -- optimistically expand the table
  table2 <-
	if (k > hLOAD * b)
	   then expandHashTable table1
	   else return table1
  writeIORef ref table2
  (segment_index,segment_offset) <- tableLocation table2 key
  segment <- myReadArray dir segment_index
  bucket <- myReadArray segment segment_offset
  let 
    (deleted,bucket') = foldr filt (0::Int32,[]) bucket
    filt pair@(k,v) (deleted,bucket)
	| key `cmp` k = (deleted+1, bucket)
	| otherwise   = (deleted,   pair:bucket)
  -- in  
  myWriteArray segment segment_offset ((key,val):bucket')
  -- update the table load, taking into account the number of
  -- items we just deleted.
  writeIORef ref table2{ kcount = kcount table2 - deleted }
  return (deleted /= 0)

-- -----------------------------------------------------------------------------
-- Looking up an entry in the hash table

-- | Looks up the value of a key in the hash table.
lookup :: HashTable key val -> key -> IO (Maybe val)

lookup (HashTable ref) key = do
  table@HT{ dir=dir, cmp=cmp } <- readIORef ref
  (segment_index,segment_offset) <- tableLocation table key
  segment <- myReadArray dir segment_index
  bucket <- myReadArray segment segment_offset
  case [ val | (key',val) <- bucket, cmp key key' ] of
	[] -> return Nothing
	(v:_) -> return (Just v)

-- -----------------------------------------------------------------------------
-- Converting to/from lists

-- | Convert a list of key\/value pairs into a hash table.  Equality on keys
-- is taken from the Eq instance for the key type.
--
fromList :: Eq key => (key -> Int32) -> [(key,val)] -> IO (HashTable key val)
fromList hash_fn list = do
  table <- new (==) hash_fn
  sequence_ [ insert table k v | (k,v) <- list ]
  return table

-- | Converts a hash table to a list of key\/value pairs.
--
toList :: HashTable key val -> IO [(key,val)]
toList (HashTable ref) = do
  HT{ dir=dir, max_bucket=max, split=split } <- readIORef ref
  --
  let
    max_segment = (max + split - 1) `quot` sEGMENT_SIZE
  --
  segments <- mapM (segmentContents dir) [0 .. max_segment]
  return (concat segments)
 where
   segmentContents dir seg_index = do
     segment <- myReadArray dir seg_index
     bs <- mapM (myReadArray segment) [0 .. sEGMENT_SIZE-1]
     return (concat bs)

-- -----------------------------------------------------------------------------
-- Diagnostics

-- | This function is useful for determining whether your hash function
-- is working well for your data set.  It returns the longest chain
-- of key\/value pairs in the hash table for which all the keys hash to
-- the same bucket.  If this chain is particularly long (say, longer
-- than 10 elements), then it might be a good idea to try a different
-- hash function.
--
longestChain :: HashTable key val -> IO [(key,val)]
longestChain (HashTable ref) = do
  HT{ dir=dir, max_bucket=max, split=split } <- readIORef ref
  --
  let
    max_segment = (max + split - 1) `quot` sEGMENT_SIZE
  --
  --trace ("maxChainLength: max = " ++ show max ++ ", split = " ++ show split ++ ", max_segment = " ++ show max_segment) $ do
  segments <- mapM (segmentMaxChainLength dir) [0 .. max_segment]
  return (maximumBy lengthCmp segments)
 where
   segmentMaxChainLength dir seg_index = do
     segment <- myReadArray dir seg_index
     bs <- mapM (myReadArray segment) [0 .. sEGMENT_SIZE-1]
     return (maximumBy lengthCmp bs)

   lengthCmp x y = length x `compare` length y
