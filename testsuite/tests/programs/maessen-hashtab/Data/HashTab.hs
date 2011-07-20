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

module Data.HashTab (
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

-- This module is imported by Data.Typeable, which is pretty low down in the
-- module hierarchy, so don't import "high-level" modules

-- Right now we import high-level modules with gay abandon.
import Prelude	hiding	( lookup )
import Data.Tuple	( fst )
import Data.Bits
import Data.Maybe
import Data.List	( maximumBy, partition, concat, foldl )
import Data.Int		( Int32 )

import Data.Array.Base
import Data.Array       hiding (bounds)
import Data.Array.IO

import Data.Char	( ord )
import Data.IORef	( IORef, newIORef, readIORef, writeIORef )
import Control.Monad	( mapM, sequence_ )


-----------------------------------------------------------------------

readHTArray  :: HTArray a -> Int32 -> IO a
readMutArray  :: MutArray a -> Int32 -> IO a
writeMutArray :: MutArray a -> Int32 -> a -> IO ()
freezeArray  :: MutArray a -> IO (HTArray a)
thawArray    :: HTArray a -> IO (MutArray a)
newMutArray   :: (Int32, Int32) -> a -> IO (MutArray a)
#if defined(DEBUG) || defined(__NHC__)
type MutArray a = IOArray Int32 a
type HTArray a = MutArray a
newMutArray = newArray
readHTArray  = readArray
readMutArray = readArray
writeMutArray = writeArray
freezeArray = return
thawArray = return
#else
type MutArray a = IOArray Int32 a
type HTArray a = Array Int32 a
newMutArray = newArray
readHTArray arr i = return $! (unsafeAt arr (fromIntegral i))
readMutArray arr i = unsafeRead arr (fromIntegral i)
writeMutArray arr i x = unsafeWrite arr (fromIntegral i) x
freezeArray = unsafeFreeze
thawArray = unsafeThaw
#endif

newtype HashTable key val = HashTable (IORef (HT key val))
-- TODO: the IORef should really be an MVar.

data HT key val
  = HT {
	kcount  :: !Int32,              -- Total number of keys.
	buckets :: !(HTArray [(key,val)]),
        bmask   :: !Int32,
	hash_fn :: key -> Int32,
	cmp     :: key -> key -> Bool
   }

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

tABLE_MAX  = 1024 * 1024 :: Int32  -- Maximum size of hash table
#if tABLE_MIN
#else
tABLE_MIN  = 16 :: Int32

hLOAD = 4 :: Int32 -- Maximum average load of a single hash bucket

hYSTERESIS = 0 :: Int32 -- entries to ignore in load computation
#endif

{- Hysteresis favors long association-list-like behavior for small tables. -}

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

new cmpr hash = do
  -- make a new hash table with a single, empty, segment
  let mask = tABLE_MIN-1
  bkts'  <- newMutArray (0,mask) []
  bkts   <- freezeArray bkts'

  let
    kcnt = 0
    ht = HT {  buckets=bkts, kcount=kcnt, bmask=mask,
               hash_fn=hash, cmp=cmpr }

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
  table@HT{ kcount=k, buckets=bkts, bmask=b } <- readIORef ref
  let table1 = table{ kcount = k+1 }
      indx = bucketIndex table key
  bucket <- readHTArray bkts indx
  bkts' <- thawArray bkts
  writeMutArray bkts' indx ((key,val):bucket)
  freezeArray bkts'
  table2 <-
	if tooBig k b
	   then expandHashTable table1
	   else return table1
  writeIORef ref table2

tooBig :: Int32 -> Int32 -> Bool
tooBig k b = k-hYSTERESIS > hLOAD * b

bucketIndex :: HT key val -> key -> Int32
bucketIndex HT{ hash_fn=hash, bmask=mask } key =
  let h = hash key
  in  (h .&. mask)

expandHashTable :: HT key val -> IO (HT key val)
expandHashTable
      table@HT{ buckets=bkts, bmask=mask } = do
   let
      oldsize = mask + 1
      newmask = mask + mask + 1
      newsize = newmask + 1
   --
   if newsize > tABLE_MAX
      then return table
      else do
   --
   newbkts' <- newMutArray (0,newmask) []

   let
    table'=table{ bmask=newmask }
    splitBucket oldindex = do
      bucket <- readHTArray bkts oldindex
      let (oldb,newb) = partition ((oldindex==).bucketIndex table' . fst) bucket
      writeMutArray newbkts' oldindex oldb
      writeMutArray newbkts' (oldindex + oldsize) newb
   mapM_ splitBucket [0..mask]

   newbkts <- freezeArray newbkts'

   return ( table'{ buckets=newbkts } )

-- -----------------------------------------------------------------------------
-- Deleting a mapping from the hash table

-- Remove a key from a bucket
deleteBucket :: (key -> Bool) -> [(key,val)] -> (Int32, [(key, val)])
deleteBucket _   [] = (0,[])
deleteBucket del (pair@(k,_):bucket) =
  case deleteBucket del bucket of
    (dels, bucket') | del k     -> dels' `seq` (dels', bucket')
                    | otherwise -> (dels, pair:bucket')
      where dels' = dels + 1

-- | Remove an entry from the hash table.
delete :: HashTable key val -> key -> IO ()

delete (HashTable ref) key = do
  table@HT{ buckets=bkts, kcount=kcnt, cmp=cmpr } <- readIORef ref
  let indx = bucketIndex table key
  bkts' <- thawArray bkts
  bucket <- readMutArray bkts' indx
  let (removed,bucket') = deleteBucket (cmpr key) bucket
  writeMutArray bkts' indx bucket'
  freezeArray bkts'
  writeIORef ref ( table{kcount = kcnt - removed} )

-- -----------------------------------------------------------------------------
-- Updating a mapping in the hash table

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
  table@HT{ kcount=k, buckets=bkts, cmp=cmpr, bmask=b } <- readIORef ref
  let indx = bucketIndex table key
  bkts' <- thawArray bkts
  bucket <- readMutArray bkts' indx
  let (deleted,bucket') = deleteBucket (cmpr key) bucket
      k' = k + 1 - deleted
      table1 = table{ kcount=k' }

  writeMutArray bkts' indx ((key,val):bucket')
  freezeArray bkts'
  table2 <-
	if tooBig k' b          -- off by one from insert's resize heuristic.
	   then expandHashTable table1
	   else return table1
  writeIORef ref table2
  return (deleted>0)

-- -----------------------------------------------------------------------------
-- Looking up an entry in the hash table

-- | Looks up the value of a key in the hash table.
lookup :: HashTable key val -> key -> IO (Maybe val)

lookup (HashTable ref) key = do
  table@HT{ buckets=bkts, cmp=cmpr } <- readIORef ref
  let indx = bucketIndex table key
  bucket <- readHTArray bkts indx
  case [ val | (key',val) <- bucket, cmpr key key' ] of
	[] -> return Nothing
	(v:_) -> return (Just v)

-- -----------------------------------------------------------------------------
-- Converting to/from lists

-- | Convert a list of key\/value pairs into a hash table.  Equality on keys
-- is taken from the Eq instance for the key type.
--
fromList :: (Eq key) => (key -> Int32) -> [(key,val)] -> IO (HashTable key val)
fromList hash list = do
  table <- new (==) hash
  sequence_ [ insert table k v | (k,v) <- list ]
  return table

-- | Converts a hash table to a list of key\/value pairs.
--
toList :: (Ord key, Ord val) => HashTable key val -> IO [(key,val)]
toList (HashTable ref) = do
  HT{ buckets=bkts, bmask=b } <- readIORef ref
  fmap concat (mapM (readHTArray bkts) [0..b])

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
  HT{ buckets=bkts, bmask=b } <- readIORef ref
  let lengthCmp (_:x)(_:y) = lengthCmp x y
      lengthCmp []   []    = EQ
      lengthCmp []   _     = LT
      lengthCmp _    []    = GT
  fmap (maximumBy lengthCmp) (mapM (readHTArray bkts) [0..b])
