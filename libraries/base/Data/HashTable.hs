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
import Data.List	( maximumBy, length, concat, foldl', partition )
import Data.Int		( Int32 )

#if defined(__GLASGOW_HASKELL__)
import GHC.Num
import GHC.Real		( fromIntegral )
import GHC.Show		( Show(..) )
import GHC.Int		( Int64 )

import GHC.IOBase	( IO, IOArray, newIOArray,
			  unsafeReadIOArray, unsafeWriteIOArray, unsafePerformIO,
			  IORef, newIORef, readIORef, writeIORef )
#else
import Data.Char	( ord )
import Data.IORef	( IORef, newIORef, readIORef, writeIORef )
import System.IO.Unsafe	( unsafePerformIO )
import Data.Int		( Int64 )
#  if defined(__HUGS__)
import Hugs.IOArray	( IOArray, newIOArray,
			  unsafeReadIOArray, unsafeWriteIOArray )
#  elif defined(__NHC__)
import NHC.IOExtras	( IOArray, newIOArray )
#  endif
#endif
import Control.Monad	( mapM, mapM_, sequence_ )


-----------------------------------------------------------------------

iNSTRUMENTED :: Bool
iNSTRUMENTED = False

-----------------------------------------------------------------------

readHTArray  :: HTArray a -> Int32 -> IO a
writeMutArray :: MutArray a -> Int32 -> a -> IO ()
freezeArray  :: MutArray a -> IO (HTArray a)
thawArray    :: HTArray a -> IO (MutArray a)
newMutArray   :: (Int32, Int32) -> a -> IO (MutArray a)
#if defined(DEBUG) || defined(__NHC__)
type MutArray a = IOArray Int32 a
type HTArray a = MutArray a
newMutArray = newArray
readHTArray  = readArray
writeMutArray = writeArray
freezeArray = return
thawArray = return
#else
type MutArray a = IOArray Int32 a
type HTArray a = MutArray a -- Array Int32 a
newMutArray = newIOArray
readHTArray arr i = readMutArray arr i -- return $! (unsafeAt arr (fromIntegral i))
readMutArray  :: MutArray a -> Int32 -> IO a
readMutArray arr i = unsafeReadIOArray arr (fromIntegral i)
writeMutArray arr i x = unsafeWriteIOArray arr (fromIntegral i) x
freezeArray = return -- unsafeFreeze
thawArray = return -- unsafeThaw
#endif

data HashTable key val = HashTable {
	                             cmp     :: !(key -> key -> Bool),
	                             hash_fn :: !(key -> Int32),
                                     tab     :: !(IORef (HT key val))
                                   }
-- TODO: the IORef should really be an MVar.

data HT key val
  = HT {
	kcount  :: !Int32,              -- Total number of keys.
        bmask   :: !Int32,
	buckets :: !(HTArray [(key,val)])
       }

-- ------------------------------------------------------------
-- Instrumentation for performance tuning

-- This ought to be roundly ignored after optimization when
-- iNSTRUMENTED=False.

-- STRICT version of modifyIORef!
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef r f = do
  v <- readIORef r
  let z = f v in z `seq` writeIORef r z

data HashData = HD {
  tables :: !Integer,
  insertions :: !Integer,
  lookups :: !Integer,
  totBuckets :: !Integer,
  maxEntries :: !Int32,
  maxChain :: !Int,
  maxBuckets :: !Int32
} deriving (Eq, Show)

{-# NOINLINE hashData #-}
hashData :: IORef HashData
hashData =  unsafePerformIO (newIORef (HD { tables=0, insertions=0, lookups=0,
                                            totBuckets=0, maxEntries=0,
                                            maxChain=0, maxBuckets=tABLE_MIN } ))

instrument :: (HashData -> HashData) -> IO ()
instrument i | iNSTRUMENTED = modifyIORef hashData i
             | otherwise    = return ()

recordNew :: IO ()
recordNew = instrument rec
  where rec hd@HD{ tables=t, totBuckets=b } =
               hd{ tables=t+1, totBuckets=b+fromIntegral tABLE_MIN }

recordIns :: Int32 -> Int32 -> [a] -> IO ()
recordIns i sz bkt = instrument rec
  where rec hd@HD{ insertions=ins, maxEntries=mx, maxChain=mc } =
               hd{ insertions=ins+fromIntegral i, maxEntries=mx `max` sz,
                   maxChain=mc `max` length bkt }

recordResize :: Int32 -> Int32 -> IO ()
recordResize older newer = instrument rec
  where rec hd@HD{ totBuckets=b, maxBuckets=mx } =
               hd{ totBuckets=b+fromIntegral (newer-older),
                   maxBuckets=mx `max` newer }

recordLookup :: IO ()
recordLookup = instrument lkup
  where lkup hd@HD{ lookups=l } = hd{ lookups=l+1 }

-- stats :: IO String
-- stats =  fmap show $ readIORef hashData

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

golden :: Int32
golden = -1640531527

-- | A sample (and useful) hash function for Int and Int32,
-- implemented by extracting the uppermost 32 bits of the 64-bit
-- result of multiplying by a 32-bit constant.  The constant is from
-- Knuth, derived from the golden ratio:
--
-- > golden = round ((sqrt 5 - 1) * 2^31) :: Int
hashInt :: Int -> Int32
hashInt x = mulHi (fromIntegral x) golden

-- hi 32 bits of a x-bit * 32 bit -> 64-bit multiply
mulHi :: Int32 -> Int32 -> Int32
mulHi a b = fromIntegral (r `shiftR` 32)
  where r :: Int64
        r = fromIntegral a * fromIntegral b :: Int64

-- | A sample hash function for Strings.  We keep multiplying by the
-- golden ratio and adding.  The implementation is:
--
-- > hashString = foldl' f 0
-- >   where f m c = fromIntegral (ord c) + mulHi m golden
--
-- Note that this has not been extensively tested for reasonability,
-- but Knuth argues that repeated multiplication by the golden ratio
-- will minimize gaps in the hash space.
hashString :: String -> Int32
hashString = foldl' f 0
  where f m c = fromIntegral (ord c) + mulHi m golden

-- | A prime larger than the maximum hash table size
prime :: Int32
prime = 33554467

-- -----------------------------------------------------------------------------
-- Parameters

tABLE_MAX :: Int32
tABLE_MAX  = 32 * 1024 * 1024   -- Maximum size of hash table
tABLE_MIN :: Int32
tABLE_MIN  = 8

hLOAD :: Int32
hLOAD = 7                       -- Maximum average load of a single hash bucket

hYSTERESIS :: Int32
hYSTERESIS = 64                 -- entries to ignore in load computation

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
  recordNew
  -- make a new hash table with a single, empty, segment
  let mask = tABLE_MIN-1
  bkts'  <- newMutArray (0,mask) []
  bkts   <- freezeArray bkts'

  let
    kcnt = 0
    ht = HT {  buckets=bkts, kcount=kcnt, bmask=mask }

  table <- newIORef ht
  return (HashTable { tab=table, hash_fn=hash, cmp=cmpr })

-- -----------------------------------------------------------------------------
-- Inserting a key\/value pair into the hash table

-- | Inserts a key\/value mapping into the hash table.
--
-- Note that 'insert' doesn't remove the old entry from the table -
-- the behaviour is like an association list, where 'lookup' returns
-- the most-recently-inserted mapping for a key in the table.  The
-- reason for this is to keep 'insert' as efficient as possible.  If
-- you need to update a mapping, then we provide 'update'.
--
insert :: HashTable key val -> key -> val -> IO ()

insert ht key val =
  updatingBucket CanInsert (\bucket -> ((key,val):bucket, 1, ())) ht key


-- ------------------------------------------------------------
-- The core of the implementation is lurking down here, in findBucket,
-- updatingBucket, and expandHashTable.

tooBig :: Int32 -> Int32 -> Bool
tooBig k b = k-hYSTERESIS > hLOAD * b

-- index of bucket within table.
bucketIndex :: Int32 -> Int32 -> Int32
bucketIndex mask h = h .&. mask

-- find the bucket in which the key belongs.
-- returns (key equality, bucket index, bucket)
--
-- This rather grab-bag approach gives enough power to do pretty much
-- any bucket-finding thing you might want to do.  We rely on inlining
-- to throw away the stuff we don't want.  I'm proud to say that this
-- plus updatingBucket below reduce most of the other definitions to a
-- few lines of code, while actually speeding up the hashtable
-- implementation when compared with a version which does everything
-- from scratch.
{-# INLINE findBucket #-}
findBucket :: HashTable key val -> key -> IO (HT key val, Int32, [(key,val)])
findBucket HashTable{ tab=ref, hash_fn=hash} key = do
  table@HT{ buckets=bkts, bmask=b } <- readIORef ref
  let indx = bucketIndex b (hash key)
  bucket <- readHTArray bkts indx
  return (table, indx, bucket)

data Inserts = CanInsert
             | Can'tInsert
             deriving (Eq)

-- updatingBucket is the real workhorse of all single-element table
-- updates.  It takes a hashtable and a key, along with a function
-- describing what to do with the bucket in which that key belongs.  A
-- flag indicates whether this function may perform table insertions.
-- The function returns the new contents of the bucket, the number of
-- bucket entries inserted (negative if entries were deleted), and a
-- value which becomes the return value for the function as a whole.
-- The table sizing is enforced here, calling out to expandSubTable as
-- necessary.

-- This function is intended to be inlined and specialized for every
-- calling context (eg every provided bucketFn).
{-# INLINE updatingBucket #-}

updatingBucket :: Inserts -> ([(key,val)] -> ([(key,val)], Int32, a)) ->
                  HashTable key val -> key ->
                  IO a
updatingBucket canEnlarge bucketFn
               ht@HashTable{ tab=ref, hash_fn=hash } key = do
  (table@HT{ kcount=k, buckets=bkts, bmask=b },
   indx, bckt) <- findBucket ht key
  (bckt', inserts, result) <- return $ bucketFn bckt
  let k' = k + inserts
      table1 = table { kcount=k' }
  bkts' <- thawArray bkts
  writeMutArray bkts' indx bckt'
  freezeArray bkts'
  table2 <- if canEnlarge == CanInsert && inserts > 0 then do
               recordIns inserts k' bckt'
               if tooBig k' b
                  then expandHashTable hash table1
                  else return table1
            else return table1
  writeIORef ref table2
  return result

expandHashTable :: (key -> Int32) -> HT key val -> IO (HT key val)
expandHashTable hash table@HT{ buckets=bkts, bmask=mask } = do
   let
      oldsize = mask + 1
      newmask = mask + mask + 1
   recordResize oldsize (newmask+1)
   --
   if newmask > tABLE_MAX-1
      then return table
      else do
   --
   newbkts' <- newMutArray (0,newmask) []

   let
    splitBucket oldindex = do
      bucket <- readHTArray bkts oldindex
      let (oldb,newb) =
              partition ((oldindex==). bucketIndex newmask . hash . fst) bucket
      writeMutArray newbkts' oldindex oldb
      writeMutArray newbkts' (oldindex + oldsize) newb
   mapM_ splitBucket [0..mask]

   newbkts <- freezeArray newbkts'

   return ( table{ buckets=newbkts, bmask=newmask } )

-- -----------------------------------------------------------------------------
-- Deleting a mapping from the hash table

-- Remove a key from a bucket
deleteBucket :: (key -> Bool) -> [(key,val)] -> ([(key, val)], Int32, ())
deleteBucket _   [] = ([],0,())
deleteBucket del (pair@(k,_):bucket) =
  case deleteBucket del bucket of
    (bucket', dels, _) | del k     -> dels' `seq` (bucket', dels', ())
                       | otherwise -> (pair:bucket', dels, ())
      where dels' = dels - 1

-- | Remove an entry from the hash table.
delete :: HashTable key val -> key -> IO ()

delete ht@HashTable{ cmp=eq } key =
  updatingBucket Can'tInsert (deleteBucket (eq key)) ht key

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

update ht@HashTable{ cmp=eq } key val =
  updatingBucket CanInsert
    (\bucket -> let (bucket', dels, _) = deleteBucket (eq key) bucket
                in  ((key,val):bucket', 1+dels, dels/=0))
    ht key

-- -----------------------------------------------------------------------------
-- Looking up an entry in the hash table

-- | Looks up the value of a key in the hash table.
lookup :: HashTable key val -> key -> IO (Maybe val)

lookup ht@HashTable{ cmp=eq } key = do
  recordLookup
  (_, _, bucket) <- findBucket ht key
  let firstHit (k,v) r | eq key k  = Just v
                       | otherwise = r
  return (foldr firstHit Nothing bucket)

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
toList :: HashTable key val -> IO [(key,val)]
toList = mapReduce id concat

{-# INLINE mapReduce #-}
mapReduce :: ([(key,val)] -> r) -> ([r] -> r) -> HashTable key val -> IO r
mapReduce m r HashTable{ tab=ref } = do
  HT{ buckets=bckts, bmask=b } <- readIORef ref
  fmap r (mapM (fmap m . readHTArray bckts) [0..b])

-- -----------------------------------------------------------------------------
-- Diagnostics

-- | This function is useful for determining whether your hash
-- function is working well for your data set.  It returns the longest
-- chain of key\/value pairs in the hash table for which all the keys
-- hash to the same bucket.  If this chain is particularly long (say,
-- longer than 14 elements or so), then it might be a good idea to try
-- a different hash function.
--
longestChain :: HashTable key val -> IO [(key,val)]
longestChain = mapReduce id (maximumBy lengthCmp)
  where lengthCmp (_:x)(_:y) = lengthCmp x y
        lengthCmp []   []    = EQ
        lengthCmp []   _     = LT
        lengthCmp _    []    = GT
