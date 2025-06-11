{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-name-shadowing #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Compact
-- Copyright   :  (c) The University of Glasgow 2001-2009
--                (c) Giovanni Campagna <gcampagn@cs.stanford.edu> 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  unstable
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides a data structure, called a 'Compact', for
-- holding immutable, fully evaluated data in a consecutive block of memory.
-- Compact regions are good for two things:
--
--  1. Data in a compact region is not traversed during GC; any
--  incoming pointer to a compact region keeps the entire region
--  live.  Thus, if you put a long-lived data structure in a compact
--  region, you may save a lot of cycles during major collections,
--  since you will no longer be (uselessly) retraversing this
--  data structure.
--
--  2. Because the data is stored contiguously, you can easily
--  dump the memory to disk and/or send it over the network.
--  For applications that are not bandwidth bound (GHC's heap
--  representation can be as much of a x4 expansion over a
--  binary serialization), this can lead to substantial speedups.
--
-- For example, suppose you have a function @loadBigStruct :: IO BigStruct@,
-- which loads a large data structure from the file system.  You can "compact"
-- the structure with the following code:
--
-- @
--      do r <- 'compact' =<< loadBigStruct
--         let x = 'getCompact' r :: BigStruct
--         -- Do things with x
-- @
--
-- Note that 'compact' will not preserve internal sharing; use
-- 'compactWithSharing' (which is 10x slower) if you have cycles and/or
-- must preserve sharing.  The 'Compact' pointer @r@ can be used
-- to add more data to a compact region; see 'compactAdd' or
-- 'compactAddWithSharing'.
--
-- The implementation of compact regions is described by:
--
--  * Edward Z. Yang, Giovanni Campagna, Ömer Ağacan, Ahmed El-Hassany, Abhishek
--    Kulkarni, Ryan Newton. \"/Efficient communication and Collection with Compact
--    Normal Forms/\". In Proceedings of the 20th ACM SIGPLAN International
--    Conference on Functional Programming. September 2015. <http://ezyang.com/compact.html>
--
-- This library is supported by GHC 8.2 and later.

module GHC.Compact (
  -- * The Compact type
  Compact(..),

  -- * Compacting data
  compact,
  compactWithSharing,
  compactAdd,
  compactAddWithSharing,

  -- * Inspecting a Compact
  getCompact,
  inCompact,
  isCompact,
  compactSize,

  -- * Other utilities
  compactResize,

  -- * Internal operations
  mkCompact,
  compactSized,
  ) where

import Control.Concurrent.MVar
import GHC.Exts
import GHC.IO

-- | A 'Compact' contains fully evaluated, pure, immutable data.
--
-- 'Compact' serves two purposes:
--
-- * Data stored in a 'Compact' has no garbage collection overhead.
--   The garbage collector considers the whole 'Compact' to be alive
--   if there is a reference to any object within it.
--
-- * A 'Compact' can be serialized, stored, and deserialized again.
--   The serialized data can only be deserialized by the exact binary
--   that created it, but it can be stored indefinitely before
--   deserialization.
--
-- Compacts are self-contained, so compacting data involves copying
-- it; if you have data that lives in two 'Compact's, each will have a
-- separate copy of the data.
--
-- The cost of compaction is fully evaluating the data + copying it. However,
-- because 'compact' does not stop-the-world, retaining internal sharing during
-- the compaction process is very costly. The user can choose whether to
-- 'compact' or 'compactWithSharing'.
--
-- When you have a @'Compact' a@, you can get a pointer to the actual object
-- in the region using 'getCompact'.  The 'Compact' type
-- serves as handle on the region itself; you can use this handle
-- to add data to a specific 'Compact' with 'compactAdd' or
-- 'compactAddWithSharing' (giving you a new handle which corresponds
-- to the same compact region, but points to the newly added object
-- in the region).  At the moment, due to technical reasons,
-- it's not possible to get the @'Compact' a@ if you only have an @a@,
-- so make sure you hold on to the handle as necessary.
--
-- Data in a compact doesn't ever move, so compacting data is also a
-- way to pin arbitrary data structures in memory.
--
-- There are some limitations on what can be compacted:
--
-- * Functions.  Compaction only applies to data.
--
-- * Pinned 'ByteArray#' objects cannot be compacted.  This is for a
--   good reason: the memory is pinned so that it can be referenced by
--   address (the address might be stored in a C data structure, for
--   example), so we can't make a copy of it to store in the 'Compact'.
--
-- * Objects with mutable pointer fields (e.g. 'Data.IORef.IORef',
--   'GHC.Array.MutableArray') also cannot be compacted, because subsequent
--   mutation would destroy the property that a compact is self-contained.
--
-- If compaction encounters any of the above, a 'Control.Exception.CompactionFailed'
-- exception will be thrown by the compaction operation.
--
data Compact a = Compact Compact# a !(MVar ())
    -- we can *read* from a Compact without taking a lock, but only
    -- one thread can be writing to the compact at any given time.
    -- The MVar here is to enforce mutual exclusion among writers.
    -- Note: the MVar protects the Compact# only, not the pure value 'a'

-- | Make a new 'Compact' object, given a pointer to the true
-- underlying region.  You must uphold the invariant that @a@ lives
-- in the compact region.
--
mkCompact
  :: Compact# -> a -> State# RealWorld -> (# State# RealWorld, Compact a #)
mkCompact compact# a s =
  case unIO (newMVar ()) s of { (# s1, lock #) ->
  (# s1, Compact compact# a lock #) }
 where
  unIO (IO a) = a

-- | Transfer @a@ into a new compact region, with a preallocated size (in
-- bytes), possibly preserving sharing or not.  If you know how big the data
-- structure in question is, you can save time by picking an appropriate block
-- size for the compact region.
--
compactSized
    :: Int -- ^ Size of the compact region, in bytes
    -> Bool -- ^ Whether to retain internal sharing
    -> a
    -> IO (Compact a)
compactSized (I# size) share a = IO $ \s0 ->
  case compactNew# (int2Word# size) s0 of { (# s1, compact# #) ->
  case compactAddPrim compact# a s1 of { (# s2, pk #) ->
  mkCompact compact# pk s2 }}
 where
  compactAddPrim
    | share = compactAddWithSharing#
    | otherwise = compactAdd#

-- | Retrieve a direct pointer to the value pointed at by a 'Compact' reference.
-- If you have used 'compactAdd', there may be multiple 'Compact' references
-- into the same compact region. Upholds the property:
--
-- > inCompact c (getCompact c) == True
--
getCompact :: Compact a -> a
getCompact (Compact _ obj _) = obj

-- | Compact a value. /O(size of unshared data)/
--
-- If the structure contains any internal sharing, the shared data
-- will be duplicated during the compaction process.  This will
-- not terminate if the structure contains cycles (use 'compactWithSharing'
-- instead).
--
-- The object in question must not contain any functions or data with mutable
-- pointers; if it does, 'compact' will raise an exception. In the future, we
-- may add a type class which will help statically check if this is the case or
-- not.
--
compact :: a -> IO (Compact a)
compact = compactSized 31268 False

-- | Compact a value, retaining any internal sharing and
-- cycles. /O(size of data)/
--
-- This is typically about 10x slower than 'compact', because it works
-- by maintaining a hash table mapping uncompacted objects to
-- compacted objects.
--
-- The object in question must not contain any functions or data with mutable
-- pointers; if it does, 'compact' will raise an exception. In the future, we
-- may add a type class which will help statically check if this is the case or
-- not.
--
compactWithSharing :: a -> IO (Compact a)
compactWithSharing = compactSized 31268 True

-- | Add a value to an existing 'Compact'.  This will help you avoid
-- copying when the value contains pointers into the compact region,
-- but remember that after compaction this value will only be deallocated
-- with the entire compact region.
--
-- Behaves exactly like 'compact' with respect to sharing and what data
-- it accepts.
--
compactAdd :: Compact b -> a -> IO (Compact a)
compactAdd (Compact compact# _ lock) a = withMVar lock $ \_ -> IO $ \s ->
  case compactAdd# compact# a s of { (# s1, pk #) ->
  (# s1, Compact compact# pk lock #) }

-- | Add a value to an existing 'Compact', like 'compactAdd',
-- but behaving exactly like 'compactWithSharing' with respect to sharing and
-- what data it accepts.
--
compactAddWithSharing :: Compact b -> a -> IO (Compact a)
compactAddWithSharing (Compact compact# _ lock) a =
  withMVar lock $ \_ -> IO $ \s ->
    case compactAddWithSharing# compact# a s of { (# s1, pk #) ->
    (# s1, Compact compact# pk lock #) }

-- | Check if the second argument is inside the passed 'Compact'.
--
inCompact :: Compact b -> a -> IO Bool
inCompact (Compact buffer _ _) !val =
  IO (\s -> case compactContains# buffer val s of
         (# s', v #) -> (# s', isTrue# v #) )

-- | Check if the argument is in any 'Compact'.  If true, the value in question
-- is also fully evaluated, since any value in a compact region must
-- be fully evaluated.
--
isCompact :: a -> IO Bool
isCompact !val =
  IO (\s -> case compactContainsAny# val s of
         (# s', v #) -> (# s', isTrue# v #) )

-- | Returns the size in bytes of the compact region.
--
compactSize :: Compact a -> IO Word
compactSize (Compact buffer _ lock) = withMVar lock $ \_ -> IO $ \s0 ->
   case compactSize# buffer s0 of (# s1, sz #) -> (# s1, W# sz #)

-- | __Experimental__  This function doesn't actually resize a compact
-- region; rather, it changes the default block size which we allocate
-- when the current block runs out of space, and also appends a block
-- to the compact region.
--
compactResize :: Compact a -> Word -> IO ()
compactResize (Compact oldBuffer _ lock) (W# new_size) =
  withMVar lock $ \_ -> IO $ \s ->
    case compactResize# oldBuffer new_size s of
      s' -> (# s', () #)
