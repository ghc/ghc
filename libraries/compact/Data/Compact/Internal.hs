{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Compact.Internal
-- Copyright   :  (c) The University of Glasgow 2001-2009
--                (c) Giovanni Campagna <gcampagn@cs.stanford.edu> 2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  unstable
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides some internal functions and representation for dealing
-- with compact regions, which may be of interest if you need higher
-- performance.
--
-- /Since: 1.0.0/

module Data.Compact.Internal
  ( Compact(..)
  , mkCompact
  , compactSized
  ) where

import Control.Concurrent.MVar
import Control.DeepSeq
import GHC.Prim
import GHC.Types

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
-- The cost of compaction is similar to the cost of GC for the same
-- data, but it is perfomed only once.  However, because
-- "Data.Compact.compact" does not stop-the-world, retaining internal
-- sharing during the compaction process is very costly. The user
-- can choose wether to 'compact' or 'compactWithSharing'.
--
-- When you have a @'Compact' a@, you can get a pointer to the actual object
-- in the region using "Data.Compact.getCompact".  The 'Compact' type
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
-- * Objects with mutable pointer fields also cannot be compacted,
--   because subsequent mutation would destroy the property that a compact is
--   self-contained.
--
-- If compaction encounters any of the above, a 'CompactionFailed'
-- exception will be thrown by the compaction operation.
--
data Compact a = Compact Compact# a (MVar ())
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

-- | Transfer @a@ into a new compact region, with a preallocated size,
-- possibly preserving sharing or not.  If you know how big the data
-- structure in question is, you can save time by picking an appropriate
-- block size for the compact region.
--
compactSized :: NFData a => Int -> Bool -> a -> IO (Compact a)
compactSized (I# size) share a = IO $ \s0 ->
  case compactNew# (int2Word# size) s0 of { (# s1, compact# #) ->
  case compactAddPrim compact# a s1 of { (# s2, pk #) ->
  mkCompact compact# pk s2 }}
 where
  compactAddPrim
    | share = compactAddWithSharing#
    | otherwise = compactAdd#
