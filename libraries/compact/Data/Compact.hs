{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-name-shadowing #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Compact
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
--  binary serialization), this can lead to substantial speed ups.
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

module Data.Compact (
  -- * The Compact type
  Compact,

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
  ) where

import Control.Concurrent
import GHC.Prim
import GHC.Types

import Data.Compact.Internal as Internal

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
-- The object in question must not contain any functions or mutable data; if it
-- does, 'compact' will raise an exception.  In the future, we may add a type
-- class which will help statically check if this is the case or not.
--
compact :: a -> IO (Compact a)
compact = Internal.compactSized 31268 False

-- | Compact a value, retaining any internal sharing and
-- cycles. /O(size of data)/
--
-- This is typically about 10x slower than 'compact', because it works
-- by maintaining a hash table mapping uncompacted objects to
-- compacted objects.
--
-- The object in question must not contain any functions or mutable data; if it
-- does, 'compact' will raise an exception.  In the future, we may add a type
-- class which will help statically check if this is the case or not.
--
compactWithSharing :: a -> IO (Compact a)
compactWithSharing = Internal.compactSized 31268 True

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

-- | *Experimental.*  This function doesn't actually resize a compact
-- region; rather, it changes the default block size which we allocate
-- when the current block runs out of space, and also appends a block
-- to the compact region.
--
compactResize :: Compact a -> Word -> IO ()
compactResize (Compact oldBuffer _ lock) (W# new_size) =
  withMVar lock $ \_ -> IO $ \s ->
    case compactResize# oldBuffer new_size s of
      s' -> (# s', () #)
