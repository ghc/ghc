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
-- This module provides a data structure, called a Compact, for
-- holding fully evaluated data in a consecutive block of memory.
--
-- /Since: 1.0.0/

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
import Control.DeepSeq (NFData)
import GHC.Prim
import GHC.Types

import Data.Compact.Internal as Internal

-- | Retrieve the object that was stored in a 'Compact'
getCompact :: Compact a -> a
getCompact (Compact _ obj _) = obj

-- | Compact a value. /O(size of unshared data)/
--
-- If the structure contains any internal sharing, the shared data
-- will be duplicated during the compaction process.  Loops if the
-- structure constains cycles.
--
-- The NFData constraint is just to ensure that the object contains no
-- functions, 'compact' does not actually use it.  If your object
-- contains any functions, then 'compact' will fail. (and your
-- 'NFData' instance is lying).
--
compact :: NFData a => a -> IO (Compact a)
compact = Internal.compactSized 31268 False

-- | Compact a value, retaining any internal sharing and
-- cycles. /O(size of data)/
--
-- This is typically about 10x slower than 'compact', because it works
-- by maintaining a hash table mapping uncompacted objects to
-- compacted objects.
--
-- The 'NFData' constraint is just to ensure that the object contains no
-- functions, `compact` does not actually use it.  If your object
-- contains any functions, then 'compactWithSharing' will fail. (and
-- your 'NFData' instance is lying).
--
compactWithSharing :: NFData a => a -> IO (Compact a)
compactWithSharing = Internal.compactSized 31268 True

-- | Add a value to an existing 'Compact'.  Behaves exactly like
-- 'compact' with respect to sharing and the 'NFData' constraint.
compactAdd :: NFData a => Compact b -> a -> IO (Compact a)
compactAdd (Compact compact# _ lock) a = withMVar lock $ \_ -> IO $ \s ->
  case compactAdd# compact# a s of { (# s1, pk #) ->
  (# s1, Compact compact# pk lock #) }

-- | Add a value to an existing 'Compact'.  Behaves exactly like
-- 'compactWithSharing' with respect to sharing and the 'NFData'
-- constraint.
compactAddWithSharing :: NFData a => Compact b -> a -> IO (Compact a)
compactAddWithSharing (Compact compact# _ lock) a =
  withMVar lock $ \_ -> IO $ \s ->
    case compactAddWithSharing# compact# a s of { (# s1, pk #) ->
    (# s1, Compact compact# pk lock #) }


-- | Check if the second argument is inside the 'Compact'
inCompact :: Compact b -> a -> IO Bool
inCompact (Compact buffer _ _) !val =
  IO (\s -> case compactContains# buffer val s of
         (# s', v #) -> (# s', isTrue# v #) )

-- | Check if the argument is in any 'Compact'
isCompact :: a -> IO Bool
isCompact !val =
  IO (\s -> case compactContainsAny# val s of
         (# s', v #) -> (# s', isTrue# v #) )

compactSize :: Compact a -> IO Word
compactSize (Compact buffer _ lock) = withMVar lock $ \_ -> IO $ \s0 ->
   case compactSize# buffer s0 of (# s1, sz #) -> (# s1, W# sz #)

compactResize :: Compact a -> Word -> IO ()
compactResize (Compact oldBuffer _ lock) (W# new_size) =
  withMVar lock $ \_ -> IO $ \s ->
    case compactResize# oldBuffer new_size s of
      s' -> (# s', () #)
