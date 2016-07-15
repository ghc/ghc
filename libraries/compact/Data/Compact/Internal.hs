{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
-- This module provides a data structure, called a Compact, for
-- holding fully evaluated data in a consecutive block of memory.
--
-- This is a private implementation detail of the package and should
-- not be imported directly.
--
-- /Since: 1.0.0/

module Data.Compact.Internal(
  Compact(..),
  compactResize,
  isCompact,
  inCompact,

  compactAppendEvaledInternal,
) where

-- Write down all GHC.Prim deps explicitly to keep them at minimum
import GHC.Prim (Compact#,
                 compactAppend#,
                 compactResize#,
                 compactContains#,
                 compactContainsAny#,
                 State#,
                 RealWorld,
                 Int#,
                 )
-- We need to import Word from GHC.Types to see the representation
-- and to able to access the Word# to pass down the primops
import GHC.Types (IO(..), Word(..), isTrue#)

-- | A 'Compact' contains fully evaluated, pure, and immutable data. If
-- any object in the compact is alive, then the whole compact is
-- alive. This means that 'Compact's are very cheap to keep around,
-- because the data inside a compact does not need to be traversed by
-- the garbage collector. However, the tradeoff is that the memory
-- that contains a 'Compact' cannot be recovered until the whole 'Compact'
-- is garbage.
data Compact a = Compact Compact# a

-- |Check if the second argument is inside the Compact
inCompact :: Compact b -> a -> IO Bool
inCompact (Compact buffer _) !val =
  IO (\s -> case compactContains# buffer val s of
         (# s', v #) -> (# s', isTrue# v #) )

-- |Check if the argument is in any Compact
isCompact :: a -> IO Bool
isCompact !val =
  IO (\s -> case compactContainsAny# val s of
         (# s', v #) -> (# s', isTrue# v #) )

compactResize :: Compact a -> Word -> IO ()
compactResize (Compact oldBuffer _) (W# new_size) =
  IO (\s -> case compactResize# oldBuffer new_size s of
         s' -> (# s', () #) )

compactAppendEvaledInternal :: Compact# -> a -> Int# -> State# RealWorld ->
                               (# State# RealWorld, Compact a #)
compactAppendEvaledInternal buffer root share s =
  case compactAppend# buffer root share s of
    (# s', adjustedRoot #) -> (# s', Compact buffer adjustedRoot #)
