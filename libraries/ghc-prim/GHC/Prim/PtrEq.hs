{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Prim.PtrEq
-- License     :  see libraries/ghc-prim/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Comparing underlying pointers for equality.
--
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Prim.PtrEq
  ( reallyUnsafePtrEquality,
    sameArray#,
    sameMutableArray#,
    sameSmallArray#,
    sameSmallMutableArray#,
    sameByteArray#,
    sameMutableByteArray#,
    sameMutVar#,
    sameTVar#,
    sameMVar#,
    sameIOPort#,
    eqStableName#
  ) where

import GHC.Prim
import GHC.Types () -- Make implicit dependency known to build system
default () -- Double and Integer aren't available yet

{- **********************************************************************
*                                                                       *
*                        Pointer equality                               *
*                                                                       *
********************************************************************** -}

{- Note [Pointer equality operations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Many primitive types - such as Array#, ByteArray#, MVar#, ... - are boxed:
they are represented by pointers to the underlying data. It is thus possible
to directly compare these pointers for equality, as opposed to comparing
the underlying data that the pointers refer to (for instance, comparing
two arrays element-wise).

To do this, GHC provides the primop reallyUnsafePtrEquality#, which is
both levity-polymorphic and heterogeneous. As its name indicates, it is an
unsafe operation which can yield unpredictable results, as explained in
Note [Pointer comparison operations] in primops.txt.pp

For a more user-friendly interface, this module defines specialisations of
the reallyUnsafePtrEquality# primop at various primitive types, such as
Array#, ByteArray#, MVar#, ...
-}

-- | Compare the underlying pointers of two values for equality.
--
-- Returns @1@ if the pointers are equal and @0@ otherwise.
--
-- The two values must be of the same type, of kind 'Type'.
-- See also 'GHC.Exts.reallyUnsafePtrEquality#', which doesn't have
-- such restrictions.
reallyUnsafePtrEquality :: a -> a -> Int#
reallyUnsafePtrEquality = reallyUnsafePtrEquality#
-- See Note [Pointer comparison operations]
--   in primops.txt.pp

-- | Compare the underlying pointers of two arrays.
sameArray# :: Array# a -> Array# a -> Int#
sameArray# = reallyUnsafePtrEquality#

-- | Compare the underlying pointers of two mutable arrays.
sameMutableArray# :: MutableArray# s a -> MutableArray# s a -> Int#
sameMutableArray# = reallyUnsafePtrEquality#

-- | Compare the underlying pointers of two small arrays.
sameSmallArray# :: SmallArray# a -> SmallArray# a -> Int#
sameSmallArray# = reallyUnsafePtrEquality#

-- | Compare the underlying pointers of two small mutable arrays.
sameSmallMutableArray# :: SmallMutableArray# s a -> SmallMutableArray# s a -> Int#
sameSmallMutableArray# = reallyUnsafePtrEquality#

-- | Compare the pointers of two byte arrays.
sameByteArray# :: ByteArray# -> ByteArray# -> Int#
sameByteArray# = reallyUnsafePtrEquality#

-- | Compare the underlying pointers of two mutable byte arrays.
sameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Int#
sameMutableByteArray# = reallyUnsafePtrEquality#

-- | Compare the underlying pointers of two 'MutVar#'s.
sameMutVar# :: MutVar# s a -> MutVar# s a -> Int#
sameMutVar# = reallyUnsafePtrEquality#

-- | Compare the underlying pointers of two 'TVar#'s.
sameTVar# :: TVar# s a -> TVar# s a -> Int#
sameTVar# = reallyUnsafePtrEquality#

-- | Compare the underlying pointers of two 'MVar#'s.
sameMVar# :: MVar# s a -> MVar# s a -> Int#
sameMVar# = reallyUnsafePtrEquality#

-- | Compare the underlying pointers of two 'IOPort#'s.
sameIOPort# :: IOPort# s a -> IOPort# s a -> Int#
sameIOPort# = reallyUnsafePtrEquality#

-- Note [Comparing stable names]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A StableName# is actually a pointer to a stable name object (SNO)
-- containing an index into the stable name table (SNT). We
-- used to compare StableName#s by following the pointers to the
-- SNOs and checking whether they held the same SNT indices. However,
-- this is not necessary: there is a one-to-one correspondence
-- between SNOs and entries in the SNT, so simple pointer equality
-- does the trick.

-- | Compare two stable names for equality.
eqStableName# :: StableName# a -> StableName# b -> Int#
eqStableName# = reallyUnsafePtrEquality#
