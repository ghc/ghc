{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Prim.PtrEq
-- License     :  see libraries/ghc-internal/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Comparing underlying pointers for equality.
--
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Internal.Prim.PtrEq
  ( reallyUnsafePtrEquality,
    unsafePtrEquality#,
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
    samePromptTag#,
    eqStableName#
  ) where

import GHC.Internal.Prim
import GHC.Internal.Types -- Also make implicit dependency known to build system
  ( RuntimeRep(BoxedRep), UnliftedType )
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

-- | Compare the underlying pointers of two unlifted values for equality.
--
-- This is less dangerous than 'reallyUnsafePtrEquality',
-- since the arguments are guaranteed to be evaluated.
-- This means there is no risk of accidentally comparing
-- a thunk.
-- It's however still more dangerous than e.g. 'sameArray#'.
--
unsafePtrEquality# :: forall (a :: UnliftedType) (b :: UnliftedType). a -> b -> Int#
unsafePtrEquality# = reallyUnsafePtrEquality#
-- See Note [Pointer comparison operations]
--   in primops.txt.pp

-- | Compare the underlying pointers of two arrays.
sameArray# :: forall {l} (a :: TYPE (BoxedRep l)). Array# a -> Array# a -> Int#
sameArray# = unsafePtrEquality#

-- | Compare the underlying pointers of two mutable arrays.
sameMutableArray# :: forall {l} s (a :: TYPE (BoxedRep l)). MutableArray# s a -> MutableArray# s a -> Int#
sameMutableArray# = unsafePtrEquality#

-- | Compare the underlying pointers of two small arrays.
sameSmallArray# :: forall {l} (a :: TYPE (BoxedRep l)). SmallArray# a -> SmallArray# a -> Int#
sameSmallArray# = unsafePtrEquality#

-- | Compare the underlying pointers of two small mutable arrays.
sameSmallMutableArray# :: forall {l} s (a :: TYPE (BoxedRep l)). SmallMutableArray# s a -> SmallMutableArray# s a -> Int#
sameSmallMutableArray# = unsafePtrEquality#

-- | Compare the pointers of two byte arrays.
sameByteArray# :: ByteArray# -> ByteArray# -> Int#
sameByteArray# = unsafePtrEquality#

-- | Compare the underlying pointers of two mutable byte arrays.
sameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Int#
sameMutableByteArray# = unsafePtrEquality#

-- | Compare the underlying pointers of two 'MutVar#'s.
sameMutVar# :: forall {l} s (a :: TYPE (BoxedRep l)). MutVar# s a -> MutVar# s a -> Int#
sameMutVar# = unsafePtrEquality#

-- | Compare the underlying pointers of two 'TVar#'s.
sameTVar# :: forall {l} s (a :: TYPE (BoxedRep l)). TVar# s a -> TVar# s a -> Int#
sameTVar# = unsafePtrEquality#

-- | Compare the underlying pointers of two 'MVar#'s.
sameMVar# :: forall {l} s (a :: TYPE (BoxedRep l)). MVar# s a -> MVar# s a -> Int#
sameMVar# = unsafePtrEquality#

-- | Compare the underlying pointers of two 'IOPort#'s.
sameIOPort# :: forall {l} s (a :: TYPE (BoxedRep l)). IOPort# s a -> IOPort# s a -> Int#
sameIOPort# = unsafePtrEquality#

-- | Compare the underlying pointers of two 'PromptTag#'s.
samePromptTag# :: forall a. PromptTag# a -> PromptTag# a -> Int#
samePromptTag# = unsafePtrEquality#

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
eqStableName# :: forall {k} {l} (a :: TYPE (BoxedRep k)) (b :: TYPE (BoxedRep l))
              . StableName# a -> StableName# b -> Int#
eqStableName# = unsafePtrEquality#
