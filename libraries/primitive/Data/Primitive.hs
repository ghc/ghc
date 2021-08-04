{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
-- |
-- Module      : Data.Primitive
-- Copyright   : (c) Roman Leshchinskiy 2009-2012
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
--
-- Reexports all primitive operations
--
module Data.Primitive (
  -- * Re-exports
  module Data.Primitive.Types
  ,module Data.Primitive.Array
  ,module Data.Primitive.ByteArray
  ,module Data.Primitive.SmallArray
  ,module Data.Primitive.PrimArray
  ,module Data.Primitive.MutVar
  -- * Naming Conventions
  -- $namingConventions
) where

import Data.Primitive.Types
import Data.Primitive.Array
import Data.Primitive.ByteArray
import Data.Primitive.SmallArray
import Data.Primitive.PrimArray
import Data.Primitive.MutVar

{- $namingConventions
For historical reasons, this library embraces the practice of suffixing
the name of a function with the type it operates on. For example, three
of the variants of the array indexing function are:

> indexArray      ::           Array      a -> Int -> a
> indexSmallArray ::           SmallArray a -> Int -> a
> indexPrimArray  :: Prim a => PrimArray  a -> Int -> a

In a few places, where the language sounds more natural, the array type
is instead used as a prefix. For example, @Data.Primitive.SmallArray@
exports @smallArrayFromList@, which would sound unnatural if it used
@SmallArray@ as a suffix instead.

This library provides several functions traversing, building, and filtering
arrays. These functions are suffixed with an additional character to
indicate their the nature of their effectfulness:

* No suffix: A non-effectful pass over the array.
* @-A@ suffix: An effectful pass over the array, where the effect is 'Applicative'.
* @-P@ suffix: An effectful pass over the array, where the effect is 'PrimMonad'.

Additionally, an apostrophe can be used to indicate strictness in the elements.
The variants with an apostrophe are used in @Data.Primitive.Array@ but not
in @Data.Primitive.PrimArray@ since the array type it provides is always strict in the element.
For example, there are three variants of the function that filters elements
from a primitive array.

> filterPrimArray  :: (Prim a               ) => (a ->   Bool) -> PrimArray a ->    PrimArray a
> filterPrimArrayA :: (Prim a, Applicative f) => (a -> f Bool) -> PrimArray a -> f (PrimArray a)
> filterPrimArrayP :: (Prim a, PrimMonad   m) => (a -> m Bool) -> PrimArray a -> m (PrimArray a)

As long as the effectful context is a monad that is sufficiently affine
the behaviors of the 'Applicative' and 'PrimMonad' variants produce the same results
and differ only in their strictness. Monads that are sufficiently affine
include:

* 'IO' and 'ST'
* Any combination of 'MaybeT', 'ExceptT', 'StateT' and 'Writer' on top
  of another sufficiently affine monad.
* Any Monad which does not include backtracking or other mechanism where an effect can
happen more than once is an Affine Monad in the sense we care about. ContT, LogicT, ListT are all
examples of search/control monads which are NOT affine: they can run a sub computation more than once.

There is one situation where the names deviate from effectful suffix convention
described above. Throughout the haskell ecosystem, the 'Applicative' variant of
'map' is known as 'traverse', not @mapA@. Consequently, we adopt the following
naming convention for mapping:

> mapPrimArray :: (Prim a, Prim b) => (a -> b) -> PrimArray a -> PrimArray b
> traversePrimArray :: (Applicative f, Prim a, Prim b) => (a -> f b) -> PrimArray a -> f (PrimArray b)
> traversePrimArrayP :: (PrimMonad m, Prim a, Prim b) => (a -> m b) -> PrimArray a -> m (PrimArray b)
-}
