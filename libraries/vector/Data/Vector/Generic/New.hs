{-# LANGUAGE CPP, Rank2Types, FlexibleContexts, MultiParamTypeClasses #-}

-- |
-- Module      : Data.Vector.Generic.New
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable
--
-- Purely functional interface to initialisation of mutable vectors
--

module Data.Vector.Generic.New (
  New(..), create, run, runPrim, apply, modify, modifyWithBundle,
  unstream, transform, unstreamR, transformR,
  slice, init, tail, take, drop,
  unsafeSlice, unsafeInit, unsafeTail
) where

import qualified Data.Vector.Generic.Mutable as MVector

import           Data.Vector.Generic.Base ( Vector, Mutable )

import           Data.Vector.Fusion.Bundle ( Bundle )
import qualified Data.Vector.Fusion.Bundle as Bundle
import           Data.Vector.Fusion.Stream.Monadic ( Stream )
import           Data.Vector.Fusion.Bundle.Size

import Control.Monad.Primitive
import Control.Monad.ST ( ST )
import Control.Monad  ( liftM )
import Prelude hiding ( init, tail, take, drop, reverse, map, filter )

-- Data.Vector.Internal.Check is unused
#define NOT_VECTOR_MODULE
#include "vector.h"

data New v a = New (forall s. ST s (Mutable v s a))

create :: (forall s. ST s (Mutable v s a)) -> New v a
{-# INLINE create #-}
create p = New p

run :: New v a -> ST s (Mutable v s a)
{-# INLINE run #-}
run (New p) = p

runPrim :: PrimMonad m => New v a -> m (Mutable v (PrimState m) a)
{-# INLINE runPrim #-}
runPrim (New p) = primToPrim p

apply :: (forall s. Mutable v s a -> Mutable v s a) -> New v a -> New v a
{-# INLINE apply #-}
apply f (New p) = New (liftM f p)

modify :: (forall s. Mutable v s a -> ST s ()) -> New v a -> New v a
{-# INLINE modify #-}
modify f (New p) = New (do { v <- p; f v; return v })

modifyWithBundle :: (forall s. Mutable v s a -> Bundle u b -> ST s ())
                 -> New v a -> Bundle u b -> New v a
{-# INLINE_FUSED modifyWithBundle #-}
modifyWithBundle f (New p) s = s `seq` New (do { v <- p; f v s; return v })

unstream :: Vector v a => Bundle v a -> New v a
{-# INLINE_FUSED unstream #-}
unstream s = s `seq` New (MVector.vunstream s)

transform
  :: Vector v a => (forall m. Monad m => Stream m a -> Stream m a)
                -> (Size -> Size) -> New v a -> New v a
{-# INLINE_FUSED transform #-}
transform f _ (New p) = New (MVector.transform f =<< p)

{-# RULES

"transform/transform [New]"
  forall (f1 :: forall m. Monad m => Stream m a -> Stream m a)
         (f2 :: forall m. Monad m => Stream m a -> Stream m a)
         g1 g2 p .
  transform f1 g1 (transform f2 g2 p) = transform (f1 . f2) (g1 . g2) p

"transform/unstream [New]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a)
         g s.
  transform f g (unstream s) = unstream (Bundle.inplace f g s)  #-}




unstreamR :: Vector v a => Bundle v a -> New v a
{-# INLINE_FUSED unstreamR #-}
unstreamR s = s `seq` New (MVector.unstreamR s)

transformR
  :: Vector v a => (forall m. Monad m => Stream m a -> Stream m a)
                -> (Size -> Size) -> New v a -> New v a
{-# INLINE_FUSED transformR #-}
transformR f _ (New p) = New (MVector.transformR f =<< p)

{-# RULES

"transformR/transformR [New]"
  forall (f1 :: forall m. Monad m => Stream m a -> Stream m a)
         (f2 :: forall m. Monad m => Stream m a -> Stream m a)
         g1 g2
         p .
  transformR f1 g1 (transformR f2 g2 p) = transformR (f1 . f2) (g1 . g2) p

"transformR/unstreamR [New]"
  forall (f :: forall m. Monad m => Stream m a -> Stream m a)
         g s.
  transformR f g (unstreamR s) = unstreamR (Bundle.inplace f g s)  #-}



slice :: Vector v a => Int -> Int -> New v a -> New v a
{-# INLINE_FUSED slice #-}
slice i n m = apply (MVector.slice i n) m

init :: Vector v a => New v a -> New v a
{-# INLINE_FUSED init #-}
init m = apply MVector.init m

tail :: Vector v a => New v a -> New v a
{-# INLINE_FUSED tail #-}
tail m = apply MVector.tail m

take :: Vector v a => Int -> New v a -> New v a
{-# INLINE_FUSED take #-}
take n m = apply (MVector.take n) m

drop :: Vector v a => Int -> New v a -> New v a
{-# INLINE_FUSED drop #-}
drop n m = apply (MVector.drop n) m

unsafeSlice :: Vector v a => Int -> Int -> New v a -> New v a
{-# INLINE_FUSED unsafeSlice #-}
unsafeSlice i n m = apply (MVector.unsafeSlice i n) m

unsafeInit :: Vector v a => New v a -> New v a
{-# INLINE_FUSED unsafeInit #-}
unsafeInit m = apply MVector.unsafeInit m

unsafeTail :: Vector v a => New v a -> New v a
{-# INLINE_FUSED unsafeTail #-}
unsafeTail m = apply MVector.unsafeTail m

{-# RULES

"slice/unstream [New]" forall i n s.
  slice i n (unstream s) = unstream (Bundle.slice i n s)

"init/unstream [New]" forall s.
  init (unstream s) = unstream (Bundle.init s)

"tail/unstream [New]" forall s.
  tail (unstream s) = unstream (Bundle.tail s)

"take/unstream [New]" forall n s.
  take n (unstream s) = unstream (Bundle.take n s)

"drop/unstream [New]" forall n s.
  drop n (unstream s) = unstream (Bundle.drop n s)

"unsafeSlice/unstream [New]" forall i n s.
  unsafeSlice i n (unstream s) = unstream (Bundle.slice i n s)

"unsafeInit/unstream [New]" forall s.
  unsafeInit (unstream s) = unstream (Bundle.init s)

"unsafeTail/unstream [New]" forall s.
  unsafeTail (unstream s) = unstream (Bundle.tail s)   #-}



