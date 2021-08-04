{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#include "bifunctors-common.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Bifunctor.Flip
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifunctor.Flip
  ( Flip(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor.Functor
import Data.Bitraversable

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
import Data.Monoid
import Data.Traversable
#endif

#if __GLASGOW_HASKELL__ >= 708
import Data.Typeable
#endif

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

#if LIFTED_FUNCTOR_CLASSES
import Data.Functor.Classes
#endif

-- | Make a 'Bifunctor' flipping the arguments of a 'Bifunctor'.
newtype Flip p a b = Flip { runFlip :: p b a }
  deriving ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Typeable
#endif
           )

#if LIFTED_FUNCTOR_CLASSES
instance (Eq2 p, Eq a) => Eq1 (Flip p a) where
  liftEq = liftEq2 (==)
instance Eq2 p => Eq2 (Flip p) where
  liftEq2 f g (Flip x) (Flip y) = liftEq2 g f x y

instance (Ord2 p, Ord a) => Ord1 (Flip p a) where
  liftCompare = liftCompare2 compare
instance Ord2 p => Ord2 (Flip p) where
  liftCompare2 f g (Flip x) (Flip y) = liftCompare2 g f x y

instance (Read2 p, Read a) => Read1 (Flip p a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList
instance Read2 p => Read2 (Flip p) where
  liftReadsPrec2 rp1 rl1 rp2 rl2 p = readParen (p > 10) $ \s0 -> do
    ("Flip",    s1) <- lex s0
    ("{",       s2) <- lex s1
    ("runFlip", s3) <- lex s2
    (x,         s4) <- liftReadsPrec2 rp2 rl2 rp1 rl1 0 s3
    ("}",       s5) <- lex s4
    return (Flip x, s5)

instance (Show2 p, Show a) => Show1 (Flip p a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
instance Show2 p => Show2 (Flip p) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (Flip x) = showParen (p > 10) $
      showString "Flip {runFlip = "
    . liftShowsPrec2 sp2 sl2 sp1 sl1 0 x
    . showChar '}'
#endif

instance Bifunctor p => Bifunctor (Flip p) where
  first f = Flip . second f . runFlip
  {-# INLINE first #-}
  second f = Flip . first f . runFlip
  {-# INLINE second #-}
  bimap f g = Flip . bimap g f . runFlip
  {-# INLINE bimap #-}

instance Bifunctor p => Functor (Flip p a) where
  fmap f = Flip . first f . runFlip
  {-# INLINE fmap #-}

instance Biapplicative p => Biapplicative (Flip p) where
  bipure a b = Flip (bipure b a)
  {-# INLINE bipure #-}

  Flip fg <<*>> Flip xy = Flip (fg <<*>> xy)
  {-# INLINE (<<*>>) #-}

instance Bifoldable p => Bifoldable (Flip p) where
  bifoldMap f g = bifoldMap g f . runFlip
  {-# INLINE bifoldMap #-}

instance Bifoldable p => Foldable (Flip p a) where
  foldMap f = bifoldMap f (const mempty) . runFlip
  {-# INLINE foldMap #-}

instance Bitraversable p => Bitraversable (Flip p) where
  bitraverse f g = fmap Flip . bitraverse g f . runFlip
  {-# INLINE bitraverse #-}

instance Bitraversable p => Traversable (Flip p a) where
  traverse f = fmap Flip . bitraverse f pure . runFlip
  {-# INLINE traverse #-}

instance BifunctorFunctor Flip where
  bifmap f (Flip p) = Flip (f p)
