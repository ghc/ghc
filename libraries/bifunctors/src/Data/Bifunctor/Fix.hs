{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- Module      :  Data.Bifunctor.Fix
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Data.Bifunctor.Fix
  ( Fix(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
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

-- | Greatest fixpoint of a 'Bifunctor' (a 'Functor' over the first argument with zipping).
newtype Fix p a = In { out :: p (Fix p a) a }
  deriving
    (
#if __GLASGOW_HASKELL__ >= 702
      Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
    , Typeable
#endif
    )

deriving instance Eq   (p (Fix p a) a) => Eq   (Fix p a)
deriving instance Ord  (p (Fix p a) a) => Ord  (Fix p a)
deriving instance Show (p (Fix p a) a) => Show (Fix p a)
deriving instance Read (p (Fix p a) a) => Read (Fix p a)

#if LIFTED_FUNCTOR_CLASSES
instance Eq2 p => Eq1 (Fix p) where
  liftEq f (In x) (In y) = liftEq2 (liftEq f) f x y

instance Ord2 p => Ord1 (Fix p) where
  liftCompare f (In x) (In y) = liftCompare2 (liftCompare f) f x y

instance Read2 p => Read1 (Fix p) where
  liftReadsPrec rp1 rl1 p = readParen (p > 10) $ \s0 -> do
    ("In",  s1) <- lex s0
    ("{",   s2) <- lex s1
    ("out", s3) <- lex s2
    (x,     s4) <- liftReadsPrec2 (liftReadsPrec rp1 rl1) (liftReadList rp1 rl1)
                                  rp1 rl1 0 s3
    ("}",   s5) <- lex s4
    return (In x, s5)

instance Show2 p => Show1 (Fix p) where
  liftShowsPrec sp1 sl1 p (In x) = showParen (p > 10) $
      showString "In {out = "
    . liftShowsPrec2 (liftShowsPrec sp1 sl1) (liftShowList sp1 sl1)
                     sp1 sl1 0 x
    . showChar '}'
#endif

instance Bifunctor p => Functor (Fix p) where
  fmap f (In p) = In (bimap (fmap f) f p)
  {-# INLINE fmap #-}

instance Biapplicative p => Applicative (Fix p) where
  pure a = In (bipure (pure a) a)
  {-# INLINE pure #-}
  In p <*> In q = In (biliftA2 (<*>) ($) p q)
  {-# INLINE (<*>) #-}

instance Bifoldable p => Foldable (Fix p) where
  foldMap f (In p) = bifoldMap (foldMap f) f p
  {-# INLINE foldMap #-}

instance Bitraversable p => Traversable (Fix p) where
  traverse f (In p) = In <$> bitraverse (traverse f) f p
  {-# INLINE traverse #-}
