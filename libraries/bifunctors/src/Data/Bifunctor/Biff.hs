{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#include "bifunctors-common.h"

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2008-2016 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Bifunctor.Biff
  ( Biff(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Biapplicative
import Data.Bifoldable
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

-- | Compose two 'Functor's on the inside of a 'Bifunctor'.
newtype Biff p f g a b = Biff { runBiff :: p (f a) (g b) }
  deriving ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Typeable
#endif
           )
#if __GLASGOW_HASKELL__ >= 702
# if __GLASGOW_HASKELL__ >= 708
deriving instance Functor (p (f a)) => Generic1 (Biff p f g a)
# else
data BiffMetaData
data BiffMetaCons
data BiffMetaSel

instance Datatype BiffMetaData where
    datatypeName = const "Biff"
    moduleName = const "Data.Bifunctor.Biff"

instance Constructor BiffMetaCons where
    conName = const "Biff"
    conIsRecord = const True

instance Selector BiffMetaSel where
    selName = const "runBiff"

instance Functor (p (f a)) => Generic1 (Biff p f g a) where
    type Rep1 (Biff p f g a) = D1 BiffMetaData (C1 BiffMetaCons
        (S1 BiffMetaSel (p (f a) :.: Rec1 g)))
    from1 = M1 . M1 . M1 . Comp1 . fmap Rec1 . runBiff
    to1 = Biff . fmap unRec1 . unComp1 . unM1 . unM1 . unM1
# endif
#endif

#if LIFTED_FUNCTOR_CLASSES
instance (Eq2 p, Eq1 f, Eq1 g, Eq a) => Eq1 (Biff p f g a) where
  liftEq = liftEq2 (==)
instance (Eq2 p, Eq1 f, Eq1 g) => Eq2 (Biff p f g) where
  liftEq2 f g (Biff x) (Biff y) = liftEq2 (liftEq f) (liftEq g) x y

instance (Ord2 p, Ord1 f, Ord1 g, Ord a) => Ord1 (Biff p f g a) where
  liftCompare = liftCompare2 compare
instance (Ord2 p, Ord1 f, Ord1 g) => Ord2 (Biff p f g) where
  liftCompare2 f g (Biff x) (Biff y) = liftCompare2 (liftCompare f) (liftCompare g) x y

instance (Read2 p, Read1 f, Read1 g, Read a) => Read1 (Biff p f g a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList
instance (Read2 p, Read1 f, Read1 g) => Read2 (Biff p f g) where
  liftReadsPrec2 rp1 rl1 rp2 rl2 p = readParen (p > 10) $ \s0 -> do
    ("Biff",    s1) <- lex s0
    ("{",       s2) <- lex s1
    ("runBiff", s3) <- lex s2
    (x,         s4) <- liftReadsPrec2 (liftReadsPrec rp1 rl1) (liftReadList rp1 rl1)
                                      (liftReadsPrec rp2 rl2) (liftReadList rp2 rl2) 0 s3
    ("}",       s5) <- lex s4
    return (Biff x, s5)

instance (Show2 p, Show1 f, Show1 g, Show a) => Show1 (Biff p f g a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
instance (Show2 p, Show1 f, Show1 g) => Show2 (Biff p f g) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (Biff x) = showParen (p > 10) $
      showString "Biff {runBiff = "
    . liftShowsPrec2 (liftShowsPrec sp1 sl1) (liftShowList sp1 sl1)
                     (liftShowsPrec sp2 sl2) (liftShowList sp2 sl2) 0 x
    . showChar '}'
#endif

instance (Bifunctor p, Functor f, Functor g) => Bifunctor (Biff p f g) where
  first f = Biff . first (fmap f) . runBiff
  {-# INLINE first #-}
  second f = Biff . second (fmap f) . runBiff
  {-# INLINE second #-}
  bimap f g = Biff . bimap (fmap f) (fmap g) . runBiff
  {-# INLINE bimap #-}

instance (Bifunctor p, Functor g) => Functor (Biff p f g a) where
  fmap f = Biff . second (fmap f) . runBiff
  {-# INLINE fmap #-}

instance (Biapplicative p, Applicative f, Applicative g) => Biapplicative (Biff p f g) where
  bipure a b = Biff (bipure (pure a) (pure b))
  {-# INLINE bipure #-}

  Biff fg <<*>> Biff xy = Biff (bimap (<*>) (<*>) fg <<*>> xy)
  {-# INLINE (<<*>>) #-}

instance (Bifoldable p, Foldable g) => Foldable (Biff p f g a) where
  foldMap f = bifoldMap (const mempty) (foldMap f) . runBiff
  {-# INLINE foldMap #-}

instance (Bifoldable p, Foldable f, Foldable g) => Bifoldable (Biff p f g) where
  bifoldMap f g = bifoldMap (foldMap f) (foldMap g) . runBiff
  {-# INLINE bifoldMap #-}

instance (Bitraversable p, Traversable g) => Traversable (Biff p f g a) where
  traverse f = fmap Biff . bitraverse pure (traverse f) . runBiff
  {-# INLINE traverse #-}

instance (Bitraversable p, Traversable f, Traversable g) => Bitraversable (Biff p f g) where
  bitraverse f g = fmap Biff . bitraverse (traverse f) (traverse g) . runBiff
  {-# INLINE bitraverse #-}
