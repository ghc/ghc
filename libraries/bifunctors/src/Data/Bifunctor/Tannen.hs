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
module Data.Bifunctor.Tannen
  ( Tannen(..)
  ) where

import Control.Applicative

import Control.Arrow as A
import Control.Category
import Control.Comonad

import Data.Bifunctor as B
import Data.Bifunctor.Functor
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

import Prelude hiding ((.),id)

-- | Compose a 'Functor' on the outside of a 'Bifunctor'.
newtype Tannen f p a b = Tannen { runTannen :: f (p a b) }
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
deriving instance Functor f => Generic1 (Tannen f p a)
# else
data TannenMetaData
data TannenMetaCons
data TannenMetaSel

instance Datatype TannenMetaData where
    datatypeName _ = "Tannen"
    moduleName _ = "Data.Bifunctor.Tannen"

instance Constructor TannenMetaCons where
    conName _ = "Tannen"
    conIsRecord _ = True

instance Selector TannenMetaSel where
    selName _ = "runTannen"

instance Functor f => Generic1 (Tannen f p a) where
    type Rep1 (Tannen f p a) = D1 TannenMetaData (C1 TannenMetaCons
        (S1 TannenMetaSel (f :.: Rec1 (p a))))
    from1 = M1 . M1 . M1 . Comp1 . fmap Rec1 . runTannen
    to1 = Tannen . fmap unRec1 . unComp1 . unM1 . unM1 . unM1
# endif
#endif

#if LIFTED_FUNCTOR_CLASSES
instance (Eq1 f, Eq2 p, Eq a) => Eq1 (Tannen f p a) where
  liftEq = liftEq2 (==)
instance (Eq1 f, Eq2 p) => Eq2 (Tannen f p) where
  liftEq2 f g (Tannen x) (Tannen y) = liftEq (liftEq2 f g) x y

instance (Ord1 f, Ord2 p, Ord a) => Ord1 (Tannen f p a) where
  liftCompare = liftCompare2 compare
instance (Ord1 f, Ord2 p) => Ord2 (Tannen f p) where
  liftCompare2 f g (Tannen x) (Tannen y) = liftCompare (liftCompare2 f g) x y

instance (Read1 f, Read2 p, Read a) => Read1 (Tannen f p a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList
instance (Read1 f, Read2 p) => Read2 (Tannen f p) where
  liftReadsPrec2 rp1 rl1 rp2 rl2 p = readParen (p > 10) $ \s0 -> do
    ("Tannen",    s1) <- lex s0
    ("{",         s2) <- lex s1
    ("runTannen", s3) <- lex s2
    (x,           s4) <- liftReadsPrec (liftReadsPrec2 rp1 rl1 rp2 rl2)
                                       (liftReadList2  rp1 rl1 rp2 rl2) 0 s3
    ("}",         s5) <- lex s4
    return (Tannen x, s5)

instance (Show1 f, Show2 p, Show a) => Show1 (Tannen f p a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
instance (Show1 f, Show2 p) => Show2 (Tannen f p) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (Tannen x) = showParen (p > 10) $
      showString "Tannen {runTannen = "
    . liftShowsPrec (liftShowsPrec2 sp1 sl1 sp2 sl2)
                    (liftShowList2  sp1 sl1 sp2 sl2) 0 x
    . showChar '}'
#endif

instance Functor f => BifunctorFunctor (Tannen f) where
  bifmap f (Tannen fp) = Tannen (fmap f fp)

instance (Functor f, Monad f) => BifunctorMonad (Tannen f) where
  bireturn = Tannen . return
  bibind f (Tannen fp) = Tannen $ fp >>= runTannen . f

instance Comonad f => BifunctorComonad (Tannen f) where
  biextract = extract . runTannen
  biextend f (Tannen fp) = Tannen (extend (f . Tannen) fp)

instance (Functor f, Bifunctor p) => Bifunctor (Tannen f p) where
  first f = Tannen . fmap (B.first f) . runTannen
  {-# INLINE first #-}
  second f = Tannen . fmap (B.second f) . runTannen
  {-# INLINE second #-}
  bimap f g = Tannen . fmap (bimap f g) . runTannen
  {-# INLINE bimap #-}

instance (Functor f, Bifunctor p) => Functor (Tannen f p a) where
  fmap f = Tannen . fmap (B.second f) . runTannen
  {-# INLINE fmap #-}

instance (Applicative f, Biapplicative p) => Biapplicative (Tannen f p) where
  bipure a b = Tannen (pure (bipure a b))
  {-# INLINE bipure #-}

  Tannen fg <<*>> Tannen xy = Tannen ((<<*>>) <$> fg <*> xy)
  {-# INLINE (<<*>>) #-}

instance (Foldable f, Bifoldable p) => Foldable (Tannen f p a) where
  foldMap f = foldMap (bifoldMap (const mempty) f) . runTannen
  {-# INLINE foldMap #-}

instance (Foldable f, Bifoldable p) => Bifoldable (Tannen f p) where
  bifoldMap f g = foldMap (bifoldMap f g) . runTannen
  {-# INLINE bifoldMap #-}

instance (Traversable f, Bitraversable p) => Traversable (Tannen f p a) where
  traverse f = fmap Tannen . traverse (bitraverse pure f) . runTannen
  {-# INLINE traverse #-}

instance (Traversable f, Bitraversable p) => Bitraversable (Tannen f p) where
  bitraverse f g = fmap Tannen . traverse (bitraverse f g) . runTannen
  {-# INLINE bitraverse #-}

instance (Applicative f, Category p) => Category (Tannen f p) where
  id = Tannen $ pure id
  Tannen fpbc . Tannen fpab = Tannen $ liftA2 (.) fpbc fpab

instance (Applicative f, Arrow p) => Arrow (Tannen f p) where
  arr f = Tannen $ pure $ arr f
  first = Tannen . fmap A.first . runTannen
  second = Tannen . fmap A.second . runTannen
  Tannen ab *** Tannen cd = Tannen $ liftA2 (***) ab cd
  Tannen ab &&& Tannen ac = Tannen $ liftA2 (&&&) ab ac

instance (Applicative f, ArrowChoice p) => ArrowChoice (Tannen f p) where
  left  = Tannen . fmap left . runTannen
  right = Tannen . fmap right . runTannen
  Tannen ab +++ Tannen cd = Tannen $ liftA2 (+++) ab cd
  Tannen ac ||| Tannen bc = Tannen $ liftA2 (|||) ac bc

instance (Applicative f, ArrowLoop p) => ArrowLoop (Tannen f p) where
  loop = Tannen . fmap loop . runTannen

instance (Applicative f, ArrowZero p) => ArrowZero (Tannen f p) where
  zeroArrow = Tannen $ pure zeroArrow

instance (Applicative f, ArrowPlus p) => ArrowPlus (Tannen f p) where
  Tannen f <+> Tannen g = Tannen (liftA2 (<+>) f g)

