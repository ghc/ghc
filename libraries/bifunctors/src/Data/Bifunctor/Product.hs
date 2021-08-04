{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

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
-- Copyright   :  (C) 2008-2016 Jesse Selover, Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- The product of two bifunctors.
----------------------------------------------------------------------------
module Data.Bifunctor.Product
  ( Product(..)
  ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import qualified Control.Arrow as A
import Control.Category
import Data.Biapplicative
import Data.Bifoldable
import Data.Bifunctor.Functor
import Data.Bitraversable

#if __GLASGOW_HASKELL__ < 710
import Data.Monoid hiding (Product)
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

-- | Form the product of two bifunctors
data Product f g a b = Pair (f a b) (g a b)
  deriving ( Eq, Ord, Show, Read
#if __GLASGOW_HASKELL__ >= 702
           , Generic
#endif
#if __GLASGOW_HASKELL__ >= 708
           , Generic1
           , Typeable
#endif
           )

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 708
data ProductMetaData
data ProductMetaCons

instance Datatype ProductMetaData where
    datatypeName _ = "Product"
    moduleName _ = "Data.Bifunctor.Product"

instance Constructor ProductMetaCons where
    conName _ = "Pair"

instance Generic1 (Product f g a) where
    type Rep1 (Product f g a) = D1 ProductMetaData (C1 ProductMetaCons ((:*:)
        (S1 NoSelector (Rec1 (f a)))
        (S1 NoSelector (Rec1 (g a)))))
    from1 (Pair f g) = M1 (M1 (M1 (Rec1 f) :*: M1 (Rec1 g)))
    to1 (M1 (M1 (M1 f :*: M1 g))) = Pair (unRec1 f) (unRec1 g)
#endif

#if LIFTED_FUNCTOR_CLASSES
instance (Eq2 f, Eq2 g, Eq a) => Eq1 (Product f g a) where
  liftEq = liftEq2 (==)
instance (Eq2 f, Eq2 g) => Eq2 (Product f g) where
  liftEq2 f g (Pair x1 y1) (Pair x2 y2) =
    liftEq2 f g x1 x2 && liftEq2 f g y1 y2

instance (Ord2 f, Ord2 g, Ord a) => Ord1 (Product f g a) where
  liftCompare = liftCompare2 compare
instance (Ord2 f, Ord2 g) => Ord2 (Product f g) where
  liftCompare2 f g (Pair x1 y1) (Pair x2 y2) =
    liftCompare2 f g x1 x2 `mappend` liftCompare2 f g y1 y2

instance (Read2 f, Read2 g, Read a) => Read1 (Product f g a) where
  liftReadsPrec = liftReadsPrec2 readsPrec readList
instance (Read2 f, Read2 g) => Read2 (Product f g) where
  liftReadsPrec2 rp1 rl1 rp2 rl2 = readsData $
    readsBinaryWith (liftReadsPrec2 rp1 rl1 rp2 rl2)
                    (liftReadsPrec2 rp1 rl1 rp2 rl2)
                    "Pair" Pair

instance (Show2 f, Show2 g, Show a) => Show1 (Product f g a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList
instance (Show2 f, Show2 g) => Show2 (Product f g) where
  liftShowsPrec2 sp1 sl1 sp2 sl2 p (Pair x y) =
    showsBinaryWith (liftShowsPrec2 sp1 sl1 sp2 sl2)
                    (liftShowsPrec2 sp1 sl1 sp2 sl2)
                    "Pair" p x y
#endif

instance (Bifunctor f, Bifunctor g) => Bifunctor (Product f g) where
  first f (Pair x y) = Pair (first f x) (first f y)
  {-# INLINE first #-}
  second g (Pair x y) = Pair (second g x) (second g y)
  {-# INLINE second #-}
  bimap f g (Pair x y) = Pair (bimap f g x) (bimap f g y)
  {-# INLINE bimap #-}

instance (Biapplicative f, Biapplicative g) => Biapplicative (Product f g) where
  bipure a b = Pair (bipure a b) (bipure a b)
  {-# INLINE bipure #-}
  Pair w x <<*>> Pair y z = Pair (w <<*>> y) (x <<*>> z)
  {-# INLINE (<<*>>) #-}

instance (Bifoldable f, Bifoldable g) => Bifoldable (Product f g) where
  bifoldMap f g (Pair x y) = bifoldMap f g x `mappend` bifoldMap f g y
  {-# INLINE bifoldMap #-}

instance (Bitraversable f, Bitraversable g) => Bitraversable (Product f g) where
  bitraverse f g (Pair x y) = Pair <$> bitraverse f g x <*> bitraverse f g y
  {-# INLINE bitraverse #-}

instance BifunctorFunctor (Product p) where
  bifmap f (Pair p q) = Pair p (f q)

instance BifunctorComonad (Product p) where
  biextract (Pair _ q) = q
  biduplicate pq@(Pair p _) = Pair p pq
  biextend f pq@(Pair p _) = Pair p (f pq)

instance (Category p, Category q) => Category (Product p q) where
  id = Pair id id
  Pair x y . Pair x' y' = Pair (x . x') (y . y') 

instance (A.Arrow p, A.Arrow q) => A.Arrow (Product p q) where
  arr f = Pair (A.arr f) (A.arr f)
  first (Pair x y) = Pair (A.first x) (A.first y)
  second (Pair x y) = Pair (A.second x) (A.second y)
  Pair x y *** Pair x' y' = Pair (x A.*** x') (y A.*** y')
  Pair x y &&& Pair x' y' = Pair (x A.&&& x') (y A.&&& y')

instance (A.ArrowChoice p, A.ArrowChoice q) => A.ArrowChoice (Product p q) where
  left (Pair x y) = Pair (A.left x) (A.left y)
  right (Pair x y) = Pair (A.right x) (A.right y)
  Pair x y +++ Pair x' y' = Pair (x A.+++ x') (y A.+++ y')
  Pair x y ||| Pair x' y' = Pair (x A.||| x') (y A.||| y')  

instance (A.ArrowLoop p, A.ArrowLoop q) => A.ArrowLoop (Product p q) where
  loop (Pair x y) = Pair (A.loop x) (A.loop y)

instance (A.ArrowZero p, A.ArrowZero q) => A.ArrowZero (Product p q) where
  zeroArrow = Pair A.zeroArrow A.zeroArrow

instance (A.ArrowPlus p, A.ArrowPlus q) => A.ArrowPlus (Product p q) where
  Pair x y <+> Pair x' y' = Pair (x A.<+> x') (y A.<+> y')
