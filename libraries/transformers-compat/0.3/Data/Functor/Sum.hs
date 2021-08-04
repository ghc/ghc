{-# LANGUAGE CPP #-}

#ifndef HASKELL98
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
# if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
# endif
# if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
# endif
# if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
# endif
#endif
-- |
-- Module      :  Data.Functor.Sum
-- Copyright   :  (c) Ross Paterson 2014
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Sums, lifted to functors.

module Data.Functor.Sum (
    Sum(..),
  ) where

import Control.Applicative
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Classes
import Data.Monoid (mappend)
import Data.Traversable (Traversable(traverse))

#ifndef HASKELL98
# ifdef GENERIC_DERIVING
import Generics.Deriving.Base
# elif __GLASGOW_HASKELL__ >= 702
import GHC.Generics
# endif
# if __GLASGOW_HASKELL__ >= 708
import Data.Data
# endif
#endif

-- | Lifted sum of functors.
data Sum f g a = InL (f a) | InR (g a)

#ifndef HASKELL98
# if __GLASGOW_HASKELL__ >= 702 || defined(GENERIC_DERIVING)
-- Generic(1) instances for Sum
instance Generic (Sum f g a) where
    type Rep (Sum f g a) =
      D1 MDSum (C1 MCInL (S1 NoSelector (Rec0 (f a)))
            :+: C1 MCInR (S1 NoSelector (Rec0 (g a))))
    from (InL f) = M1 (L1 (M1 (M1 (K1 f))))
    from (InR g) = M1 (R1 (M1 (M1 (K1 g))))
    to (M1 (L1 (M1 (M1 (K1 f))))) = InL f
    to (M1 (R1 (M1 (M1 (K1 g))))) = InR g

instance Generic1 (Sum f g) where
    type Rep1 (Sum f g) =
      D1 MDSum (C1 MCInL (S1 NoSelector (Rec1 f))
            :+: C1 MCInR (S1 NoSelector (Rec1 g)))
    from1 (InL f) = M1 (L1 (M1 (M1 (Rec1 f))))
    from1 (InR g) = M1 (R1 (M1 (M1 (Rec1 g))))
    to1 (M1 (L1 (M1 (M1 f)))) = InL (unRec1 f)
    to1 (M1 (R1 (M1 (M1 g)))) = InR (unRec1 g)

data MDSum
data MCInL
data MCInR

instance Datatype MDSum where
    datatypeName _ = "Sum"
    moduleName   _ = "Data.Functor.Sum"

instance Constructor MCInL where
    conName _ = "InL"

instance Constructor MCInR where
    conName _ = "InR"
# endif

# if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable Sum
deriving instance (Data (f a), Data (g a), Typeable f, Typeable g, Typeable a)
               => Data (Sum (f :: * -> *) (g :: * -> *) (a :: *))
# endif
#endif

instance (Eq1 f, Eq1 g) => Eq1 (Sum f g) where
    liftEq eq (InL x1) (InL x2) = liftEq eq x1 x2
    liftEq _ (InL _) (InR _) = False
    liftEq _ (InR _) (InL _) = False
    liftEq eq (InR y1) (InR y2) = liftEq eq y1 y2

instance (Ord1 f, Ord1 g) => Ord1 (Sum f g) where
    liftCompare comp (InL x1) (InL x2) = liftCompare comp x1 x2
    liftCompare _ (InL _) (InR _) = LT
    liftCompare _ (InR _) (InL _) = GT
    liftCompare comp (InR y1) (InR y2) = liftCompare comp y1 y2

instance (Read1 f, Read1 g) => Read1 (Sum f g) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "InL" InL `mappend`
        readsUnaryWith (liftReadsPrec rp rl) "InR" InR

instance (Show1 f, Show1 g) => Show1 (Sum f g) where
    liftShowsPrec sp sl d (InL x) =
        showsUnaryWith (liftShowsPrec sp sl) "InL" d x
    liftShowsPrec sp sl d (InR y) =
        showsUnaryWith (liftShowsPrec sp sl) "InR" d y

instance (Eq1 f, Eq1 g, Eq a) => Eq (Sum f g a) where
    (==) = eq1
instance (Ord1 f, Ord1 g, Ord a) => Ord (Sum f g a) where
    compare = compare1
instance (Read1 f, Read1 g, Read a) => Read (Sum f g a) where
    readsPrec = readsPrec1
instance (Show1 f, Show1 g, Show a) => Show (Sum f g a) where
    showsPrec = showsPrec1

instance (Functor f, Functor g) => Functor (Sum f g) where
    fmap f (InL x) = InL (fmap f x)
    fmap f (InR y) = InR (fmap f y)

instance (Foldable f, Foldable g) => Foldable (Sum f g) where
    foldMap f (InL x) = foldMap f x
    foldMap f (InR y) = foldMap f y

instance (Traversable f, Traversable g) => Traversable (Sum f g) where
    traverse f (InL x) = InL <$> traverse f x
    traverse f (InR y) = InR <$> traverse f y
