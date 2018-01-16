{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
#endif
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Functor.Product
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Products, lifted to functors.
-----------------------------------------------------------------------------

module Data.Functor.Product (
    Product(..),
  ) where

import Control.Applicative
import Control.Monad (MonadPlus(..))
import Control.Monad.Fix (MonadFix(..))
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
#if __GLASGOW_HASKELL__ >= 708
import Data.Data
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Functor.Classes
import Data.Monoid (mappend)
import Data.Traversable (Traversable(traverse))
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

-- | Lifted product of functors.
data Product f g a = Pair (f a) (g a)

#if __GLASGOW_HASKELL__ >= 702
deriving instance Generic (Product f g a)

instance Generic1 (Product f g) where
    type Rep1 (Product f g) =
      D1 MDProduct
        (C1 MCPair
          (S1 NoSelector (Rec1 f) :*: S1 NoSelector (Rec1 g)))
    from1 (Pair f g) = M1 (M1 (M1 (Rec1 f) :*: M1 (Rec1 g)))
    to1 (M1 (M1 (M1 f :*: M1 g))) = Pair (unRec1 f) (unRec1 g)

data MDProduct
data MCPair

instance Datatype MDProduct where
    datatypeName _ = "Product"
    moduleName   _ = "Data.Functor.Product"

instance Constructor MCPair where
    conName _ = "Pair"
#endif

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable Product
deriving instance (Data (f a), Data (g a), Typeable f, Typeable g, Typeable a)
               => Data (Product (f :: * -> *) (g :: * -> *) (a :: *))
#endif

instance (Eq1 f, Eq1 g) => Eq1 (Product f g) where
    liftEq eq (Pair x1 y1) (Pair x2 y2) = liftEq eq x1 x2 && liftEq eq y1 y2

instance (Ord1 f, Ord1 g) => Ord1 (Product f g) where
    liftCompare comp (Pair x1 y1) (Pair x2 y2) =
        liftCompare comp x1 x2 `mappend` liftCompare comp y1 y2

instance (Read1 f, Read1 g) => Read1 (Product f g) where
    liftReadsPrec rp rl = readsData $
        readsBinaryWith (liftReadsPrec rp rl) (liftReadsPrec rp rl) "Pair" Pair

instance (Show1 f, Show1 g) => Show1 (Product f g) where
    liftShowsPrec sp sl d (Pair x y) =
        showsBinaryWith (liftShowsPrec sp sl) (liftShowsPrec sp sl) "Pair" d x y

instance (Eq1 f, Eq1 g, Eq a) => Eq (Product f g a)
    where (==) = eq1
instance (Ord1 f, Ord1 g, Ord a) => Ord (Product f g a) where
    compare = compare1
instance (Read1 f, Read1 g, Read a) => Read (Product f g a) where
    readsPrec = readsPrec1
instance (Show1 f, Show1 g, Show a) => Show (Product f g a) where
    showsPrec = showsPrec1

instance (Functor f, Functor g) => Functor (Product f g) where
    fmap f (Pair x y) = Pair (fmap f x) (fmap f y)

instance (Foldable f, Foldable g) => Foldable (Product f g) where
    foldMap f (Pair x y) = foldMap f x `mappend` foldMap f y

instance (Traversable f, Traversable g) => Traversable (Product f g) where
    traverse f (Pair x y) = Pair <$> traverse f x <*> traverse f y

instance (Applicative f, Applicative g) => Applicative (Product f g) where
    pure x = Pair (pure x) (pure x)
    Pair f g <*> Pair x y = Pair (f <*> x) (g <*> y)

instance (Alternative f, Alternative g) => Alternative (Product f g) where
    empty = Pair empty empty
    Pair x1 y1 <|> Pair x2 y2 = Pair (x1 <|> x2) (y1 <|> y2)

instance (Monad f, Monad g) => Monad (Product f g) where
#if !(MIN_VERSION_base(4,8,0))
    return x = Pair (return x) (return x)
#endif
    Pair m n >>= f = Pair (m >>= fstP . f) (n >>= sndP . f)
      where
        fstP (Pair a _) = a
        sndP (Pair _ b) = b

instance (MonadPlus f, MonadPlus g) => MonadPlus (Product f g) where
    mzero = Pair mzero mzero
    Pair x1 y1 `mplus` Pair x2 y2 = Pair (x1 `mplus` x2) (y1 `mplus` y2)

instance (MonadFix f, MonadFix g) => MonadFix (Product f g) where
    mfix f = Pair (mfix (fstP . f)) (mfix (sndP . f))
      where
        fstP (Pair a _) = a
        sndP (Pair _ b) = b

#if MIN_VERSION_base(4,4,0)
instance (MonadZip f, MonadZip g) => MonadZip (Product f g) where
    mzipWith f (Pair x1 y1) (Pair x2 y2) = Pair (mzipWith f x1 x2) (mzipWith f y1 y2)
#endif
