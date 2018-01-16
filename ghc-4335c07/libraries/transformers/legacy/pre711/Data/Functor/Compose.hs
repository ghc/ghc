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
-- Module      :  Data.Functor.Compose
-- Copyright   :  (c) Ross Paterson 2010
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Composition of functors.
-----------------------------------------------------------------------------

module Data.Functor.Compose (
    Compose(..),
  ) where

import Data.Functor.Classes

import Control.Applicative
#if __GLASGOW_HASKELL__ >= 708
import Data.Data
#endif
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

infixr 9 `Compose`

-- | Right-to-left composition of functors.
-- The composition of applicative functors is always applicative,
-- but the composition of monads is not always a monad.
newtype Compose f g a = Compose { getCompose :: f (g a) }

#if __GLASGOW_HASKELL__ >= 702
deriving instance Generic (Compose f g a)

instance Functor f => Generic1 (Compose f g) where
    type Rep1 (Compose f g) =
      D1 MDCompose
        (C1 MCCompose
          (S1 MSCompose (f :.: Rec1 g)))
    from1 (Compose x) = M1 (M1 (M1 (Comp1 (fmap Rec1 x))))
    to1 (M1 (M1 (M1 x))) = Compose (fmap unRec1 (unComp1 x))

data MDCompose
data MCCompose
data MSCompose

instance Datatype MDCompose where
    datatypeName _ = "Compose"
    moduleName   _ = "Data.Functor.Compose"
# if __GLASGOW_HASKELL__ >= 708
    isNewtype    _ = True
# endif

instance Constructor MCCompose where
    conName     _ = "Compose"
    conIsRecord _ = True

instance Selector MSCompose where
    selName _ = "getCompose"
#endif

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable Compose
deriving instance (Data (f (g a)), Typeable f, Typeable g, Typeable a)
               => Data (Compose (f :: * -> *) (g :: * -> *) (a :: *))
#endif

-- Instances of lifted Prelude classes

instance (Eq1 f, Eq1 g) => Eq1 (Compose f g) where
    liftEq eq (Compose x) (Compose y) = liftEq (liftEq eq) x y

instance (Ord1 f, Ord1 g) => Ord1 (Compose f g) where
    liftCompare comp (Compose x) (Compose y) =
        liftCompare (liftCompare comp) x y

instance (Read1 f, Read1 g) => Read1 (Compose f g) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "Compose" Compose
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show1 f, Show1 g) => Show1 (Compose f g) where
    liftShowsPrec sp sl d (Compose x) =
        showsUnaryWith (liftShowsPrec sp' sl') "Compose" d x
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

-- Instances of Prelude classes

instance (Eq1 f, Eq1 g, Eq a) => Eq (Compose f g a) where
    (==) = eq1

instance (Ord1 f, Ord1 g, Ord a) => Ord (Compose f g a) where
    compare = compare1

instance (Read1 f, Read1 g, Read a) => Read (Compose f g a) where
    readsPrec = readsPrec1

instance (Show1 f, Show1 g, Show a) => Show (Compose f g a) where
    showsPrec = showsPrec1

-- Functor instances

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose (fmap (fmap f) x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose t) = foldMap (foldMap f) t

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose t) = Compose <$> traverse (traverse f) t

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure x = Compose (pure (pure x))
    Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

instance (Alternative f, Applicative g) => Alternative (Compose f g) where
    empty = Compose empty
    Compose x <|> Compose y = Compose (x <|> y)
