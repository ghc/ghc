{-# LANGUAGE CPP                        #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric              #-}
#endif

-- This module uses GND
#if __GLASGOW_HASKELL__ >= 702
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
module Data.Bifunctor.Biap
 ( Biap(..)
 ) where

import Control.Applicative
import Control.Monad
import qualified Control.Monad.Fail as Fail (MonadFail)
import Data.Biapplicative
import Data.Bifoldable
import Data.Bitraversable
import Data.Functor.Classes

#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics
#endif

#if !(MIN_VERSION_base(4,8,0))
import Data.Foldable
import Data.Monoid
import Data.Traversable
#endif

import qualified Data.Semigroup as S

-- | Pointwise lifting of a class over two arguments, using
-- 'Biapplicative'.
--
-- Classes that can be lifted include 'Monoid', 'Num' and
-- 'Bounded'. Each method of those classes can be defined as lifting
-- themselves over each argument of 'Biapplicative'.
--
-- @
-- mempty        = bipure mempty          mempty
-- minBound      = bipure minBound        minBound
-- maxBound      = bipure maxBound        maxBound
-- fromInteger n = bipure (fromInteger n) (fromInteger n)
--
-- negate = bimap negate negate
--
-- (+)  = biliftA2 (+)  (+)
-- (<>) = biliftA2 (<>) (<>)
-- @
--
-- 'Biap' is to 'Biapplicative' as 'Data.Monoid.Ap' is to
-- 'Applicative'.
--
-- 'Biap' can be used with @DerivingVia@ to derive a numeric instance
-- for pairs:
--
-- @
-- newtype Numpair a = Np (a, a)
--  deriving (S.Semigroup, Monoid, Num, Bounded)
--  via Biap (,) a a
-- @
--
newtype Biap bi a b = Biap { getBiap :: bi a b }
 deriving ( Eq
          , Ord
          , Show
          , Read
          , Enum
          , Functor
          , Foldable
          , Traversable
          , Alternative
          , Applicative
#if __GLASGOW_HASKELL__ >= 702
          , Generic
#endif
#if __GLASGOW_HASKELL__ >= 706
          , Generic1
#endif
          , Monad
          , Fail.MonadFail
          , MonadPlus
          , Eq1
          , Ord1

          , Bifunctor
          , Biapplicative
          , Bifoldable
#if LIFTED_FUNCTOR_CLASSES
          , Eq2
          , Ord2
#endif
          )

instance Bitraversable bi => Bitraversable (Biap bi) where
 bitraverse f g (Biap as) = Biap <$> bitraverse f g as

instance (Biapplicative bi, S.Semigroup a, S.Semigroup b) => S.Semigroup (Biap bi a b) where
  (<>) = biliftA2 (S.<>) (S.<>)

instance (Biapplicative bi, Monoid a, Monoid b) => Monoid (Biap bi a b) where
  mempty = bipure mempty mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = biliftA2 mappend mappend
#endif

instance (Biapplicative bi, Bounded a, Bounded b) => Bounded (Biap bi a b) where
  minBound = bipure minBound minBound
  maxBound = bipure maxBound maxBound

instance ( Biapplicative bi, Num a, Num b
#if !(MIN_VERSION_base(4,5,0))
           -- Old versions of Num have Eq and Show as superclasses. Sigh.
         , Eq (bi a b), Show (bi a b)
#endif
         ) => Num (Biap bi a b) where
  (+) = biliftA2 (+) (+)
  (*) = biliftA2 (*) (*)

  negate = bimap negate negate
  abs    = bimap abs    abs
  signum = bimap signum signum

  fromInteger n = bipure (fromInteger n) (fromInteger n)

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 706
data BiapMetaData
data BiapMetaCons
data BiapMetaSel

instance Datatype BiapMetaData where
    datatypeName = const "Biap"
    moduleName = const "Data.Bifunctor.Wrapped"

instance Constructor BiapMetaCons where
    conName = const "Biap"
    conIsRecord = const True

instance Selector BiapMetaSel where
    selName = const "getBiap"

instance Generic1 (Biap p a) where
    type Rep1 (Biap p a) = D1 BiapMetaData
        (C1 BiapMetaCons
            (S1 BiapMetaSel (Rec1 (p a))))
    from1 = M1 . M1 . M1 . Rec1 . getBiap
    to1 = Biap . unRec1 . unM1 . unM1 . unM1
#endif
