{-# LANGUAGE CPP #-}

{-|
Module:      Data.Functor.Classes.Generic
Copyright:   (C) 2015-2016 Edward Kmett, Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Functions to generically derive 'C.Eq1', 'C.Ord1', 'C.Read1', and 'C.Show1'
instances from "Data.Functor.Classes".
-}
module Data.Functor.Classes.Generic
  ( -- * Options
    Options(..)
  , defaultOptions
  , latestGHCOptions
#if defined(TRANSFORMERS_FOUR)
    -- * 'Eq1'
  , eq1Default
  , eq1Options
    -- * 'Ord1'
  , compare1Default
  , compare1Options
    -- * 'Read1'
  , readsPrec1Default
  , readsPrec1Options
    -- * 'Show1'
  , showsPrec1Default
  , showsPrec1Options
#else
    -- * 'Eq1'
  , liftEqDefault
  , liftEqOptions
    -- * 'Ord1'
  , liftCompareDefault
  , liftCompareOptions
    -- * 'Read1'
  , liftReadsPrecDefault
  , liftReadsPrecOptions
    -- * 'Show1'
  , liftShowsPrecDefault
  , liftShowsPrecOptions
#endif
    -- * 'GenericFunctorClasses'
  , FunctorClassesDefault(..)
    -- * Example
    -- $example
  ) where

import qualified Data.Functor.Classes as C ()
import           Data.Functor.Classes.Generic.Internal

#undef MIN_VERSION_transformers
{- $example
The most straightforward way to use the defaults in this module is to use
@DerivingVia@ on GHC 8.6 or later. For example:

@
&#123;-&#35; LANGUAGE DeriveGeneric, DerivingVia &#35;-&#125;

import Data.Functor.Classes
import Data.Functor.Classes.Generic
import GHC.Generics

data Pair a = Pair a a
  deriving stock Generic1
  deriving (Eq1, Ord1, Read1, Show1)
           via FunctorClassesDefault Pair
@

If using an older version of GHC, then one can also define instances manually.
This is slightly trickier to accomplish since this module exports different
functions depending on which version of @transformers@ this library is built
against. Here is an example of how to define instances manually:

@
&#123;-&#35; LANGUAGE CPP, DeriveGeneric &#35;-&#125;

import Data.Functor.Classes
import Data.Functor.Classes.Generic
import GHC.Generics

data Pair a = Pair a a deriving Generic1

instance 'C.Eq1' Pair where
\#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
    'C.eq1' = 'eq1Default'
\#else
    'C.liftEq' = 'liftEqDefault'
\#endif

instance 'C.Ord1' Pair where
\#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
    'C.compare1' = 'compare1Default'
\#else
    'C.liftCompare' = 'liftCompareDefault'
\#endif

instance 'C.Read1' Pair where
\#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
    'C.readsPrec1' = 'readsPrec1Default'
\#else
    'C.liftReadsPrec' = 'liftReadsPrecDefault'
\#endif

instance 'C.Show1' Pair where
\#if MIN_VERSION_transformers(0,4,0) && !(MIN_VERSION_transformers(0,5,0))
    'C.showsPrec1' = 'showsPrec1Default'
\#else
    'C.liftShowsPrec' = 'liftShowsPrecDefault'
\#endif
@
-}
