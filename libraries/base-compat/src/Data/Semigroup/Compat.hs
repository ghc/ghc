{-# LANGUAGE CPP, NoImplicitPrelude #-}
-- | This backports the modern "Data.Semigroup" interface back to
-- @base-4.9@/GHC 8.0.
module Data.Semigroup.Compat (
#if MIN_VERSION_base(4,9,0)
    Semigroup(..)
  , stimesMonoid
  , stimesIdempotent
  , stimesIdempotentMonoid
  , mtimesDefault
  -- * Semigroups
  , Min(..)
  , Max(..)
  , First(..)
  , Last(..)
  , WrappedMonoid(..)
  -- * Re-exported monoids from Data.Monoid
  , Dual(..)
  , Endo(..)
  , All(..)
  , Any(..)
  , Sum(..)
  , Product(..)
  -- * A better monoid for Maybe
  , Option(..)
  , option
  -- * Difference lists of a semigroup
  , diff
  , cycle1
  -- * ArgMin, ArgMax
  , Arg(..)
  , ArgMin
  , ArgMax
#endif
  ) where

#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif
