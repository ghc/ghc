{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Trustworthy   #-}
-- | Zipping and unzipping of functors with non-uniform shapes.
--
module Data.Zip (
    Semialign (..),
    Zip (..),
    Repeat (..),
    Unzip (..),
    unzipDefault,
    Zippy (..),
    ) where

import Control.Applicative (Applicative (..))
import Data.Monoid         (Monoid (..))
import Data.Semigroup      (Semigroup (..))
import Prelude             (Eq, Functor (..), Ord, Read, Show, ($), (.))

import Data.Semialign.Internal

#ifdef MIN_VERSION_semigroupoids
import Data.Functor.Apply (Apply (..))
#endif

-------------------------------------------------------------------------------
-- Zippy
-------------------------------------------------------------------------------

newtype Zippy f a = Zippy { getZippy :: f a }
  deriving (Eq, Ord, Show, Read, Functor)

instance (Zip f, Semigroup a) => Semigroup (Zippy f a) where
    Zippy x <> Zippy y = Zippy $ zipWith (<>) x y

instance (Repeat f, Monoid a) => Monoid (Zippy f a) where
    mempty                      = Zippy $ repeat mempty
    mappend = (<>)

#ifdef MIN_VERSION_semigroupoids
instance Zip f => Apply (Zippy f) where
    Zippy f <.> Zippy x = Zippy $ zipWith ($) f x
#endif

instance Repeat f => Applicative (Zippy f) where
    pure  = Zippy . repeat
#ifdef MIN_VERSION_semigroupoids
    (<*>) = (<.>)
#else
    Zippy f <*> Zippy x = Zippy $ zipWith ($) f x
#endif

    liftA2 f (Zippy x) (Zippy y) = Zippy $ zipWith f x y
