{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Internal.Data.NonEmpty
  ( NonEmpty (..)
  , map
  ) where

import GHC.Internal.Base
         ( Applicative (..), Functor (..), Monad (..), NonEmpty (..)
         , Semigroup (..), (++), (.), ap, liftM2
         )

-- The following were moved here from module Data.List.NonEmpty of the base
-- package: map.

-- | Map a function over a 'NonEmpty' stream.
map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f (a :| as) = f a :| fmap f as

-- The following orphan instances were moved here from module GHC.Internal.Base:
-- Semigroup, Functor, Applicative and Monad.

-- | @since base-4.9.0.0
instance Semigroup (NonEmpty a) where
  (a :| as) <> bs = a :| (as ++ toList bs)
   where
    toList (c :| cs) = c : cs

-- | @since base-4.9.0.0
instance Functor NonEmpty where
  fmap = map
  b <$ (_ :| as) = b :| (b <$ as)

-- | @since base-4.9.0.0
instance Applicative NonEmpty where
  pure a = a :| []
  (<*>) = ap
  liftA2 = liftM2

-- | @since base-4.9.0.0
instance Monad NonEmpty where
  (a :| as) >>= f =
    case f a of
      b :| bs -> b :| (bs ++ bs')
    where
     bs' = as >>= toList . f
     toList (c :| cs) = c : cs
