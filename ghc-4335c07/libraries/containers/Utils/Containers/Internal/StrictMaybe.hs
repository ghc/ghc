{-# LANGUAGE CPP #-}

#include "containers.h"

{-# OPTIONS_HADDOCK hide #-}
-- | Strict 'Maybe'

module Utils.Containers.Internal.StrictMaybe (MaybeS (..), maybeS, toMaybe, toMaybeS) where

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable (..))
import Data.Monoid (Monoid (..))
#endif

data MaybeS a = NothingS | JustS !a

instance Foldable MaybeS where
  foldMap _ NothingS = mempty
  foldMap f (JustS a) = f a

maybeS :: r -> (a -> r) -> MaybeS a -> r
maybeS n _ NothingS = n
maybeS _ j (JustS a) = j a

toMaybe :: MaybeS a -> Maybe a
toMaybe NothingS = Nothing
toMaybe (JustS a) = Just a

toMaybeS :: Maybe a -> MaybeS a
toMaybeS Nothing = NothingS
toMaybeS (Just a) = JustS a
