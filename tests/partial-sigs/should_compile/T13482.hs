{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

module T12382 where

import Data.Kind (Type)

minimal1_noksig :: forall m. ( _ ) => Int -> Bool
minimal1_noksig _ = (mempty :: m) == (mempty :: m)

minimal1 :: forall (m :: Type). _ => Bool
minimal1 = (mempty :: m) == (mempty :: m)

minimal2 :: forall m. (Eq m, _) => Bool
minimal2 = (mempty :: m) == (mempty :: m)

minimal3 :: forall m. (Monoid m, _) => Bool
minimal3 = (mempty :: m) == (mempty :: m)

minimal4 :: forall m. (Monoid m, Eq m) => Bool
minimal4 = (mempty :: m) == (mempty :: m)

