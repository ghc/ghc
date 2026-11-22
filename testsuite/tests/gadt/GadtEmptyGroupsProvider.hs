{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}

module GadtEmptyGroupsProvider where

import Data.Proxy (Proxy)

data EmptyGroups where
  Trailing :: Bool -> forall. () => EmptyGroups
  Between :: Bool -> () => forall. Bool -> EmptyGroups
  Constrained :: forall a. (Eq a) => forall. a -> EmptyGroups
  Ordered :: forall a. a -> () => forall b -> forall. (Eq a) => b -> EmptyGroups
  Canonical :: forall a. () => forall c. (Eq a) => forall b -> a -> Proxy (b, c) -> EmptyGroups
