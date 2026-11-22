{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}

module GadtEmptyGroupsProvider where

data EmptyGroups where
  Trailing :: Bool -> forall. () => EmptyGroups
  Between :: Bool -> () => forall. Bool -> EmptyGroups
  Constrained :: forall a. (Eq a) => forall. a -> EmptyGroups
  Ordered :: forall a. a -> () => forall b -> forall. (Eq a) => b -> EmptyGroups
