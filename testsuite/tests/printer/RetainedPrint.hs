{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds, ExplicitForAll, GADTs, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments, TypeAbstractions #-}

module RetainedPrint where

import GHC.Exts (M, U)

keep :: foreach -- Retain this Boolean argument.
    (b :: Bool)
    -> Select b -> Select b
keep b x = x

nested :: (foreach (b :: Bool) -> Select b) -> Bool
nested x = True

identity :: forall a. {- Before metadata. -} @U a -> a
identity x = x

ignoreMatchable :: (forall a. @M a -> a) -> ()
ignoreMatchable _ = ()

visible :: forall a -> @U a -> a
visible (type a) x = x

data Ordered where
    Ordered :: forall a. @U {- Keep this comment after metadata. -}
               forall b. @U -- Keep the first inner group.
               forall c. @U -- Keep the second inner group.
               (Show a, Eq b) => a -> b -> c -> Ordered
    Parenthesized :: forall a. @U
                     (Show a =>
                      forall b. @U
                      (Eq b => a -> b -> Ordered))
