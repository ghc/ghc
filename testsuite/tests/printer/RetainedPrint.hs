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

invisible :: foreach (b :: Bool) -- Preserve this comment before the dot.
    . Select b -> Select b
invisible x = x

explicit = invisible @{ {- Preserve this comment inside the braces. -}
    True} 42

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

type Wrap = \a {- Before the arrow. -} -> Maybe a

type Row e = let wrap = \a -> Either e a in wrap Bool

type PairWith = (\a -> \b -> (a, b))
