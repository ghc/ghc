{-# LANGUAGE NoImplicitPrelude #-}

module Data.Semigroup.Internal where

import {-# SOURCE #-} GHC.Real (Integral)
import {-# SOURCE #-} GHC.Base (NonEmpty, Monoid, Maybe)
import GHC.Integer () -- See Note [Depend on GHC.Integer] in GHC.Base

stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a

stimesDefault :: (Integral b, Semigroup a) => b -> a -> a
stimesMaybe :: (Integral b, Semigroup a) => b -> Maybe a -> Maybe a
stimesList :: Integral b => b -> [a] -> [a]

class Semigroup a where

        (<>) :: a -> a -> a

        sconcat :: NonEmpty a -> a
        sconcat = sconcatDefault

        stimes :: Integral b => b -> a -> a
        stimes = stimesDefault

instance Semigroup [a]
