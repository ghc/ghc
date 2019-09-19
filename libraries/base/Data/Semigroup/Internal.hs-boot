{-# LANGUAGE NoImplicitPrelude #-}

module Data.Semigroup.Internal where

import {-# SOURCE #-} GHC.Real (Integral)
import {-# SOURCE #-} GHC.Base (Semigroup,Monoid,Maybe)
import GHC.Num.Integer () -- See Note [Depend on GHC.Num.Integer] in GHC.Base

stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a

stimesDefault :: (Integral b, Semigroup a) => b -> a -> a
stimesMaybe :: (Integral b, Semigroup a) => b -> Maybe a -> Maybe a
stimesList :: Integral b => b -> [a] -> [a]
