{-# LANGUAGE NoImplicitPrelude #-}

module Data.Semigroup.Internal where

import {-# SOURCE #-} GHC.Real (Integral)
import {-# SOURCE #-} GHC.Base (Semigroup,Monoid,Maybe)

stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a

stimesDefault :: (Integral b, Semigroup a) => b -> a -> a
stimesMaybe :: (Integral b, Semigroup a) => b -> Maybe a -> Maybe a
stimesList :: Integral b => b -> [a] -> [a]
