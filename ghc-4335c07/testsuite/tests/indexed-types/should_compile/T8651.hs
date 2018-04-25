{-# LANGUAGE RankNTypes, FlexibleContexts, TypeFamilies #-}
module T8651 where

import Data.Monoid

type family Id a

type instance Id a = a
 --type instance Id [a] = [Id a]

foo :: (Monoid (Id String) => r) -> r
foo x = x

bar = foo "Hello"
