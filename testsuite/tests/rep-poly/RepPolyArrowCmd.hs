{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepPolyArrowCmd where

import Data.Kind
import GHC.Exts
import Prelude ( undefined )

first :: forall r (a :: TYPE r) (b :: TYPE r). a -> b
first = undefined

(>>>) :: forall r (a :: TYPE r) (b :: TYPE r) (c :: TYPE r). a -> b -> c
(>>>) = undefined

arr :: forall r (a :: TYPE r) (b :: TYPE r). (a -> b) -> (a -> b)
arr = undefined

returnA :: forall r (a :: TYPE r). a -> a
returnA = undefined

foo :: forall r (a :: TYPE r). a -> a
foo = proc x -> returnA -< x
