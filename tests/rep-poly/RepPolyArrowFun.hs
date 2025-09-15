{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepPolyArrowFun where

import Data.Kind
import GHC.Exts
import Prelude ( undefined )

arr :: forall r (arr :: Type -> Type -> TYPE r) a b. (a -> b) -> arr a b
arr = undefined

(.) :: forall r (arr :: Type -> Type -> TYPE r) a b c. arr b c -> arr a b -> arr a c
(.) = undefined

(>>>) :: forall r (arr :: Type -> Type -> TYPE r) a b c. arr a b -> arr b c -> arr a c
(>>>) = undefined

first :: forall r (arr :: Type -> Type -> TYPE r) a b c d. arr a b -> arr c d
first = undefined

returnA :: forall r (arr :: Type -> Type -> TYPE r) a. ( () ~ () ) => arr a a
returnA = undefined

foo :: forall r (arr :: Type -> Type -> TYPE r) a. () -> arr a a
foo _ = proc x -> undefined -< x
