{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module T23162d where

import GHC.TypeNats
import Data.Kind

data T2 a b = MkT2 a b

type TArgKind :: Nat -> Type
type family TArgKind n where
   TArgKind 2 = T2 Type Type

eq :: a -> a -> ()
eq x y = ()

bar :: (c->()) -> ()
bar =  bar

foo :: forall n k0 k1. (TArgKind n ~ T2 k0 k1) => Int
foo = foo @n

f :: () -> Int
f () = foo
