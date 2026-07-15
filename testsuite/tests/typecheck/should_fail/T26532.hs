{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE TypeFamilies #-}

module T26532 where

import Data.Kind
import Type.Reflection

test1 :: TypeRep (forall a. a -> a)
test1 = typeRep

test2 :: TypeRep (Eq Int => Int)
test2 = typeRep

test3 :: TypeRep (((),()) => Int)
test3 = typeRep

test4 :: TypeRep (# Int | Bool #)
test4 = typeRep

type F :: Type -> Type
type family F a where {}

test5 :: TypeRep (F Int)
test5 = typeRep

type PolyDF :: forall k. Type -> k
data family PolyDF a

type ProxyPolyDF :: (forall k. Type -> k) -> Type
data ProxyPolyDF f = MkProxy

test6 :: TypeRep (ProxyPolyDF PolyDF)
test6 = typeRep
