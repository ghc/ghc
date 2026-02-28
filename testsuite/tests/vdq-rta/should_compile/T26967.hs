{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE StarIsType #-}

module T26967 where

import Data.Proxy (type Proxy)
import Data.Kind (type Constraint)
import Data.Functor.Product (type Product)
import Data.Functor.Compose (type Compose)
import Data.Type.Equality (type (:~:))

checkKind :: forall k (t :: k) -> ()
checkKind _ _ = ()

r0, r1, r2, r3, r4, r5 :: ()
r0 = checkKind (type *) Int
r1 = checkKind (* -> *) Maybe
r2 = checkKind (* -> * -> *) Either
r3 = checkKind (forall k. k -> *) Proxy
r4 = checkKind ((* -> *) -> Constraint) Functor
r5 = checkKind (forall k. (k -> *) -> (k -> *) -> k -> *) Product
r6 = checkKind (forall k1 k2. (k1 -> *) -> (k2 -> k1) -> k2 -> *) Compose
r7 = checkKind (forall k. k -> k -> *) (:~:)

data S (x :: f a) (y :: f b)
data T (x :: f a) (y :: g a)

r8, r9 :: ()
r8  = checkKind (forall f a b. f a -> f b -> *) S
r9  = checkKind (forall f x g. f x -> g x -> *) T

data Q a (y :: Maybe a) = MkQ a

r10, r11 :: ()
r10 = checkKind (forall a -> Maybe a -> *) Q
r11 = checkKind (* -> Q (type *) Nothing) MkQ
