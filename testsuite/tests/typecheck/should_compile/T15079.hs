{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T15079 where

import           Data.Kind
import qualified Data.Type.Equality as Eq
import           Data.Void
import           GHC.Exts (Any)

infixl 4 :==
-- | Heterogeneous Leibnizian equality.
newtype (a :: j) :== (b :: k)
  = HRefl { hsubst :: forall (c :: forall (i :: Type). i -> Type). c a -> c b }

-----

newtype Coerce a = Coerce { uncoerce :: Starify a }
type family Starify (a :: k) :: Type where
  Starify (a :: Type) = a
  Starify _           = Void

coerce :: a :== b -> a -> b
coerce f = uncoerce . hsubst f . Coerce

-----

newtype Flay :: (forall (i :: Type). i -> i -> Type)
             -> forall (j :: Type). j -> forall (k :: Type). k -> Type where
  Flay :: forall (p :: forall (i :: Type). i -> i -> Type)
                 (j :: Type) (k :: Type) (a :: j) (b :: k).
          { unflay :: p a (MassageKind j b) } -> Flay p a b

type family MassageKind (j :: Type) (a :: k) :: j where
  MassageKind j (a :: j) = a
  MassageKind _ _        = Any

fromLeibniz :: forall a b. a :== b -> a Eq.:~: b
fromLeibniz f = unflay $ hsubst f $ Flay Eq.Refl

-----

newtype Foo (f :: forall (a :: Type). a -> Type) = MkFoo (f Int)
data InferredProxy a = MkInferredProxy

foo :: Foo InferredProxy
foo = MkFoo MkInferredProxy

-----

id1 :: forall a. a -> a
id1 x = x

id2 :: forall {a}. a -> a
id2 x = x

app1 :: (forall a. a -> a) -> b -> b
app1 g x = g x

app2 :: (forall {a}. a -> a) -> b -> b
app2 g x = g x
