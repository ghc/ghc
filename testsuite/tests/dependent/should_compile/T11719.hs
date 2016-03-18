{-# LANGUAGE RankNTypes, TypeFamilies, TypeInType, TypeOperators,
             UndecidableInstances #-}

module T11719 where

import Data.Kind

data TyFun :: * -> * -> *
type a ~> b = TyFun a b -> *

type family (f :: a ~> b) @@ (x :: a) :: b

data Null a = Nullable a | NotNullable a

type family ((f :: b ~> c) ∘ (g :: a ~> b)) (x :: a) :: c where
  (f ∘ g) x = f @@ (g @@ x)

type family BaseType (k :: forall a. a ~> Type) (x :: b) :: Type where   -- this fails :(
