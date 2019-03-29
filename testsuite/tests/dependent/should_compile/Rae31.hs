{-# LANGUAGE TemplateHaskell, TypeOperators, PolyKinds, DataKinds,
             TypeFamilies, RankNTypes #-}

module A where

import Data.Kind

data family Sing (k :: Type) :: k -> Type
type Sing' (x :: k) = Sing k x
data TyFun' (a :: Type) (b :: Type) :: Type
type TyFun (a :: Type) (b :: Type) = TyFun' a b -> Type
type family (a :: TyFun k1 k2) @@ (b :: k1) :: k2
data TyPi' (a :: Type) (b :: TyFun a Type) :: Type
type TyPi (a :: Type) (b :: TyFun a Type) = TyPi' a b -> Type
type family (a :: TyPi k1 k2) @@@ (b :: k1) :: k2 @@ b
$(return [])

data A (a :: Type) (b :: a) (c :: TyFun' a Type)
  -- A :: forall a -> a -> a ~> Type
type instance (@@) (A a b) c = Type
$(return [])
data B (a :: Type) (b :: TyFun' a Type)
  -- B :: forall a -> a ~> Type
type instance (@@) (B a) b = TyPi a (A a b)
$(return [])
data C (a :: Type) (b :: TyPi a (B a)) (c :: a) (d :: a)
       (e :: TyFun' (b @@@ c @@@ d) Type)
