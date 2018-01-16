{-# LANGUAGE TemplateHaskell, TypeOperators, PolyKinds, DataKinds,
             TypeFamilies, TypeInType #-}

module A where

import Data.Kind

data family Sing (k :: *) :: k -> *
type Sing' (x :: k) = Sing k x
data TyFun' (a :: *) (b :: *) :: *
type TyFun (a :: *) (b :: *) = TyFun' a b -> *
type family (a :: TyFun k1 k2) @@ (b :: k1) :: k2
data TyPi' (a :: *) (b :: TyFun a *) :: *
type TyPi (a :: *) (b :: TyFun a *) = TyPi' a b -> *
type family (a :: TyPi k1 k2) @@@ (b :: k1) :: k2 @@ b
$(return [])

data A (a :: *) (b :: a) (c :: TyFun' a *) -- A :: forall a -> a -> a ~> *
type instance (@@) (A a b) c = *
$(return [])
data B (a :: *) (b :: TyFun' a *) -- B :: forall a -> a ~> *
type instance (@@) (B a) b = TyPi a (A a b)
$(return [])
data C (a :: *) (b :: TyPi a (B a)) (c :: a) (d :: a) (e :: TyFun' (b @@@ c @@@ d) *)
