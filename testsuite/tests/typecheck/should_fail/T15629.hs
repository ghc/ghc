{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module Bug (f) where

import Data.Kind
import Data.Proxy

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

type family F x :: Type -> Type
data F1Sym :: forall x a. x ~> F x a
data F2Sym :: forall x a. F x a ~> x
data Comp :: forall b c a. (b ~> c) -> (a ~> b) -> (a ~> c)

sg :: forall a b c (f :: b ~> c) (g :: a ~> b) (x :: a).
      Proxy f -> Proxy g -> Proxy (Comp f g)
sg _ _ = Proxy

f :: forall (x :: Type). Proxy x -> ()
f _ = ()
  where
    g :: forall z ab. Proxy ((Comp (F1Sym :: x ~> F x z) F2Sym) :: F x ab ~> F x ab)
    g = sg Proxy Proxy
