{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
module T15170  where

import Data.Kind
import Data.Proxy

data TyFun :: Type -> Type -> Type
type a ~> b = TyFun a b -> Type
infixr 0 ~>

type family Apply (f :: k1 ~> k2) (x :: k1) :: k2
type f @@ x = Apply f x
infixl 9 @@

wat :: forall (a :: Type)
              (b :: a ~> Type)
              (c :: forall (x :: a). Proxy x ~> b @@ x ~> Type)
              (f :: forall (x :: a) (y :: b @@ x). Proxy x ~> Proxy y ~> c @@ ('Proxy :: Proxy x) @@ y)
              (x :: a).
      (forall (xx :: a) (yy :: b @@ xx). Proxy (f @@ ('Proxy :: Proxy xx) @@ ('Proxy :: Proxy yy)))
   -> ()
wat _ = ()
