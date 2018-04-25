{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}

module PolyKinds13 where


data Proxy a = Proxy

instance Show (Proxy a) where
  show _ = "Proxy"

instance Functor Proxy where
  fmap _ Proxy = Proxy


data TypeRep = TypeRep

class MyTypeable t where
-- MyTypeable :: forall k. k -> Constraint
  myTypeOf :: Proxy t -> TypeRep
  myTypeOf _ = TypeRep

data Apply f t = Apply (f t)
-- Apply :: forall k.  (k -> *) -> k -> * 

instance MyTypeable Apply
-- df :: forall k. MyTypeable ((k -> *) -> k -> *) (Apply k)
instance MyTypeable Int
instance MyTypeable Maybe
