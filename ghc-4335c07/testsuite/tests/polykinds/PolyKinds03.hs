{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PolyKinds                  #-}

module PolyKinds03 where

data Proxy t
data TypeRep = TypeRep

class MyTypeable t where
  myTypeOf :: Proxy t -> TypeRep

instance MyTypeable Int  where myTypeOf _ = TypeRep
instance MyTypeable []   where myTypeOf _ = TypeRep
