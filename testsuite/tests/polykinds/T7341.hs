{-# LANGUAGE KindSignatures, TypeFamilies, PolyKinds #-}

module T7341 where

data Proxy a = Proxy

class C a where
  type F (a :: *) :: *
  op :: Proxy a -> Int

instance C [] where
  op _ = 5
