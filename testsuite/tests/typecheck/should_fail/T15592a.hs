{-# LANGUAGE PolyKinds, TypeApplications, DataKinds, RankNTypes #-}

module T15592 where
import Data.Proxy

data VisProxy k (a :: k) = MkVP
class D (a :: Proxy j) (b :: Proxy k) c where
  meth1 :: forall z. D @j @k a b z => z -> Proxy '(a, b)
  meth2 :: Proxy k j -> Proxy '(a, b, c)
