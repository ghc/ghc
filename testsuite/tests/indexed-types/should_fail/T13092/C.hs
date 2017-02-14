{-# LANGUAGE RankNTypes #-}
module C (x) where
import Data.Proxy
import B
x :: Proxy b -> (forall t. Proxy t -> Bool -> A (t, b)) -> (Bool -> ())
x _ f = f (undefined :: Proxy X)
