{-# LANGUAGE ScopedTypeVariables, InstanceSigs #-}
module Foo where

data T a = T a

class C a where
  foo :: b -> a -> (a, [b])

instance C a => C (T a) where
  foo :: forall b. b -> T a -> (T a, [b])
  foo x (T y) = (T y, xs)
     where
       xs :: [b]
       xs = [x,x,x]

instance Functor T where
  fmap :: (a -> b) -> T a -> T b
  fmap f (T x) = T (f x)

