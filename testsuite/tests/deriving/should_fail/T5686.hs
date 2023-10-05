{-# LANGUAGE TypeFamilies, DeriveFunctor #-}
module T5686 where

data U a = U (G a) deriving Functor

class A a where
   type G a
