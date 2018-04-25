{-# LANGUAGE CPP, TypeFamilies, Rank2Types, FlexibleContexts, FlexibleInstances, GADTs, StandaloneDeriving, UndecidableInstances #-}

module T7354 where

type family Base t :: * -> *
data family Prim t :: * -> *

class Functor (Base t) => Unfoldable t where
  embed :: Base t t -> t
  ana
    :: (a -> Base t a) -- ^ a (Base t)-coalgebra
    -> a               -- ^ seed
    -> t               -- ^ resulting fixed point
  ana g = a where a = embed . fmap a . g


data instance Prim [a] b = Cons a b | Nil deriving (Eq,Ord,Show,Read)

coalg 0 = Nil
coalg n = Cons n (n-1)
alg Nil = 1
alg (Cons a b) = a * b

instance Functor (Prim [a]) where
  fmap f (Cons a b) = Cons a (f b)
  fmap _ Nil = Nil

foo = ana alg

bar = foo  -- With 7.6, the definition of foo is simply discarded by
           -- by the type checker, which makes Lint complain about bar

