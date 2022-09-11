module T22167 where

import GHC.Generics (Generic1)

data T1 f a = MkT1 (f a)
  deriving (Functor, Foldable, Traversable)

data T2 f a where
  MkT2 :: f a -> T2 f a
  deriving (Functor, Foldable, Traversable)

-- A slightly more complicated example from the `syntactic` library
data (sym1 :+: sym2) sig
  where
    InjL :: sym1 a -> (sym1 :+: sym2) a
    InjR :: sym2 a -> (sym1 :+: sym2) a
  deriving (Functor, Foldable, Traversable)

-- Test Generic1 instances with inferred Functor constraints
data G1 f g a = MkG1 (f (g a)) deriving Generic1

data G2 f g a where
  MkG2 :: f (g a) -> G2 f g a
  deriving Generic1
