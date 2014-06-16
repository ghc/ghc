{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

module T2448 where

-- Demonstrates a bug in propagating type equality constraints

class VectorSpace v where
  type Scalar v :: *

class VectorSpace v => InnerSpace v

instance (VectorSpace u,VectorSpace v, Scalar u ~ Scalar v) => 
  VectorSpace (u,v) 
  where
  type Scalar (u,v) = Scalar u

instance (InnerSpace u,InnerSpace v, Scalar u ~ Scalar v) => InnerSpace (u,v)
