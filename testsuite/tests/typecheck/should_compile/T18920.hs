{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
module T18920 where

import Data.Kind

class Monad solver => Solver solver where
  type Constraint solver  :: Type
  type Label solver       :: Type

class Queue q

data Tree s a where
  NewVar :: Term s t => (t -> Tree s a) -> Tree s a

class Solver solver => Term solver term

class Transformer t where
  type EvalState t :: Type
  type TreeState t :: Type
  type ForSolver t :: (Type -> Type)
  type ForResult t :: Type
  nextT :: SearchSig (ForSolver t) q t (ForResult t)
  returnT :: ContinueSig solver q t (ForResult t)

type ContinueSig solver  q t a =
  ( Solver solver, Queue q, Transformer t  )
  => Int -> q -> t -> EvalState t
  -> solver (Int, [a])

type SearchSig solver q t a =
     (Solver solver, Queue q, Transformer t     )
     => Int -> Tree solver a -> q -> t -> EvalState t -> TreeState t
     -> solver (Int,[a])
