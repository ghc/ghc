{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
module T17880 where

data T1 a = MkT1 (forall b. b -> (forall c. a -> c) -> a)
  deriving Functor

data T2 a = MkT2 (Int -> forall c. c -> a)
  deriving Functor
