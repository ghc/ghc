{-# LANGUAGE QuantifiedConstraints #-}

module T21037 where

type f ~> g = forall x. f x -> g x

class (forall f. Functor f => Functor (h f)) => HFunctor h where
  hmap :: (f ~> g) -> (h f ~> h g)

class (forall m. Monad m => Monad (h m), HFunctor h) => HMonad h where
  hreturn :: Monad m => m ~> h m
  hdnib :: (Monad m, Monad n) => (m ~> h n) -> h m ~> h n

bar :: (HMonad h, Monad f) => (Functor (h f) => c) -> c
bar x = x

baz ::
  ( forall m. Monad m => Monad (h m),
    forall f. Functor f => Functor (h f),
    Monad f
  ) =>
  (Functor (h f) => c) ->
  c
baz x = x
