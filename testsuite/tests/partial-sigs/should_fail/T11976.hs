{-# LANGUAGE PartialTypeSignatures, RankNTypes #-}

module T11976 where

type Lens s a = forall f. Functor f => (a -> f a) -> (s -> f s)

foo = undefined :: Lens _ _ _
