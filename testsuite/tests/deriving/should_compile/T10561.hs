{-# LANGUAGE PolyKinds, DeriveFunctor, RankNTypes #-}

module T10561 where

newtype Compose f g a = Compose (f (g a)) deriving Functor
