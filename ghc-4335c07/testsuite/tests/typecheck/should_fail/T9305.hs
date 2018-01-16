{-# LANGUAGE DeriveFunctor#-}
module Main where

data Event a b = Event a deriving (Functor)

newtype F f = F (f (F f))

data EventF a = EventF (F (Event a)) deriving (Functor)
