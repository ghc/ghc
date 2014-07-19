{-# LANGUAGE DeriveFunctor #-}
module T9071_2 where

newtype Mu f = Mu (f (Mu f))

newtype K1 a b = K1 a
newtype F1 a = F1 (Mu (K1 a)) deriving Functor
