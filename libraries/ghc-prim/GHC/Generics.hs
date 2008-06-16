
{-# OPTIONS_GHC -XNoImplicitPrelude #-}

module GHC.Generics where

default ()

data Unit = Unit
#ifndef __HADDOCK__
data (:+:) a b = Inl a | Inr b
data (:*:) a b = a :*: b
#endif

