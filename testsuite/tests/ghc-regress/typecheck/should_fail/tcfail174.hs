{-# OPTIONS -fglasgow-exts #-}

module Foo where

data Capture a = Base a
               | Capture (Capture (forall x . x -> a))

g :: Capture (forall a . a ->  a)
g = Base id

-- This function should definitely be rejected, with or without type signature

h1 = Capture g

h2 :: Capture b
h2 = Capture g

