{-# OPTIONS -XImpredicativeTypes #-}

module Foo where

data Capture a = Base a
               | Capture (Capture (forall x . x -> a))

g :: Capture (forall a . a ->  a)
g = Base id  -- Fails; need a rigid signature on 'id'
    -- Actually, succeeds now, with visible type application
    -- Disagree: should not succeed because it instantiates
    --           Base with a forall type

-- This function should definitely be rejected, with or without type signature

h1 = Capture g

h2 :: Capture b
h2 = Capture g
