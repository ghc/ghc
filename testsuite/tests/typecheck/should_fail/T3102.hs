{-# OPTIONS -XImplicitParams -XRankNTypes #-}
module Bug where

t :: forall a. ((?p :: Int) => a) -> String
t _ = "Hello"

f :: (forall a. a -> String) -> Int
f _ = 3

result :: Int
result = f t


-- This should work.
-- Elaborated result = f (/\a. \x:a. t @a (\p::Int. x))
-- But it did not work in 8.0.1; fixed in HEAD
