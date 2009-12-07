{-# OPTIONS -XImplicitParams -XRankNTypes #-}
 module Bug where

t :: forall a. ((?p :: Int) => a) -> String
t _ = "Hello"

f :: (forall a. a -> String) -> Int
f _ = 3

result :: Int
result = f t

