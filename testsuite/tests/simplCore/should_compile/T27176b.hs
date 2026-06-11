-- Exercises -dcanonicalize-local-binds on the desugarer output (-ddump-ds).
module T27176b (f) where

f :: Int -> Int
f x = g (g x)
  where
    g y = y + x
