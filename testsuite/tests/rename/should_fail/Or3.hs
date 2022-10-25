{-# LANGUAGE OrPatterns #-}

module Or3 where

f x = case x of
  (Left a; Right (a,b)) -> a

g x = case x of
  (_; (x; _)) -> x

h x = case x of
  (Just @y 3; Nothing) -> 2
