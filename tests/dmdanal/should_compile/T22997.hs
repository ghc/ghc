module T22997 where

{-# OPAQUE trivial #-}
trivial :: Int -> Int
trivial = succ

{-# OPAQUE pap #-}
pap :: Integer -> Integer
pap = (42 +)
