{-# LANGUAGE PartialTypeSignatures #-}
module AddAndOr1 where

addAndOr1 :: _
addAndOr1 (a, b) (c, d) = (a `plus` d, b || c)
  where plus :: Int -> Int -> Int
        x `plus` y = x + y
