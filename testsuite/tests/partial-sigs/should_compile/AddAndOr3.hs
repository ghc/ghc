{-# LANGUAGE PartialTypeSignatures #-}
module AddAndOr3 where

addAndOr3 :: _ -> _ -> _
addAndOr3 (a, b) (c, d) = (a `plus` d, b || c)
  where plus :: Int -> Int -> Int
        x `plus` y = x + y
