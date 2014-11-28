{-# LANGUAGE PartialTypeSignatures #-}
module AddAndOr4 where

addAndOr4 :: (_ _ _) -> (_ _ _) -> (_ _ _)
addAndOr4 (a, b) (c, d) = (a `plus` d, b || c)
  where plus :: Int -> Int -> Int
        x `plus` y = x + y
