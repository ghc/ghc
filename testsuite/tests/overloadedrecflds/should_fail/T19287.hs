{-# LANGUAGE DuplicateRecordFields #-}
module Main where

data R a b = R { x :: a , x :: b }

unsafeCoerce :: a -> b
unsafeCoerce i = case (R i i){x = i} of
  R a b -> b

main = do
  print (unsafeCoerce (1 :: Int) :: Bool)
