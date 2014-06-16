{-# LANGUAGE ExistentialQuantification #-}
module Main where

import T7797a 

data Box = forall a. (Size a) => Box a a 

box = Box (go 10000000) (go 10000000) where
  go :: Int -> [Int]
  go 0 = []
  go n = 1 : go (n - 1)
{-# NOINLINE box #-}

main = print $ case box of
  Box l r -> size l r
