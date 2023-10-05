{-# LANGUAGE RankNTypes, GADTs #-}

module Foo where

g :: (Int, Int) -> Int
{-# NOINLINE g #-}
g (p,q) = p+q

f :: Int -> Int -> Int -> Int
f x p q
  = g (let j y = (y+p,q)
           {-# NOINLINE j #-}
          in
          case x of
            2 -> j 3
            _ -> j 4)

