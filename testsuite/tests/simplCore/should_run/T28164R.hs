module Main (main) where

import System.Environment (getArgs)

data T = T ![Int] !Int !Int

{-# NOINLINE work #-}
work :: [Int] -> [Int] -> Int -> Int -> [Int]
work sh ss o v = [ x * o + v + y | x <- sh, y <- ss ]

{-# INLINE expensive #-}
expensive :: [Int] -> T -> [Int]
expensive sh (T ss o v) = work sh ss o v

{-# INLINE z #-}
z :: [Int] -> T -> T -> Int
z sh (T ss _ v) (T _ _ v')
  | v == 1, v' == 1 = sum ss
z sh t t' = sum (expensive sh t) + sum (expensive sh t')

{-# NOINLINE mkT #-}
mkT :: Int -> T
mkT n = T [1 .. n] 3 5

c :: T
c = mkT 1000

{-# NOINLINE f #-}
f :: T -> Int
f a = z [1 .. 100] a c

main :: IO ()
main = do
  [n] <- map read <$> getArgs
  print (sum [ f (T [k] 0 2) | k <- [1 .. n] ])
