module Main where

import Control.Monad
import System.Random

-- splitN :: (RandomGen g) => Int -> g -> ([g], g)
splitN 0 g = ([], g)
splitN n g = (g1:l, g')
  where
  (l, g') = splitN (n-1) g2
  (g1, g2) = split g

-- The funny splitting operation.
split' :: (RandomGen g) => g -> (g, g)
split' g = (g12, g21)
  where
  (g1, g2) = split g
  (_, g12) = split g1
  (g21, _) = split g2

-- This test checks if generators created by calling split 2 times are independent.
-- It generates pairs of integers from 0 to n-1, using split' to
-- generate both numbers using one seed. Then it counts how often the
-- two numbers are equal.
test :: (RandomGen g) => Int -> Int -> g -> Int
test numTests n g = equals
  where
  (gs, _) = splitN numTests g
  equals = count id $ map single gs
  count p l = length $ filter p l
  single g' = (fst $ randomR (0, n-1) g1) == (fst $ randomR (0, n-1) g2)
    where
    (g1, g2) = split' g'

main = do
  let g = mkStdGen 42
  forM_ [2..15] $ \i -> do
    let actual = test (i * 1000) i g
    putStrLn $ "Generated " ++ show (i * 1000)
      ++ " pairs of numbers from 0 to " ++ show (i - 1)
      ++ " -- " ++ show actual ++ " pairs contained equal numbers "
      ++ "and we expected about 1000."
