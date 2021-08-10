module Main (main) where

import Data.List (unfoldr, foldl')
import System.Random.SplitMix32

doubles :: SMGen -> [Float]
doubles = unfoldr (Just . nextFloat)

monteCarloPi :: SMGen -> Float
monteCarloPi = (4 *) . calc . foldl' accum (P 0 0) . take 50000000 . pairs . doubles
  where
    calc (P n m) = fromIntegral n / fromIntegral m

    pairs (x : y : xs) = (x, y) : pairs xs
    pairs _ = []

    accum (P n m) (x, y) | x * x + y * y >= 1 = P n (m + 1)
                         | otherwise          = P (n + 1) (m + 1)

data P = P !Int !Int

main :: IO ()
main = do
    pi' <- fmap monteCarloPi newSMGen
    print (pi :: Float)
    print pi'
    print (pi - pi')
