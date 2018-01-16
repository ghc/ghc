
-- In 6.13 this stack overflowed

module Main (main) where

main :: IO ()
main = let n = 1000000
       in print $ integrate n (1 / fromIntegral n)

integrate :: Int -> Double -> Double
integrate n h = h * (sum (map area [1..n]))
    where area :: Int -> Double
          area i = let x = h * (fromIntegral i - 0.5)
                   in 4 / (1 + x*x)

