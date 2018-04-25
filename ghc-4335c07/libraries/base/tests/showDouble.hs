
module Main (main) where

main :: IO ()
main = do let xs = [p0, p01, p3, p31, n0, n01, n3, n31, pinf, ninf, nan]
          mapM_ print xs
          mapM_ (print . Just) xs

p0 :: Double
p0 = 0

p01 :: Double
p01 = 0.1

p3 :: Double
p3 = 3

p31 :: Double
p31 = 3.1

n0 :: Double
n0 = -0

n01 :: Double
n01 = -0.1

n3 :: Double
n3 = -3

n31 :: Double
n31 = -3.1

pinf :: Double
pinf = 1 / 0

ninf :: Double
ninf = - 1 / 0

nan :: Double
nan = 0 / 0

