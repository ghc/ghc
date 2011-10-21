module T5533 where

data T a = CT {f1 :: Int -> a, f2 :: Double}
f2 :: Int -> Double
g x = CT {f1 = \t -> f2 t + x, f2 = x}

