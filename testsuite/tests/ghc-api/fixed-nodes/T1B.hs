module T1B where

import T1A

-- Using dataA from T1A
dataB :: (Int, String)
dataB = (dataA, "B data")

-- Using funcA from T1A
funcB :: Int -> String
funcB n = funcA ("B got " ++ show n)