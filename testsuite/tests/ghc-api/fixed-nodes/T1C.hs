module T1C where

import T1A
import T1B

-- Using dataA from T1A and dataB from T1B
dataC :: (Int, String, String)
dataC = (dataA, snd dataB, "C data")

-- Using funcA from T1A and funcB from T1B
funcC :: Int -> String
funcC n = funcA (funcB n)

-- Main export function
mainFunc :: IO ()
mainFunc = putStrLn $ "Values: " ++ show dataC ++ ", " ++ funcC 10