
-- Test for trac #921

import GHC.Float

main :: IO ()
main = do -- The reported case
          putStrLn (show (map log2 vals))
          -- Smaller failing cases
          print (fromIntegral ((2^31) :: Int) :: Double)
          print (round ((2^33) :: Double) :: Int)
          print (fromIntegral ((2^31) :: Int) :: Float)
          print (round ((2^33) :: Float) :: Int)
          -- The underlying failing internal operations
          print (int2Double (2^31))
          print (double2Int (2^33))
          print (int2Float (2^31))
          print (float2Int (2^33))

log2 x = ceiling log_x
    where log_x :: Double
          log_x = logBase 2 (fromIntegral (max 1 x))

vals = [1, 2, 17, 259, 1000, 10000,
        2^30 + 9000, 2^31 - 1, 2^31, 2^31 + 1,
        2^32 - 1, 2^32, 2^32 + 1]

