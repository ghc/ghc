
-- Test for trac #921

import GHC.Float
import Foreign
import Control.Monad

main :: IO ()
main = do -- The reported case
          putStrLn (show (map log2 vals))
          -- Smaller failing cases
          print (fromIntegral ((2^31) :: Int) :: Double)
          if_not_32 $ print (round ((2^33) :: Double) :: Int)
          print (fromIntegral ((2^31) :: Int) :: Float)
          if_not_32 $ print (round ((2^33) :: Float) :: Int)
          -- The underlying failing internal operations
          print (int2Double (2^31))
          if_not_32 $ print (double2Int (2^33))
          print (int2Float (2^31))
          if_not_32 $ print (float2Int (2^33))
  where
    -- the value of float2Int x where the result would be outside the
    -- range of the target is undefined.  We also take the view in GHC
    -- that round and truncate are similarly undefined when the result
    -- would be outside the range of the target type (see #1254)
    if_not_32 = when (sizeOf (undefined::Int) > 4)

log2 x = ceiling log_x
    where log_x :: Double
          log_x = logBase 2 (fromIntegral (max 1 x))

vals = [1, 2, 17, 259, 1000, 10000,
        2^30 + 9000, 2^31 - 1, 2^31 + 1,
        2^32 - 1, 2^32 + 1]

