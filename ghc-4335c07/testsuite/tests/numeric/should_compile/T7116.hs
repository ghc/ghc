module T7116 where

-- this module tests strength reduction, i.e. turning floating
-- point multiplication by two into addition:
--
-- 2.0 * x -> x + x

dl :: Double -> Double
dl x = 2.0 * x

dr :: Double -> Double
dr x = x * 2.0

fl :: Float -> Float
fl x = 2.0 * x

fr :: Float -> Float
fr x = x * 2.0

