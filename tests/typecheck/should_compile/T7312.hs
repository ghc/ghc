module T7312 where

-- this works
mac :: Double -> (Double->Double) -> (Double-> Double)
mac ac m = \ x -> ac + x * m x

-- this doesn't
mac2 :: Double -> (->) Double Double -> (->) Double Double
mac2 ac m = \ x -> ac + x * m x
