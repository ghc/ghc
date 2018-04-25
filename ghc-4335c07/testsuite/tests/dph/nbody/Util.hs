
module Util
        ( magV
        , mulSV
        , normaliseV)
where
        
-- | The magnitude of a vector.
magV :: (Double, Double) -> Double
magV (x, y)     = sqrt (x * x + y * y)
        
-- | Multiply a vector by a scalar.
mulSV :: Double -> (Double, Double) -> (Double, Double)
mulSV s (x, y)  = (s * x, s * y)
        
-- | Normalise a vector, so it has a magnitude of 1.
normaliseV :: (Double, Double) -> (Double, Double)
normaliseV v    = mulSV (1 / magV v) v
