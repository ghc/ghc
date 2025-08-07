{-# LANGUAGE NegativeLiterals #-}

module T26229 where

sqrte2pqiq :: (Floating a, Ord a) => a -> a -> a
sqrte2pqiq e qiq -- = sqrt (e*e + qiq)
  | e < - 1.5097698010472593e153 = -(qiq/e) - e
  | e < 5.582399551122541e57     = sqrt (e*e + qiq) -- test Infinity#
  | e < -5.582399551122541e57    = -sqrt (e*e + qiq) -- test -Infinity#
  | otherwise                    = (qiq/e) + e
{-# SPECIALIZE sqrte2pqiq :: Double -> Double -> Double #-}
{-# SPECIALIZE sqrte2pqiq :: Float -> Float -> Float #-}
