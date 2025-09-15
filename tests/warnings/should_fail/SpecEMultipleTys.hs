{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module SpecEMultipleTys where

bar :: Num a => a -> a -> a
bar x y = 2 * ( x + y )

-- The "specialise expression" syntax doesn't support multiple type ascriptions.
{-# SPECIALISE bar 3 :: Float -> Float, Double -> Double #-}
