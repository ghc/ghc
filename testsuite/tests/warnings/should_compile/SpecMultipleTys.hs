{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module SpecMultipleTys where

-- NB: this program should be rejected starting from GHC 9.18.
-- See GHC ticket #25540.

foo :: Num a => a -> a
foo x = 2 * ( x + 1 )

{-# SPECIALISE foo :: Float -> Float, Double -> Double #-}
