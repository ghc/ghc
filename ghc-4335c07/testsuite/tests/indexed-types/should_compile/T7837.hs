{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module T7837 where

type family Scalar a
class Fractional (Scalar a) => Norm a where
  norm :: a -> Scalar a

type instance Scalar Double = Double
instance Norm Double where norm = abs

normalize :: (Norm a, a ~ Scalar a) => a -> a
normalize x = x / norm x
{-# NOINLINE normalize #-}

normalize_Double :: Double -> Double
normalize_Double = signum
{-# NOINLINE normalize_Double #-}

-- This rule should fire in 'foo'
{-# RULES "normalize/Double" normalize = normalize_Double #-}

foo :: Double
foo = normalize (4 :: Double)
