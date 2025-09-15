{-# LANGUAGE   CApiFFI
             , CPP
             , GHCForeignImportPrim
             , MagicHash
  #-}

-- | Check that libm foreign import log1p/expm1
--   are equivalent to that of the primops
--   for float/double
module Main ( main ) where

import GHC.Float (Floating(..))

main :: IO ()
main = do
  print $ oldEqualsNewDouble log1pDoubleOld log1pDoubleNew randomDouble
  print $ oldEqualsNewDouble expm1DoubleOld expm1DoubleNew randomDouble
  print $ oldEqualsNewFloat log1pFloatOld log1pFloatNew randomFloat
  print $ oldEqualsNewFloat expm1FloatOld expm1FloatNew randomFloat

foreign import capi unsafe "math.h log1p" log1pDoubleOld :: Double -> Double
foreign import capi unsafe "math.h expm1" expm1DoubleOld :: Double -> Double
foreign import capi unsafe "math.h log1pf" log1pFloatOld :: Float -> Float
foreign import capi unsafe "math.h expm1f" expm1FloatOld :: Float -> Float

oldEqualsNewDouble :: (Double -> Double) -> (Double -> Double) -> Double -> Bool
oldEqualsNewDouble f g x = f x == g x

oldEqualsNewFloat :: (Float -> Float) -> (Float -> Float) -> Float -> Bool
oldEqualsNewFloat f g x = f x == g x

log1pDoubleNew, expm1DoubleNew :: Double -> Double
log1pDoubleNew = log1p
expm1DoubleNew = expm1

log1pFloatNew, expm1FloatNew :: Float -> Float
log1pFloatNew = log1p
expm1FloatNew = expm1

randomFloat :: Float
randomFloat = 53213

randomDouble :: Double
randomDouble = 41901526
