{-# LANGUAGE  MagicHash , UnboxedTuples #-}

module T8598(fun) where

import GHC.Float (Double(..))
import GHC.Num.Integer (integerDecodeDouble#, integerEncodeDouble#)

-- Float.scaleFloat for Doubles, slightly simplified
fun :: Double -> Double
fun x | isFix           = x
      | otherwise       = case x of
          (D# x#) -> case integerDecodeDouble# x# of
            (# i, j #) -> D# (integerEncodeDouble# i j)
  where
  isFix = isDoubleFinite x == 0

foreign import ccall unsafe "isDoubleFinite" isDoubleFinite :: Double -> Int
