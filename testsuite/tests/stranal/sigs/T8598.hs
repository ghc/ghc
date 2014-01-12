{-# LANGUAGE  MagicHash , UnboxedTuples #-}

module T8598(fun) where

import GHC.Float (Double(..))
import GHC.Integer (decodeDoubleInteger, encodeDoubleInteger)

-- Float.scaleFloat for Doubles, slightly simplified
fun :: Double -> Double
fun x | isFix           = x
      | otherwise       = case x of
          (D# x#) -> case decodeDoubleInteger x# of
            (# i, j #) -> D# (encodeDoubleInteger i j)
  where
  isFix = isDoubleFinite x == 0

foreign import ccall unsafe "isDoubleFinite" isDoubleFinite :: Double -> Int
