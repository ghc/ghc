module Algo.Spectral ( spectral ) where

import Data.Vector.Unboxed as V

import Data.Bits

spectral :: Vector Double -> Vector Double
{-# NOINLINE spectral #-}
spectral us = us `seq` V.map row (V.enumFromTo 0 (n-1))
    where
      n = V.length us

      row i = i `seq` V.sum (V.imap (\j u -> eval_A i j * u) us)

      eval_A i j = 1 / fromIntegral r
        where
          r = u + (i+1)
          u = t `shiftR` 1
          t = n * (n+1)
          n = i+j

