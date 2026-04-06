{-# language DataKinds, MagicHash, UnboxedTuples #-}

module ListTuplePunsFail6 where

import Data.Tuple.Experimental (Tuple#)

unboxedSoloFam :: Int
unboxedSoloFam =
  case (# 1 #) :: Tuple# Int of
    _ -> 0
