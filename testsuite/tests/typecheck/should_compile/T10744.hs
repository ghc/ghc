{-# LANGUAGE MagicHash #-}
module T10744 where

import GHC.Exts
import GHC.Magic

-- Checks if oneShot is open-kinded

f0 :: Int -> Int
f0 = oneShot $ \n -> n

f1 :: Int# -> Int
f1 = oneShot $ \n# -> I# n#

f2 :: Int -> Int#
f2 = oneShot $ \(I# n#) -> n#

