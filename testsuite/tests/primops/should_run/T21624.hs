{-# LANGUAGE MagicHash #-}

import GHC.Int
import GHC.Prim

mulIntMayOflo :: Int -> Int -> Bool
mulIntMayOflo (I# x) (I# y) = I# (mulIntMayOflo# x y) /= 0

main :: IO ()
main = print (mulIntMayOflo maxBound 0x4)

