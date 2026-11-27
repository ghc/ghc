{-# LANGUAGE MagicHash #-}

module T28169 where
import GHC.Exts

f :: Int# -> (Int,Int)
f x = let y = I# (let z = x +# 1# in z *# z)
      in (y + y, y * y)
