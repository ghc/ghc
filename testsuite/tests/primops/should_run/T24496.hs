{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import GHC.Exts

twoProductFloat# :: Float# -> Float# -> (# Float#, Float# #)
twoProductFloat# x y = let !r = x `timesFloat#` y
                       in (# r, fmsubFloat# x y r #)
{-# NOINLINE twoProductFloat# #-}

twoProductDouble# :: Double# -> Double# -> (# Double#, Double# #)
twoProductDouble# x y = let !r = x *## y
                        in (# r, fmsubDouble# x y r #)
{-# NOINLINE twoProductDouble# #-}

main :: IO ()
main = do
    print $ case twoProductFloat# 2.0# 3.0# of (# r, s #) -> (F# r, F# s)
    print $ case twoProductDouble# 2.0## 3.0## of (# r, s #) -> (D# r, D# s)
