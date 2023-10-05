{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module TwoProdFMA where
import GHC.Exts

twoProductFloat# :: Float# -> Float# -> (# Float#, Float# #)
twoProductFloat# x y = let !r = x `timesFloat#` y
                       in (# r, fmsubFloat# x y r #)
