{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- !!! test the packing of floats and doubles into a vector

import GHC.Exts

data FloatX4 = FX4# FloatX4#

instance Show FloatX4 where
  show (FX4# f) = case (unpackFloatX4# f) of
    (# a, b, c, d #) -> show ((F# a), (F# b), (F# c), (F# d))

data DoubleX2 = DX2# DoubleX2#

instance Show DoubleX2 where
  show (DX2# d) = case (unpackDoubleX2# d) of
    (# a, b #) -> show ((D# a), (D# b))


main :: IO ()
main = do
  print (FX4# (packFloatX4# (# 9.2#, 8.15#, 7.0#, 6.4# #)))
  print (DX2# (packDoubleX2# (# 7.2##, 9.3## #)))
