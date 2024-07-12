{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- !!! test arithmetic vector operations

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
  print (FX4# (plusFloatX4# (broadcastFloatX4# 1.3#) (broadcastFloatX4# 2.2#)))
  print (FX4# (minusFloatX4# (broadcastFloatX4# 3.5#) (broadcastFloatX4# 2.2#)))
  print (FX4# (timesFloatX4# (broadcastFloatX4# 2.4#) (broadcastFloatX4# 2.2#)))
  print (FX4# (divideFloatX4# (broadcastFloatX4# 9.2#) (broadcastFloatX4# 4.0#)))
  print (FX4# (negateFloatX4# (broadcastFloatX4# 3.5#)))

  print (DX2# (plusDoubleX2# (broadcastDoubleX2# 1.3##) (broadcastDoubleX2# 2.2##)))
  print (DX2# (minusDoubleX2# (broadcastDoubleX2# 3.5##) (broadcastDoubleX2# 2.2##)))
  print (DX2# (timesDoubleX2# (broadcastDoubleX2# 2.4##) (broadcastDoubleX2# 2.2##)))
  print (DX2# (divideDoubleX2# (broadcastDoubleX2# 9.2##) (broadcastDoubleX2# 4.0##)))
  print (DX2# (negateDoubleX2# (broadcastDoubleX2# 3.5##)))
