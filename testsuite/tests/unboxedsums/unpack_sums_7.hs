module UnpackedSums7 where

data T = MkT {-# UNPACK #-} !MI

data MI = NoI | JI Int

t = MkT (JI 5)
