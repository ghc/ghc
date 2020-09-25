
module UnpackedSums8 where

-- Unpack a sum of 100 ints in each constructor
data Unpackee
   = U !Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int

    | O Word Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int
        Int Int Int Int Int Int Int Int Int Int

data Box = Box {-# UNPACK #-} !Unpackee

b = Box $ U 0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0
            0 0 0 0 0 0 0 0 0 0
