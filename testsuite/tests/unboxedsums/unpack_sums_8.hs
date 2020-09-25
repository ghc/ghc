{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}

module Main where

data Void
data WithVoid = LV Void | RV
data EnumT = L | R
    deriving Show

data BoxEnum = BoxEnum {-# UNPACK #-} !EnumT
    deriving Show

l = BoxEnum L
r = BoxEnum R

main = do
    print l
    print r


data BoxWithVoid = BoxWithVoid {-# UNPACK #-} !WithVoid
wv = BoxWithVoid (LV undefined)

data BoxVoid = BoxVoid {-# UNPACK #-} Void
bv = BoxVoid undefined

data BoxSum = BoxS {-# UNPACK #-} !(# Int | Char #)
bs = BoxS (# 1 | #)
