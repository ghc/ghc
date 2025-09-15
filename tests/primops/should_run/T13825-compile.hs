{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Exts
import Data.Word
import Data.Int

data Packed1 = Packed1 Float# Float# Int# Float#
    deriving Show

data Packed2 =
    Packed2
        {-# UNPACK #-} !Float
        {-# UNPACK #-} !Float
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !Float
    deriving Show

data Packed3 =
    Packed3
        {-# UNPACK #-} !Word8
        {-# UNPACK #-} !Int8
        {-# UNPACK #-} !Int64
        {-# UNPACK #-} !Float
        {-# UNPACK #-} !Word64
        {-# UNPACK #-} !Word32
        {-# UNPACK #-} !Float
        {-# UNPACK #-} !Double
    deriving Show

packed1 = go 0.0# 1.0# 2# 3.0#
  where
    go a b c d =
        Packed1 a b c d
            : go (a `plusFloat#` 1.0#)
                 (b `plusFloat#` 1.0#)
               (c +# 1#)
               (d `plusFloat#` 1.0#)

packed2 =
    [ Packed2
        (fromIntegral i)
        (fromIntegral (i + 1))
        (fromIntegral (i + 2))
        (fromIntegral (i + 3))
    | i <- [0..]
    ]

packed3 =
    [ Packed3
        (fromIntegral i)
        (fromIntegral (i + 1))
        (fromIntegral (i + 2))
        (fromIntegral (i + 3))
        (fromIntegral (i + 4))
        (fromIntegral (i + 5))
        (fromIntegral (i + 6))
        (fromIntegral (i + 6))
    | i <- [0..]
    ]

main :: IO ()
main = do
    print (take 3 packed1)
    print (take 3 packed2)
    print (take 3 packed3)
