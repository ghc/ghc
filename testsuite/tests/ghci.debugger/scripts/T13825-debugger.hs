{-# LANGUAGE MagicHash #-}
module T13825 where

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
        {-# UNPACK #-} !Word16
        {-# UNPACK #-} !Word64
        {-# UNPACK #-} !Word32
        {-# UNPACK #-} !Float
        {-# UNPACK #-} !Double
    deriving Show

packed1 = Packed1 12.34# 56.78# 42# 99.99#
packed2 = Packed2 12.34 56.78 42 99.99
packed3 = Packed3 1 2 3 4 5 6 7.8 9.0
