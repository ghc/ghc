module T13825 where

import Data.Int
import Data.Word

data Packed =
    Packed
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Int8
      {-# UNPACK #-} !Word16
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Int
  deriving (Show)

-- Test a top-level constant
packed :: Packed
packed = Packed 1.0 2.0 3 4 5 6

packedAll :: [Packed]
packedAll =
    packed :
    [ Packed
        (fromIntegral i)
        (fromIntegral (i + 1))
        (fromIntegral (i + 2))
        (fromIntegral (i + 3))
        (fromIntegral (i + 3))
        (fromIntegral (i + 4))
    | i <- [1.. 4]
    ]

addOne :: Packed -> Packed
addOne (Packed a b c d e f) =
    Packed (a + 1.0) (b + 1.0) (c + 1) (d + 1) (e + 1.0) (f + 1)

mapAddOne :: [Packed] -> [Packed]
mapAddOne = map addOne
