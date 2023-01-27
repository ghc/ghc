{-# OPTIONS_GHC -O2 -ddump-simpl -dno-typeable-binds -dsuppress-all -dsuppress-uniques #-}
module T22152b where

import Data.Int
import Data.Word

a :: Int32 -> Int32
a x = (x `quot` maxBound) `quot` maxBound -- overflow, mustn't trigger the rewrite rule

b :: Int -> Int
b x = (x `quot` 10) `quot` 20

c :: Word -> Word
c x = (x `quot` 10) `quot` 20

d :: Word8 -> Word8
d x = (x `quot` 10) `quot` 20

e :: Word16 -> Word16
e x = (x `quot` 10) `quot` 20

f :: Word32 -> Word32
f x = (x `quot` 10) `quot` 20

g :: Word64 -> Word64
g x = (x `quot` 10) `quot` 20

h :: Int8 -> Int8
h x = (x `quot` 10) `quot` 20

i :: Int16 -> Int16
i x = (x `quot` 10) `quot` 20

j :: Int32 -> Int32
j x = (x `quot` 10) `quot` 20

k :: Int64 -> Int64
k x = (x `quot` 10) `quot` 20
