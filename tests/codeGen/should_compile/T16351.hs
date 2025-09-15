module T16351 where

import Data.Bits

x1,x2,x3,x4,x5,x6,x7,x8 :: Int -> Int -> Int

x1 a b = a .&. (a .|. b)
x2 a b = a .|. (a .&. b)
x3 a b = a .&. (b .|. a)
x4 a b = a .|. (b .&. a)
x5 a b = (a .|. b) .&. a
x6 a b = (a .&. b) .|. a
x7 a b = (b .|. a) .&. a
x8 a b = (b .&. a) .|. a

-- add more nesting
x10,x11,x12,x13,x14 :: Int -> Int -> Int -> Int

x10 a b c = a .&. ((a .|. b) .|. c)
x11 a b c = a .&. (c .|. (a .|. b))
x12 a b c = a .&. (c .|. (b .|. a))
x13 a b c = a .&. ((b .|. a) .|. c)
x14 a b c = a .&. ((c .|. (b .|. a)) .|. c)
