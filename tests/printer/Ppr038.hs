{-# LANGUAGE MagicHash #-}
module LiteralsTest2 where

x,y :: Int
x = 0003
y = 0x04

s :: String
s = "\x20"

c :: Char
c = '\x20'

d :: Double
d = 0.00

blah = x
  where
    charH = '\x41'#
    intH = 0004#
    wordH = 005##
    floatH = 3.20#
    doubleH = 04.16##
    intNH = 1000#Int
    int8H = 1008#Int8
    int16H = 1016#Int8
    int32H = 1032#Int32
    int64H = 1064#Int64
    wordNH = 2000#Word
    word8H = 2008#Word8
    word16H = 2016#Word16
    word32H = 2032#Word32
    word64H = 2064#Word64
    x = 1
