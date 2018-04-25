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
    -- int64H = 00456L#
    -- word64H = 00456L##
    x = 1
