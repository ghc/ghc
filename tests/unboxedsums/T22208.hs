{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE UnboxedSums               #-}
{-# LANGUAGE UnboxedTuples             #-}
module M where

import GHC.Base

-- Reproducer from #22208
foo :: (# Float# | Double# #) -> (# Float# | Float #)
foo (# x | #) = (# x | #)
bar :: (# Word# | Int64# #) -> (# Double# | Word# #)
bar (# y | #) = let x = y in (# | x #)
baz :: (# Word# | Word64# #) -> (# Word# | (##) #)
baz (# x | #) = (# x | #)

foo1 :: (# Float# | Double# #) -> (# Float# | Float #)
foo1 (# x | #) = (# x | #)
bar1 :: (# Word# | Int64# #) -> (# Double# | Word# #)
bar1 (# y | #) = let x = y in (# | x #)
baz1 :: (# Word# | Word64# #) -> (# Word# | (##) #)
baz1 (# x | #) = (# x | #)

-- i8 value from w64 slot
baz2 :: (# Int8# | Word64# #) -> (# Int8# | (##) #)
baz2 (# x | #) = (# x | #)

-- w8 value from w64 slot
baz3 :: (# Word8# | Word64# #) -> (# Word8# | (##) #)
baz3 (# x | #) = (# x | #)

-- w8 from w slot
baz4 :: (# Word8# | Word# #) -> (# Word8# | (##) #)
baz4 (# x | #) = (# x | #)

-- w from w slot
baz5 :: (# Word8# | Word# #) -> (# Word# | (##) #)
baz5 (# | x #) = (# x | #)

-- addr from w slot
baz6 :: (# Addr# | Word# #) -> (# Addr# | (##) #)
baz6 (# x | #) = (# x | #)