module Foo where

newtype Identity x = MkId x
newtype Fix f = MkFix (f (Fix f))

-- This test just checks that the compiler itself doesn't loop
data Loop = LCon {-# UNPACK #-} !(Fix Identity)
