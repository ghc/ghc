module Lib where

data Number = F {-# UNPACK #-} !Float | I {-# UNPACK #-} !Int

-- This UNPACK was causing a panic:
--   ghc-stage1: panic! (the 'impossible' happened)
--     (GHC version 8.1.20160722 for x86_64-unknown-linux):
--           LocalReg's live-in to graph crG {_grh::F32, _gri::I64}
data T = T {-# UNPACK #-} !Number
