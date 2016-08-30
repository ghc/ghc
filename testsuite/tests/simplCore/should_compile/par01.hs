module Par01 where

import GHC.Conc

-- The smoking gun in -ddump-prep is:
--  case Par01.depth d of sat { __DEFAULT -> sat }
-- this should never happen!

depth :: Int -> Int
depth d = d `par` depth d
