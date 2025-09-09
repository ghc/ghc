{-# LANGUAGE MagicHash #-}
module T25718a where

import GHC.Exts

-- Test a limitation of range analysis: if we improve the analysis, it could
-- become a constant function `\_ -> True`
foo :: Int# -> Bool
foo x = case isTrue# (x ># 0#) of
  True  -> {- Here we should know that x's range is > 0 -}
           not (isTrue# (x <=# 0#)) -- we write it this way to avoid CSE for `x ># 0#` to kick in
  False -> {- ... and here <= 0 -}
           isTrue# (x <=# 0#)
