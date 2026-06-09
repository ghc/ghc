{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

-- Test that -fcheck-prim-bounds reports the failing primop, the offending
-- index and the array size. The negative index also checks that it is reported
-- as a signed number (e.g. -1, not a huge unsigned word).

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
    IO $ \s0 ->
      case newSmallArray# 5# () s0 of
        (# s1, marr #) -> readSmallArray# marr (-1#) s1
