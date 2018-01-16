{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Exts
import System.Environment

main = do
  [n] <- fmap (fmap read) getArgs
  case g n of
    (# a, b, c, d, e, f, g, h, i #) -> print a

-- a deep stack in which each frame is an unboxed tuple-return, to exercise
-- the stack underflow machinery.
g :: Int -> (# Int,Float,Double,Int#,Float#,Double#,Int,Float,Double #)
g 0 = (# 1, 2.0, 3.0, 1#, 2.0#, 3.0##, 1, 2.0, 3.0 #)
g x = case g (x-1) of
        (# a, b, c, d, e, f, g, h, i #) ->
          (# a+1, b, c, d, e, f, g, h, i #)
