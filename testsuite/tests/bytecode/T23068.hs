{-# LANGUAGE MagicHash, UnboxedTuples #-}
module T23068 where
import GHC.Exts

f :: () -> (# Int, Int #)
f () = (# 0, 0 #)

g :: () -> (# Int#, Int#, Int #)
g () = (# 0#, 0#, 0 #)
