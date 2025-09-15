-- Minimal reproducer for https://gitlab.haskell.org/ghc/ghc/-/issues/22033
{-# LANGUAGE MagicHash #-}
import Numeric
import GHC.Exts

a :: Float
a = F# (int2Float# 0xFFFFFF7FFFFFFFF#)

f :: Int# -> Float#
f x = int2Float# x
{-# NOINLINE f #-}

main :: IO ()
main = do
    putStrLn (showHFloat a "")
    putStrLn (showHFloat (F# (f 0xFFFFFF7FFFFFFFF#)) "")
