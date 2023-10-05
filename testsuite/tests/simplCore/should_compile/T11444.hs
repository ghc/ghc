{-# LANGUAGE MagicHash, BangPatterns #-}

-- Produces a Lint error in GHC 8.0

module T11444 where
import GHC.Exts (reallyUnsafePtrEquality#, Int (..))

ptrEq :: a -> a -> Bool
ptrEq !x !y = I# (reallyUnsafePtrEquality# x y) == 1
