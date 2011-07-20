{-# LANGUAGE MagicHash, BangPatterns #-}

-- Killed GHC 6.3 HEAD

module Bug where
import GHC.Base

foo v = let !(I# x#) = 7 * 7 in "Forty-Two"