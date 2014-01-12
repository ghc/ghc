{-# LANGUAGE UnboxedTuples, MagicHash, BangPatterns #-}

module ShouldCompile where

import Data.Ix
import GHC.Exts


f ixs@(_, ix_end) frozen# =
 let
  !n# =
   case (
	 if null (range ixs)
	  then 0
	  else 1
        ) of { I# x -> x }
 in
 (# frozen#, False #)
