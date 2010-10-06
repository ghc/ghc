{-# LANGUAGE UnboxedTuples, MagicHash #-}

module ShouldCompile where

import Ix
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
