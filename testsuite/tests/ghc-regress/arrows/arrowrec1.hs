{-# OPTIONS -farrows #-}

module ShouldCompile where

import Control.Arrow
import Data.Char

f :: ArrowLoop a => a Char Int
f = proc x -> do
	a <- returnA -< ord x
	rec	b <- returnA -< ord c - ord x
		c <- returnA -< chr a
	returnA -< b + ord c
