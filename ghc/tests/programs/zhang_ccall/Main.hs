{-# OPTIONS -#include "ccall.h" #-}
-- The 'bug' here was the omission of prototypes for
-- a C function peddling floats.
module Main where

import IOExts

main = putStr (shows res "\n")
	where
	res = unsafePerformIO ca
	ca :: IO Float
	ca = _ccall_ nn
