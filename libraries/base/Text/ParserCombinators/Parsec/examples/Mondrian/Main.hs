-----------------------------------------------------------
-- Daan Leijen (c) 1999-2000, daan@cs.uu.nl
-----------------------------------------------------------
module Main where

import MonParser (prettyFile)


main    :: IO ()
main	= prettyFile "prelude.m"
