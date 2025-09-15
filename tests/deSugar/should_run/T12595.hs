{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Base

-- In #12595 a bogus desugaring led (bizarrely)
-- to a top-level binding maxInt = maxInt
-- This test just checks that doesn't happen again

main = print (let !x = maxInt in even x)
