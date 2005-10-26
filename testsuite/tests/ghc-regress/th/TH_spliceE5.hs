{-# OPTIONS -fglasgow-exts #-}

module Main where

import TH_spliceE5_Lib

v1 = "foo"

main = putStrLn $(expandVars ["v1","v2"])
-- The splice expands to refer to both v1 and v2,
-- and the test checks that we don't dependency-analyse
-- the program so that one or the other isn't in scope
-- to the type checker

    
v2 = "bar"
