{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

--
-- Test case adopted from the HList library
-- http://www.cwi.nl/~ralf/HList/
--
-- Tests functional dependencies and overlapping instances

module Main where

import FakePrelude
import TypeEq
import TypeCast


--
-- Let's test.
-- The following should print "(HTrue,HFalse)".
--

main = print $ ( typeEq "42" "88"
               , typeEq "42" (42::Int)
               )
