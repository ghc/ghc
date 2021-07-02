{-# OPTIONS -Woperator-whitespace #-}

module T19372consym where

import Data.List.NonEmpty

a_suffix      = \x y -> x: y
a_prefix      = \x y -> x :y
a_tight_infix = \x y -> x:y
a_loose_infix = \x y -> x : y   -- Only this one should be without a warning.

b_suffix      = \x y -> x:| y
b_prefix      = \x y -> x :|y
b_tight_infix = \x y -> x:|y
b_loose_infix = \x y -> x :| y  -- Only this one should be without a warning.
