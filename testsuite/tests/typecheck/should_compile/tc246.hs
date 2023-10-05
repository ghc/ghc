-- Test for trac #3066
-- GHC with optimisation off would go into an infinite loop

module Tc246 () where

newtype Foo = Foo Foo

