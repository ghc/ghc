-- Test for trac #2141

module Foo where

foo :: () -> ()
foo x = x { foo = 1 }
