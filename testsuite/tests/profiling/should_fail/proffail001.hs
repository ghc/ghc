
module Foo where

wibble :: a
wibble = {-# SCC "foo bar" #-} wibble

