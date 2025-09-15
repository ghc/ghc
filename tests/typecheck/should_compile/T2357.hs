{-# LANGUAGE NoMonomorphismRestriction #-}

module Foo where

f :: Read a => a
-- This one needs NoMonomorphismRestriction else f could
-- not get a polymorphic type
(f, _) = (read "3", True)

g :: Read a => a
g = f
