{-# LANGUAGE Haskell2010 #-}
-- Tests for collapsable headers
module Bug335 where

{-|
=== __ExF:__
abc
-}
f :: ()
f = ()

{-|
=== __ExG:__
>>> a
b

>>> c
d

==== Under ex
foo

== Out of Ex
foo
-}
g :: ()
g = ()
