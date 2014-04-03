-- Tests that the listed order of functions grouped under a single
-- type signature is preserved as in-source. Before fixing #188, it
-- seems to have preserved the first function but reversed the rest.
module Bug188 where

class A a where
  f, g, h, i :: a -> ()
