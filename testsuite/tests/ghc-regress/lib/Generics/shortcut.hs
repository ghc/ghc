{-# OPTIONS -fglasgow-exts #-}

{-

The following example demonstrates a rather useless approach to
shortcutting traversals on the basis of type information about
constructors. We do run-time cut-off for top-down traversal with one
specific type case. This is only for illustrative purposes. The naive
approach here is prohibitively inefficient.

-}

module Main where

-----------------------------------------------------------------------------


import Data.Generics.Basics
import Data.Generics.Aliases
import Data.Generics.Reify


-----------------------------------------------------------------------------



everywhere1RT' :: (Data a, Data b) => (a -> a) -> b -> b
everywhere1RT' f t =
  if not $ reachableType (argType f) (val2type t)
   then t
   else gmapT (everywhere1RT' f) (mkT f t)

main = undefined
