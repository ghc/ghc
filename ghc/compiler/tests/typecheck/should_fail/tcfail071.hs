--!!! Mis-matched contexts in a mutually recursive group

module Foo7( f ) where

f :: (Ord c) => c -> c
f c = g c

g :: c -> c
g c = c
  where p = foldr (f c) [] []
