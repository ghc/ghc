{-# LANGUAGE LinearTypes #-}
module Pr110 where

data Bloop = Bloop Bool

g :: Bloop ->. Bool
g (Bloop x) = x

h :: Bool ->. Bool
h x = x
