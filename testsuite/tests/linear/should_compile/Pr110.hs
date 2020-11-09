{-# LANGUAGE LinearTypes #-}
module Pr110 where

data Bloop = Bloop Bool

g :: Bloop %1 -> Bool
g (Bloop x) = x

h :: Bool %1 -> Bloop
h x = Bloop x
