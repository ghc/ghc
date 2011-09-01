module Ghci025C (f, g, h) where

import Ghci025D

g x = f x + 1

h x = x `div` 2

data C = C {x :: Int}
