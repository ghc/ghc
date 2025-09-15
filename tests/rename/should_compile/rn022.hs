-- this is legal, I think (WDP)

module Confused where

import Prelude hiding (otherwise)

otherwise = False

f x | otherwise = 1

g otherwise | otherwise = 2
