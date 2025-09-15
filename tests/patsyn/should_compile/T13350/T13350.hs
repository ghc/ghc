{-# OPTIONS_GHC -Wincomplete-patterns #-}
module T13350 where

import Boolean

booleanToInt :: Boolean -> Int
booleanToInt F = 0
booleanToInt T = 1
