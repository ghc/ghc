{-# OPTIONS_GHC -fno-warn-type-holes #-}
module ShouldFail where

-- !!! Pattern syntax in expressions

f x = x @ x
g x = ~ x
h x = _

