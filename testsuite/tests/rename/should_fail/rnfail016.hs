{-# OPTIONS_GHC -fno-warn-typed-holes #-}
module ShouldFail where

-- !!! Pattern syntax in expressions

f x = x@x

