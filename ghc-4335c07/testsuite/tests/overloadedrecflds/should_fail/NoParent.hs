{-# LANGUAGE DuplicateRecordFields #-}
module NoParent (A(x)) where

data A = A
data B = B { x :: Int }
data C = C { x :: String }
