{-# OPTIONS_GHC -fglasgow-exts #-}

-- Data type returns the wrong type

module ShouldFail where

data T a where
     P :: L1 -> L2

data L1 = L1
data L2 = L2
