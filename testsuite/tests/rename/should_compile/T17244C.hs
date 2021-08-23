{-# OPTIONS_GHC -Wcompat-unqualified-imports #-}

module T17244C (hello) where

-- This should not warn with -Wcompat-unqualified-imports.
import Data.List (sum)

hello :: [Int] -> Int
hello = sum

