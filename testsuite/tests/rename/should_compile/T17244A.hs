{-# OPTIONS_GHC -Wcompat-unqualified-imports #-}

module T17244A (hello) where

-- This should warn with -Wcompat-unqualified-imports.
import Data.List

hello :: [Int] -> Int
hello = sum
