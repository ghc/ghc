{-# OPTIONS_GHC -Wcompat-unqualified-imports #-}

module T17244B (hello) where

-- This should not warn with -Wcompat-unqualified-imports.
import qualified Data.List as List

hello :: [Int] -> Int
hello = List.sum

