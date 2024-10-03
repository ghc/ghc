{-# OPTIONS_GHC -Wcompat-unqualified-imports #-}

module T17244B (hello) where

-- This wouldn't warn with -Wcompat-unqualified-imports.
-- The flag is deprecated now.
import qualified Data.List as List

hello :: [Int] -> Int
hello = List.sum

