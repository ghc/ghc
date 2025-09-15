{-# OPTIONS_GHC -Wcompat-unqualified-imports #-}

module T17244C (hello) where

-- This would not warn with -Wcompat-unqualified-imports.
-- The flag is deprecated now.
import Data.List (sum)

hello :: [Int] -> Int
hello = sum

