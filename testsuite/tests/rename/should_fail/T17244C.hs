{-# OPTIONS_GHC -Wcompat-unqualified-imports #-}

module T17244C (hello) where

-- This should not warn with -Wcompat-unqualified-imports.
-- But not his fails, as sum name clashes with Prelude
import Data.List (sum)

hello :: [Int] -> Int
hello = sum

