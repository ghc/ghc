{-# OPTIONS_GHC -Wcompat-unqualified-imports #-}

module T17244A (hello) where

-- This used to warn with -Wcompat-unqualified-imports.
-- Now it shows the flag is deprecated.
import Data.List

hello :: [Int] -> Int
hello = sum
