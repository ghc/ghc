{-# OPTIONS_GHC -Wcompat-unqualified-imports #-}

module T17244A (hello) where

-- This should NOT warn with -Wcompat-unqualified-imports,
-- Instead this just fails.
import Data.List

hello :: [Int] -> Int
hello = sum
