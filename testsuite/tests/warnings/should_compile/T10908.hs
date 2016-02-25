{-# OPTIONS_GHC -fwarn-missing-exported-signatures #-}

module Bug (Data.List.intercalate, x) where

import qualified Data.List

intercalate = True

x :: Bool
x = intercalate
