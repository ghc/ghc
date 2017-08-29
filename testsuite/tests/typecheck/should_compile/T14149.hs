{-# OPTIONS_GHC -fdefer-out-of-scope-variables #-}

module Foo where

import Data.Coerce

f :: Bool
f = coerce (k :: Int)
