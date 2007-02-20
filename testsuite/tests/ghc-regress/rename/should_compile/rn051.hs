{-# OPTIONS -fno-implicit-prelude #-}

-- This one crashed GHC 6.6 in lookupDeprec
-- See Trac #1128
-- and Note [Used names with interface not loaded]
-- in RnNames

module ShouldCompile where

import Prelude

foo :: Int -> Float
foo x = 3.0
