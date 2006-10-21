
{-# OPTIONS_GHC -fwarn-incomplete-patterns -Wall #-}

-- Test for trac #851
-- Should not give a non-exhaustive pattern warning

module ShouldCompile where

import Data.Word

f :: Word -> Bool
f 0 = True
f (_n + 1) = False

