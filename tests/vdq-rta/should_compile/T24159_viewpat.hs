{-# LANGUAGE ViewPatterns #-}

-- Test the -Wview-pattern-signatures warning
module T24159_viewpat where

import Data.Maybe

f1 (isJust -> True :: Bool) = ()
f1 _ = ()

f2 (id -> isJust -> True :: Bool) = ()
f2 _ = ()
