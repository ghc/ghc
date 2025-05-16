{-# LANGUAGE MagicHash, ExplicitNamespaces #-}
module DataToTagWarnings (X(X2, X3), f, g) where

import GHC.Exts (dataToTag#, (+#), Int#)

-- The constructor imports should not produce unused-imports warnings.
-- They are required by dataToTag# in f.
import qualified GHC.Int (data I#)
import Prelude (Int, Maybe(Nothing, Just))

f :: (Int, Maybe t) -> Int#
f (p, q) = dataToTag# p +# dataToTag# q


data X = X1 | X2 | X3 | X4

g :: X -> Int#
g X2 = 12#
g v = dataToTag# v
  -- Although the DataToTag constraint here checks that X1 and X4 are in scope,
  -- this check would be unnecessary if they were just removed.
  -- So this *should* result in -Wunused-top-binds warnings for X1 and X4.
