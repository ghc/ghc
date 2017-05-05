{-# LANGUAGE MagicHash #-}
module T10600 where

import GHC.Prim

-- This shouldn't compile as unlifted bindings aren't allowed at top-level.
-- However, #10600 described the situation where an error isn't throw when we
-- compile with -fno-code.
foo :: Int#
foo = 10600#
