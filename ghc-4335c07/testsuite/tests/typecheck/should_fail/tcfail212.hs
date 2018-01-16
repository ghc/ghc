{-# LANGUAGE ConstraintKinds, MagicHash #-}
module ShouldFail where

import GHC.Exts

-- If we turn on ConstraintKinds the typing rule for
-- tuple types is generalised. This test checks that
-- we get a reasonable error for unreasonable tuples.

f :: (Maybe, Either Int)
f = (Just 1, Left 1)

g :: (Int#, Int#)
g = (1#, 2#)