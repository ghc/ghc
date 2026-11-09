{-# LANGUAGE MagicHash #-}
module ClosureTblAmode (toC) where

import GHC.Exts

-- An entry of a closure table, I64[(i << 3) + lbl] in the Cmm: a constructor
-- of C chosen by its tag.
data C = C0 | C1 | C2 | C3 | C4 | C5

toC :: Int -> C
toC (I# i) = tagToEnum# i
