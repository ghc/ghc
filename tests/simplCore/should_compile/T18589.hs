{-# LANGUAGE MagicHash #-}
module T18589 where

import GHC.Exts

-- See Note [Guarding against silly shifts]
-- Make sure that a silly shift is optimized correctly
f1 x = uncheckedIShiftL# x -1#
f2 x = uncheckedIShiftRA# x -1#
f3 x = uncheckedIShiftRL# x -1#
f4 x = uncheckedShiftL# x -1#
f5 x = uncheckedShiftRL# x -1#
