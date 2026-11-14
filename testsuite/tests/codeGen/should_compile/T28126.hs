{-# LANGUAGE MagicHash #-}
module T28126 (f) where

import GHC.Exts

-- At -O the address folds to a label at offset -1. AArch64's getRegister'
-- takes a label's offset from -2048 to 2047 by a clause whose assembly
-- the pretty-printer's check_off accepts only for 0 to 4095.
f :: Int -> Addr#
{-# NOINLINE f #-}
f _ = plusAddr# "hello"# (-1#)
