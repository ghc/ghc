{-# LANGUAGE MagicHash #-}
module M where

import GHC.Exts
import GHC.IO

data T a = MkT !Bool !a

fun :: T a -> IO a
{-# OPAQUE fun #-}
fun (MkT _ x) = IO $ \s -> noinline seq# x s
-- evaluate/seq# should not produce its own eval for x
-- since it is properly tagged (from a strict field)

-- uses noinline to prevent caseRules from eliding the seq# in Core
