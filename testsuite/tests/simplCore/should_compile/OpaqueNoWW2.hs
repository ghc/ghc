module OpaqueNoWW2 where

{-# LANGUAGE MagicHash #-}
module M where

import GHC.Exts
import GHC.IO

data T a = MkT !Bool !a

fun :: T a -> IO a
{-# OPAQUE fun #-}
fun (MkT _ x) = IO $ \s -> noinline seq# x s
