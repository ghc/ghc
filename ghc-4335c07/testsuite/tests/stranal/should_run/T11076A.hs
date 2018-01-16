{-# OPTIONS_GHC -O #-}
{-# LANGUAGE MagicHash,
             ForeignFunctionInterface,
             UnliftedFFITypes,
             GHCForeignImportPrim,
             BangPatterns
  #-}
module T11076A where

import GHC.Exts
import Unsafe.Coerce

{-
   If the demand type for the foreign call argument is incorrectly strict,
   the bang pattern can be optimized out
 -}
testBool :: Bool -> Int
testBool !x = I# (cmm_testPrim (unsafeCoerce x))
{-# INLINE testBool #-}

foreign import prim "testPrim" cmm_testPrim :: Any -> Int#
