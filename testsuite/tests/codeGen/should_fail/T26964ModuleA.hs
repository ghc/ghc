{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

-- The failing -fcheck-prim-bounds primop is emitted while code-generating this
-- helper module, so the runtime diagnostic should report T26964ModuleA, not the
-- Main module that merely calls 'bad'.

module T26964ModuleA (bad) where

import GHC.Exts
import GHC.IO

{-# NOINLINE bad #-}
bad :: IO ()
bad = IO $ \s0 ->
  case newSmallArray# 5# () s0 of
    (# s1, marr #) -> readSmallArray# marr (-1#) s1
