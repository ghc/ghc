{-# OPTIONS -fglasgow-exts #-}

module ShouldCompile where

-- !!! test lit-lits in patterns

import Addr
{- Litlits are deprecated, aren't they?!
litlit_int    (``1''      :: Int)    = 42
litlit_word   (``1''      :: Word)   = 42
litlit_char   (`` '\n' '' :: Char)   = 42
litlit_addr   (``NULL''   :: Addr)   = 42
litlit_float  (``1.0''    :: Float)  = 42
litlit_double (``1.0''    :: Double) = 42
-}