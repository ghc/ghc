{-# LANGUAGE CPP, MagicHash, BlockArguments, UnboxedTuples #-}

-- Tests compilation for atomic exchange primop.

module M where

import GHC.Exts (atomicExchangeWord#, Word#, Addr#, State# )

swap :: Addr# -> Word# -> State# s -> (# #)
swap ptr val s = case (atomicExchangeWord# ptr val s) of
            (# s2, old_val #) -> (# #)
