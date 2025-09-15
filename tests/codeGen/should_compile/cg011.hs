{-# LANGUAGE CPP, MagicHash, BlockArguments, UnboxedTuples #-}

-- Tests compilation for atomic exchange primop.

module M where

import GHC.Exts (atomicExchangeWordAddr#, Word#, Addr#, State# )

swap :: Addr# -> Word# -> State# s -> (# #)
swap ptr val s = case (atomicExchangeWordAddr# ptr val s) of
            (# s2, old_val #) -> (# #)
