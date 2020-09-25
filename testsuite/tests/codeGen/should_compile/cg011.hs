{-# LANGUAGE CPP, MagicHash, BlockArguments, UnboxedTuples #-}

-- Tests compilation for atomic exchange primop.

module M where

import GHC.Exts (atomicExchangeInt#, Int#, Addr#, State# )

swap :: Addr# -> Int# -> State# s -> (# #)
swap ptr val s = case (atomicExchangeInt# ptr val s) of
            (# s2, old_val #) -> (# #)
