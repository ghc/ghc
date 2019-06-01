{-# LANGUAGE CPP, MagicHash, BlockArguments, UnboxedTuples #-}

-- Tests compilation for interlockedExchange primop.

module M where

import GHC.Exts (interlockedExchangeInt#, Int#, Addr#, State# )

swap :: Addr# -> Int# -> State# s -> (# #)
swap ptr val s = case (interlockedExchangeInt# ptr val s) of
            (# s2, old_val #) -> (# #)
