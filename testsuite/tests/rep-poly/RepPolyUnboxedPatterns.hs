{-# LANGUAGE UnboxedTuples, MagicHash #-}

module RepPolyUnboxedPatterns where

import GHC.Exts

foo :: forall rep1 (a :: TYPE rep1) rep2 (b :: TYPE rep2). (# a, b #) -> ()
foo (# bndr_a, bndr_b #) = ()

bar :: forall rep1 (a :: TYPE rep1) rep2 (b :: TYPE rep2). (# a | b #) -> ()
bar (# bndr_a | #) = ()
bar (# | bndr_b #) = ()
