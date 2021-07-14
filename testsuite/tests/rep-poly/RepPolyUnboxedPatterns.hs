{-# LANGUAGE UnboxedTuples, MagicHash #-}

module RepPolyUnboxedPatterns where

import GHC.Exts

foo :: forall rep1 (a :: TYPE rep1) rep2 (b :: TYPE rep2). (# a, b #) -> ()
foo (# a, b #) = ()

bar :: forall rep1 (a :: TYPE rep1) rep2 (b :: TYPE rep2). (# a | b #) -> ()
bar (# a | #) = ()
bar (# | b #) = ()
