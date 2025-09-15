{-# LANGUAGE MagicHash, UnboxedTuples #-}

module RepPolyTuple4 where

import GHC.Exts ( TYPE )

bar :: forall r (a :: TYPE r). a -> a -> (# a, a #)
bar = (# , #) @_ @_
