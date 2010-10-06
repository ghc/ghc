{-# LANGUAGE MagicHash #-}

module ShouldCompile where

import GHC.Exts

data STRef s a = STRef (MutVar# s a)

-- ghc 4.08 had a problem with returning a MutVar#.

from :: STRef s a -> MutVar# s a
from (STRef x) = x

to :: MutVar# s a -> STRef s a
to x = STRef x
