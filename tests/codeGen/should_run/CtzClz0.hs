{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts
import Control.Monad

#include <MachDeps.h>

{-# OPAQUE x #-} -- needed to avoid triggering constant folding
x :: Word
x = 0

main :: IO ()
main = do
  let !(W# w) = x

  guard (W# (ctz# w) == WORD_SIZE_IN_BITS)
  guard (W# (ctz8# w) == 8)
  guard (W# (ctz16# w) == 16)
  guard (W# (ctz32# w) == 32)

  guard (W# (clz# w) == WORD_SIZE_IN_BITS)
  guard (W# (clz8# w) == 8)
  guard (W# (clz16# w) == 16)
  guard (W# (clz32# w) == 32)
