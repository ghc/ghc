{-# language HexFloatLiterals, MagicHash, NegativeLiterals #-}
module T22155 where

import GHC.Types

a = D# 0x0.1p12##
b = D# -0x0.1p12##
c = F# 0x0.1p12#
d = F# -0x0.1p12#
