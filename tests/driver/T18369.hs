module T18369 where

import Unsafe.Coerce
import GHC.Exts (Any)

{-# NOINLINE emptyRecord #-}
emptyRecord :: Any
emptyRecord = unsafeCoerce EmptyElement

data TombStone = EmptyElement
