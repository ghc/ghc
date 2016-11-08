{-# LANGUAGE MagicHash, GHCForeignImportPrim #-}

module T10461 where
import GHC.Exts

foreign import prim cheneycopy :: Any -> Word#

foreign import prim "foo" foo :: Any -> Word#
