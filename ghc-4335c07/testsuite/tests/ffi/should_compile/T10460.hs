{-# LANGUAGE GHCForeignImportPrim #-}
module T10460 where
import GHC.Exts
-- don't link me!
foreign import prim "f" f :: Any -> Any
