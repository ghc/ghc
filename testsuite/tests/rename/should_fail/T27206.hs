{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
module T27206 where

import GHC.Exts

-- Expect ok for known package rts and ghc-internal, and local/this
foreign import prim "rts          cmm_good1" cmm_good1 :: Int# -> Int#
foreign import prim "ghc-internal cmm_good2" cmm_good2 :: Int# -> Int#
foreign import prim "             cmm_good3" cmm_good3 :: Int# -> Int#

-- But expect failure for unknown package pkg_foo
foreign import prim "pkg-foo cmm_bad" cmm_bad :: Int# -> Int#
