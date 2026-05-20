{-# LANGUAGE GHCForeignImportPrim, MagicHash #-}

module Ccfail009 where

import GHC.Exts

foreign import prim unsafe "my_primop" myPrimop :: Int# -> Int#