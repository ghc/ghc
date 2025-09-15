{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -fwrite-interface #-}
{-
  GHCi doesn't automatically switch to object code anymore now that
  UnboxedTuples are supported in bytecode. But we test for the
  existence of the file.
 -}
{-# OPTIONS_GHC -fobject-code #-}

module T16670_unboxed where

data UnboxedTupleData = MkUTD (# (),() #)

doThings :: UnboxedTupleData -> ()
doThings (MkUTD t) = ()
