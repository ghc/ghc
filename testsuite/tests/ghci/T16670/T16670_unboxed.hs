{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fwrite-interface #-}
module T16670_unboxed where

data UnboxedTupleData = MkUTD (# (),() #)

doThings :: UnboxedTupleData -> ()
doThings (MkUTD t) = ()
