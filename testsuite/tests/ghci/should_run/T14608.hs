{-# LANGUAGE UnboxedTuples #-}
module T14608 where

data UnboxedTupleData = MkUTD (# (),() #)

doThings :: UnboxedTupleData -> ()
doThings (MkUTD t) = ()
