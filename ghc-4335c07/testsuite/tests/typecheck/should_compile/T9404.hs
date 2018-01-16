{-# LANGUAGE UnboxedTuples #-}

module T9404 where

foo _ = case seq () (# #) of (# #) -> ()
foo2 _ = case () `seq` (# #) of (# #) -> ()
