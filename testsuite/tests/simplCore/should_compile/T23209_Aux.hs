{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -O #-}
module T23209_Aux where

newtype I = MkI { uI :: () -> () }
newtype ArrayWriter = ArrayWriter (() -> I)
data Allocator = Allocator !ArrayWriter

combine :: Allocator -> Allocator -> (# () -> () #)
combine (Allocator (ArrayWriter w1)) (Allocator (ArrayWriter w2)) =
  (# \s -> id' (uI (w1 ()) (uI (w2 ()) s)) #)

e, s :: () -> I
e x = MkI id
s x = MkI id
{-# NOINLINE s #-}

id' :: () -> ()
id' x = x
