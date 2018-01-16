{-# OPTIONS_GHC -O0 #-}
module T12076a(f) where

import GHC.Exts

{-# NOINLINE z #-}
z = ()

f :: () -> ()
f _ = let x = lazy z
      in g x x

{-# NOINLINE g #-}
g :: () -> () -> ()
g _ _ = ()
