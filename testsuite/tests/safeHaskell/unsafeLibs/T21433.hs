{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Safe #-}

module T21433 where

import GHC.Prim.PtrEq

f x y = reallyUnsafePtrEquality x y
