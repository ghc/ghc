{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Safe #-}
module Dep07 where

import GHC.ForeignPtr

bad1 = unsafeForeignPtrToPtr

