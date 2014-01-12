{-# LANGUAGE Safe #-}
module Dep09 where

import GHC.Ptr

bad1 = castFunPtrToPtr

