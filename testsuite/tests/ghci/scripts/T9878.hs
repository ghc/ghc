{-# LANGUAGE StaticPointers #-}
module T9878 where

import GHC.StaticPtr

f = deRefStaticPtr (static True)
