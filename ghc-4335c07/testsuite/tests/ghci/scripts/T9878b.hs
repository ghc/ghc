{-# LANGUAGE StaticPointers #-}
module T9878b where

import GHC.StaticPtr

f = deRefStaticPtr (static True)
