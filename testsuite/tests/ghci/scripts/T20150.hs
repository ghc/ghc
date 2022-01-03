{-# LANGUAGE StaticPointers #-}
module T20150 where

import GHC.StaticPtr

foo :: StaticPtr Int
foo = static 0


