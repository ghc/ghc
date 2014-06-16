
-- !!! From a bug report from Satnam.
-- !!! To do with re-exporting importees from PreludeGla* modules.
module ShouldSucceed ( module GHC.Prim ) where

import GHC.Prim

type FooType = Int
data FooData = FooData
