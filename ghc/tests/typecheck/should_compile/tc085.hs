--!!! From a bug report from Satnam.
--!!! To do with re-exporting importees from PreludeGla* modules.
module ShouldSucceed ( module GlaExts ) where

--OLD: import PreludeGlaIO
import GlaExts

type FooType = Int
data FooData = FooData
