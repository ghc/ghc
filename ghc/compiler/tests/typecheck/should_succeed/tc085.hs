--!!! From a bug report from Satnam.
--!!! To do with re-exporting importees from PreludeGla* modules.
module Foo ( module GlaExts, module Foo ) where

--OLD: import PreludeGlaIO
import GlaExts

type FooType = Int
data FooData = FooData
