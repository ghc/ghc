--!!! From a bug report from Satnam.
--!!! To do with re-exporting importees from PreludeGla* modules.
module ShouldSucceed ( module IOExts, module GHC ) where

--OLD:   import GlaExts
--OLDER: import PreludeGlaIO
import IOExts
import GHC

type FooType = Int
data FooData = FooData
