--! From a bug report from Satnam.
--! To do with re-exporting importees from PreludeGla* modules.
module Foo ( PreludePrimIO.., {-PreludeGlaIO..,-} Foo..) where

--OLD: import PreludeGlaIO
import PreludePrimIO

type FooType = Int
data FooData = FooData
