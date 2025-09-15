-- !!! Conflicting constructors from two data type decls

module Foo where

data T1 = MkT Int

data T2 = MkT Bool

f (MkT x) = x
