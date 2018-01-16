module T11563 where

data T a = MkT
class C t
instance C s => C T
