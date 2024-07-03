-- Should not compile trying to export a missing name
module T25014c (A (foo)) where

data A

data B = B {
  foo :: Int
}
