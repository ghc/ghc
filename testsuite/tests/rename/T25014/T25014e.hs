-- Should compile as A.foo matches parent
module T25014b (A (foo)) where

data A = A {
  foo :: Int
}
