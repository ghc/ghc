module T25650 (baz_foo, baz_bar) where

import Data.Word

data A
  = A1 {-# UNPACK #-} !Word32
  | A2 {-# UNPACK #-} !B

data B = B1 | B2

foo = A1 10
bar = A2 B2

data C = C {-# UNPACK #-} !A

baz_foo = C foo
baz_bar = C bar
