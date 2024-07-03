-- Should not compile trying to export a name with the wrong parent
module T25014b (A (foo)) where

data A

foo = 1
