-- Should not compile trying to export a missing name
module T25014b (A (foo)) where

data A
