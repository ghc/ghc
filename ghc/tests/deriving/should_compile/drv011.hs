-- !!! deriving Enum, but not Ord.
module ShouldSucceed where

data ABC = A | B | C deriving Enum

x = [A ..C]
