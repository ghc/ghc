--!!! deriving Enum on d. type with nullary constructors
module ShouldSucceed where

data D = D1 deriving(Eq,Ord)
