-- !!! deriving Enum on d. type with nullary constructors
module ShouldSucceed where

data AD = A | B | C | D deriving (Enum)
