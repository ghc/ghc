-- !!! deriving Ix on d. type with nullary constructors
module ShouldSucceed where

import Ix

data AD = A | B | C | D deriving (Show, Ord, Eq, Ix)

