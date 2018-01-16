-- !!! deriving Ix on d. type with one constructor
module ShouldSucceed where

import Data.Ix

data Pair a b = Pair a b deriving (Show, Ord, Eq, Ix)
