-- !!! Checking that lazy name clashing work.
module ShouldSucceed where

import List ( sort )

ShouldSucceed.sort :: Int
ShouldSucceed.sort = 3

