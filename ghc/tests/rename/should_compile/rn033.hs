-- !!! Checking that lazy name clashing works
module ShouldSucceed where

import List ( sort )

sort :: Int
sort = 3

foo :: Int
foo = ShouldSucceed.sort

baz :: (Ord a) => [a] -> [a]
baz = List.sort

