-- !!! Checking that lazy name clashing works
module ShouldCompile where

import List ( sort )

sort :: Int
sort = 3

foo :: Int
foo = ShouldCompile.sort

baz :: (Ord a) => [a] -> [a]
baz = List.sort

