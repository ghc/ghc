-- !!! Checking that a toplevel declaration 'f' in module M is accessible
-- !!! as both 'f' and 'M.f' within the scope of M. Similarly for imported
-- !!! entities.
module ShouldSucceed where

import List ( sort )

x :: Int
x = 2

y :: Int
y = x

z :: Int
z = ShouldSucceed.x

sortOf :: Ord a=> [a] -> [a]
sortOf = List.sort
