-- The span-less copies of f1's and f3's 'merge' render identically, so
-- one warning covers both; f2's 'merge' differs in type and gets its
-- own warning.
module T27628i where

import T27628i_M

g1 :: Int -> T Int -> T Int
g1 x t = f1 x (f1 x t)

g2 :: Int -> S Int -> S Int
g2 x t = f2 x (f2 x t)

g3 :: Int -> T Int -> T Int
g3 x t = f3 x (f3 x t)
