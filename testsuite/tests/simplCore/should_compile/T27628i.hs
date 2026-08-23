-- The span-less copies of T27628i_M's two 'merge' loops render
-- identically, so a single warning must cover both, listing both
-- reboxed constructors.
module T27628i where

import T27628i_M

g1 :: Int -> T Int -> T Int
g1 x t = f1 x (f1 x t)

g2 :: Int -> S Int -> S Int
g2 x t = f2 x (f2 x t)
