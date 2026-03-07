module T26615 where

import T26615a

f :: HashMap String a -> HashMap String b -> Bool
f = disjointSubtrees 0
