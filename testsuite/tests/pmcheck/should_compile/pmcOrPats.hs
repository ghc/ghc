{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE OrPatterns #-}

data T = A | B
data U = V | W

g :: T -> U -> Int
g (one of A,B) V = 0
g B (one of V,W) = 1

h A (one of _,W) B = 0
h B (one of V,_) B = 1
h (one of A,B) _ B = 2

z (one of 1,2,1) = 0
z (one of 3,2) = 1
z 1 = 2

main = print 2