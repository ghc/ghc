-- Trac #2735

module Bug where

data S = S { s1 :: (), s2 :: () }

f s = s { s1 = (), s2 = s1 s }
