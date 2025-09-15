module T21959 where

data R = R { fld :: Int }

f :: R -> R
f r = r { T21959.fld = 1, fld = 2 }
