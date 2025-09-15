module T13588 where

newtype T a = MkT a
f (x, y) = (MkT x, y)
{-# NOINLINE f #-}


bar x =
    let y = f (x,x) in
    let z = case y of (MkT x,y) -> (x,y) in
    (z,z)
