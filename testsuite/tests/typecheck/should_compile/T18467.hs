{-# OPTIONS_GHC -fdefer-type-errors #-}

module T18467 where

----------------------
x = 3   -- Monomorphism restriction; we get x::alpha

f y = x::Float
g z = x::Int

-------------
h v = let t = \y -> (v, y : y) in fst (t True)
