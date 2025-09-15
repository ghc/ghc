module A where

data F a b = F { x :: !Int, y :: !(Float,Float), z :: !(a,b) }
