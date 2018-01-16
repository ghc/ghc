module Matrices (Matrix,mat,rows,mulm) where
import Numbers
import Vectors
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- section 4: matrices

type Matrix = [Vector]

mat  :: [Vector] -> Matrix
mat  m    = m

rows :: Matrix -> [Vector]
rows m    = m

mulm :: Matrix -> Vector -> Vector
m `mulm` v = vec (map (inpr v) (rows m))
