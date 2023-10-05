-- !! check if tc type substitutions really do
-- !! clone (or if not, work around it by cloning
-- !! all binders in first pass of the simplifier).
module ShouldCompile where

f,g :: Eq a => (a,b)
f = g
g = f
