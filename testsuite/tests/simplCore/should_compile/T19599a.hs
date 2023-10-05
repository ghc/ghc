module Spec where

class C a where { foo,bar :: [a] -> [a] }

instance C Int where
     foo x = r_bar x
     bar xs = reverse xs

r_bar :: C a => [a] -> [a]
r_bar (x:xs) = bar (xs ++ r_bar xs)
r_bar [] = []

-- We should specialise `r_bar` at Int
-- C.f. Note Note [Avoiding loops (non-DFuns)] in GHC.Core.Opt.Specialise
