module Operators where


(+++) :: [a] -> [a] -> [a]
a +++ b = a ++ b ++ a

($$$) :: [a] -> [a] -> [a]
a $$$ b = b +++ a

(***) :: [a] -> [a] -> [a]
(***) a [] = a
(***) a (_:b) = a +++ (a *** b)

(*/\*) :: [[a]] -> [a] -> [a]
a */\* b = concatMap (*** b) a

(**/\**) :: [[a]] -> [[a]] -> [[a]]
a **/\** b = zipWith (*/\*) [a +++ b] (a $$$ b)
