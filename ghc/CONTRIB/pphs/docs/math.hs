-- list concatenation (right-associative)
(++)			:: [a] -> [a] -> [a]
xs ++ ys		=  foldr (:) ys xs
