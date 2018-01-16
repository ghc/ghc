module MyList (minus) where

minus                 :: (Eq x) => [x] -> [x] -> [x]
xs `minus` ys         =  foldl rmv xs ys
rmv                   :: (Eq x) => [x] -> x -> [x]
[] `rmv` y            =  []
(x:xs) `rmv` y        =  if  x == y  then  xs  else  x : (xs `rmv` y)
