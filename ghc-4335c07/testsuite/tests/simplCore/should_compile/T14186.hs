module T14186 where

foo f g (x, xs) = (f (g x), map (f . g) xs)
bar f g (x, xs) = (f (g x), map (f . g) xs)
