import Data.Char

{-# RULES "map-loop" [~] forall f . map' f = map' (id . f) #-}

{-# NOINLINE map' #-}
map' f [] = []
map' f (x:xs) = f x : map' f xs

main = print (map' toUpper "Hello, World")
