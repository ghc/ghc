module Global where

data Maybe a = Nothing | Just a

maybeToBool Nothing = False
maybeToBool (Just _) = True
maybeToA Nothing = error "tried to get something from Maybe Nothing"
maybeToA (Just x) = x
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just x:xs) = x:catMaybes xs

