data T a b = T (a -> b)

f g y = () where x = T g
