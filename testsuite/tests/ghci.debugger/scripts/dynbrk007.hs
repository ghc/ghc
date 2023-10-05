f :: Maybe Int
f = do
   i <- return 1
   j <- return 2
   k <- return 3
   return i