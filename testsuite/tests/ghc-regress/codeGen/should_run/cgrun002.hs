main = print ((f id2) (10 + thirty_two))
  where
    f x = g x
      where
        g x = h x
	  where
	    h x = x

    thirty_two :: Int
    thirty_two = 32

id2 x = x
