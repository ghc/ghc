main = print (length thirteen_ones)
     where
	thirteen_ones	= take (13::Int) ones

	ones :: [Int]
	ones		= 1 : ones
