-- !!! test quot/rem/div/mod functions on Ints and Integers
--
main
  = putStr
       (-- w/ Ints and Integers
	show (unzipWith div ints_list)
    	++ "\n"
	++ show (unzipWith div integers_list)
    	++ "\n"
	++ show (unzipWith rem ints_list)
    	++ "\n"
	++ show (unzipWith rem integers_list)
    	++ "\n"
	++ show (unzipWith quot ints_list)
    	++ "\n"
	++ show (unzipWith quot integers_list)
    	++ "\n"
	++ show (unzipWith mod ints_list)
    	++ "\n"
	++ show (unzipWith mod integers_list)
    	++ "\n"
	++ show (unzipWith law1 ints_list)
    	++ "\n"
	++ show (unzipWith law1 integers_list)
    	++ "\n"
	++ show (unzipWith law2 ints_list)
    	++ "\n"
	++ show (unzipWith law2 integers_list)
    	++ "\n"
    	)
  where
    ints_list :: [(Int, Int)]
    integers_list :: [(Integer, Integer)]

    ints_list = [
	(0, 4),
	(0, -8),
	(7, 3),
	(13, 4),
	(13, -4),
	(-13, 4),
	(-13, -4),
	(12345678, 10000),
	(12345678, -10000),
	(-12345678, 10000),
	(-12345678, -10000),
	(123456,10000),
	(1234567,20000),
	(12345678,-10000),
	(123456789,10000),
	(1234567890,-10000),
	(-12345,10000),
	(-123456789,-10000)
	]

    integers_list = [
	(0, 4),
	(0, -8),
	(7, 3),
	(13, 4),
	(13, -4),
	(-13, 4),
	(-13, -4),
	(12345678, 10000),
	(12345678, -10000),
	(-12345678, 10000),
	(-12345678, -10000),
	(123456,10000),
	(1234567,20000),
	(12345678,-10000),
	(123456789,10000),
	(1234567890,-10000),
	(-12345,10000),
	(-123456789,-10000),
	(12345678900,500000000),
	(1234000000000000000000005678900,5001111111111111000000)
	]

unzipWith :: (a -> b -> c) -> [(a,b)] -> [c]
unzipWith f []	       = []
unzipWith f ((x,y):zs) = f x y : unzipWith f zs

law1, law2 :: Integral a => a -> a -> Bool

law1 x y = (x `quot` y)*y + (x `rem` y) == x
law2 x y = (x `div`  y)*y + (x `mod` y) == x
