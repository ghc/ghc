--!!! test RealFrac ops (ceiling/floor/etc.) on Floats/Doubles
--
main
  = putStr
       (-- {Float,Double} inputs, {Int,Integer} outputs
	show ((map ceiling float_list) :: [Int])
    	++ "\n"
	++ show ((map ceiling float_list) :: [Integer])
    	++ "\n"
	++ show ((map ceiling double_list) :: [Int])
    	++ "\n"
	++ show ((map ceiling double_list) :: [Integer])
    	++ "\n"
	++ show ((map floor float_list) :: [Int])
    	++ "\n"
	++ show ((map floor float_list) :: [Integer])
    	++ "\n"
	++ show ((map floor double_list) :: [Int])
    	++ "\n"
	++ show ((map floor double_list) :: [Integer])
    	++ "\n"
	++ show ((map truncate float_list) :: [Int])
    	++ "\n"
	++ show ((map truncate float_list) :: [Integer])
    	++ "\n"
	++ show ((map truncate double_list) :: [Int])
    	++ "\n"
	++ show ((map truncate double_list) :: [Integer])
    	++ "\n"
	++ show ((map round float_list) :: [Int])
    	++ "\n"
	++ show ((map round float_list) :: [Integer])
    	++ "\n"
	++ show ((map round double_list) :: [Int])
    	++ "\n"
	++ show ((map round double_list) :: [Integer])
    	++ "\n"
	++ show ((map properFraction float_list) :: [(Int,Float)])
    	++ "\n"
	++ show ((map properFraction float_list) :: [(Integer,Float)])
    	++ "\n"
	++ show ((map properFraction double_list) :: [(Int,Double)])
    	++ "\n"
	++ show ((map properFraction double_list) :: [(Integer,Double)])
    	++ "\n"
    	)
  where
    float_list :: [Float]
    float_list = [
	0.0, -0.0, 1.1, 2.8, 3.5, 4.5, -1.0000000001, -2.9999995,
	-3.50000000001, -4.49999999999, 1000012.0, 123.456, 100.25,
	102.5, 0.0012, -0.00000012, 1.7e4, -1.7e-4, 0.15e-6, pi
      ]

    double_list :: [Double]
    double_list = [
	0.0, -0.0, 1.1, 2.8, 3.5, 4.5, -1.0000000001, -2.9999995,
	-3.50000000001, -4.49999999999, 1000012.0, 123.456, 100.25,
	102.5, 0.0012, -0.00000012, 1.7e4, -1.7e-4, 0.15e-6, pi
      ]
