-- !!! test RealFrac ops (ceiling/floor/etc.) on Floats/Doubles
--
main = 
 putStr $
  unlines
    [  -- just for fun, we show the floats to
       -- exercise the code responsible.
      show (float_list :: [Float])
    , show (double_list :: [Double])
       -- {Float,Double} inputs, {Int,Integer} outputs
    , show ((map ceiling float_list) :: [Int])
    , show ((map ceiling float_list) :: [Integer])
    , show ((map ceiling double_list) :: [Int])
    , show ((map ceiling double_list) :: [Integer])
    , show ((map floor float_list) :: [Int])
    , show ((map floor float_list) :: [Integer])
    , show ((map floor double_list) :: [Int])
    , show ((map floor double_list) :: [Integer])
    , show ((map truncate float_list) :: [Int])
    , show ((map truncate float_list) :: [Integer])
    , show ((map truncate double_list) :: [Int])
    , show ((map truncate double_list) :: [Integer])
{-
    , show ((map round float_list) :: [Int])
-}
    , show ((map round float_list) :: [Integer])
    , show ((map round double_list) :: [Int])
    , show ((map round double_list) :: [Integer])
    , show ((map properFraction float_list) :: [(Int,Float)])
    , show ((map properFraction float_list) :: [(Integer,Float)])
    , show $ head ((map properFraction double_list) :: [(Int,Double)])
{-
    , show ((map properFraction double_list) :: [(Integer,Double)])
-}
    ]
  where
    float_list :: [Float]
    float_list = [
	0.0, -0.0, 1.1, 2.8, 3.5, 4.5, -1.0000000001, -2.9999995,
	-3.50000000001, -4.49999999999, 1000012.0, 123.456, 100.25,
	102.5, 0.0012, -0.00000012, 1.7e4, -1.7e-4, 0.15e-6, pi,
	1.18088e+11, 	1.2111e+14 
      ]

    double_list :: [Double]
    double_list = [
	0.0, -0.0, 1.1, 2.8, 3.5, 4.5, -1.0000000001, -2.9999995,
	-3.50000000001, -4.49999999999, 1000012.0, 123.456, 100.25,
	102.5, 0.0012, -0.00000012, 1.7e4, -1.7e-4, 0.15e-6, pi,
	1.18088e+11, 	1.2111e+14 
      ]
