--!!! Testing arithmetic operators

-- Int primitives

 -- standard show function will produce garbage for primMinInt
test1 = show (1 + minBound::Int, minBound::Int)
test2 = show (maxBound::Int)
test3 = show $ (1 + 2::Int)
test4 = show $ (1 - 2::Int)
test5 = show $ (3 * 5::Int)
test6 = show $ (-(10::Int))
test7 = show $ (even (10::Int), even (11::Int))
test8 = show $ (10 == (10::Int), 10 == (11::Int))
test9 = show $ [ x `quotRem` (y::Int) | x <- [-5,0,5], y <- [-3,3] ]
test10 = show $ [ x `divMod` (y::Int) | x <- [-5,0,5], y <- [-3,3] ]
test11 = show $ 1 `quot` (0::Int)
test12 = show $ 1 `rem` (0::Int)

-- Integer primitives

--test21 = show (1 + minBound::Integer, minBound::Integer)
--test22 = show (maxBound::Integer)
test23 = show $ (1 + 2::Integer)
test24 = show $ (1 - 2::Integer)
test25 = show $ (3 * 5::Integer)
test26 = show $ (-(10::Integer))
test27 = show $ (even (10::Integer), even (11::Integer))
test28 = show $ (10 == (10::Integer), 10 == (11::Integer))
test29 = show $ [ x `quotRem` (y::Integer) | x <- [-5,0,5], y <- [-3,3] ]
test30 = show $ [ x `divMod` (y::Integer) | x <- [-5,0,5], y <- [-3,3] ]
test31 = show $ 1 `quot` (0::Integer)
test32 = show $ 1 `rem` (0::Integer)

-- Float primitives

--test41 = show (1 + minBound::Float, minBound::Float)
--test42 = show (maxBound::Float)
test43 = show $ (1 + 2::Float)
test44 = show $ (1 - 2::Float)
test45 = show $ (3 * 5::Float)
test46 = show $ (-(10::Float))
test47 = show $ (10 == (10::Float), 10 == (11::Float))
test48 = show $ [ x / (y::Float) | x <- [-5,0,5], y <- [-3,3] ]
test49 = show $ 1 / (0::Float)

test50 = show $ (pi::Float)
test51 = show $ map sin  [0.0, 0.3, 0.6, 1.0::Float]
test52 = show $ map cos  [0.0, 0.3, 0.6, 1.0::Float]
test53 = show $ map tan  [0.0, 0.3, 0.6, 1.0::Float]
test54 = show $ map asin [0.0, 0.3, 0.6, 1.0::Float]
test55 = show $ map acos [0.0, 0.3, 0.6, 1.0::Float]
test56 = show $ map atan [0.0, 0.3, 0.6, 1.0::Float]
test57 = show $ map exp  [0.0, 0.3, 0.6, 1.0::Float]

test58 = show $ map log  [0.3, 0.6, 1.0, 10.0::Float]
test59 = show $ log 0.0

--primitive primLog10Float "primLog10Float" :: Float -> Float
--test60 = show $ map primLog10Float [0.3, 0.6, 1.0, 10.0]
--test61 = show $ primLog10Float 0.0

test62 = show $ map sqrt [0.0, 0.3, 0.6, 1.0::Float]
test63 = show $ sqrt (-1.0::Float)

-- not in Hugs prelude, rounds towards zero
--primitive primFloatToInt "primFloatToInt" :: Float -> Int
--test64 = show $ map primFloatToInt [-2,-1.6,-1.5,-1.4,-1,0,1,2.0,2.4,2.5,2.6,pi,10]

test65 = show $ floatDigits (1.0 :: Float)
test66 = show $ floatDigits (error "test66" :: Float) -- laziness check

test67 = show $ floatRange (1.0 :: Float)
test68 = show $ floatRange (error "test68" :: Float) -- laziness check

test69 = show $ floatRadix (1.0 :: Float)
test70 = show $ floatRadix (error "test70" :: Float) -- laziness check



-- Double primitives

--test81 = show (1 + minBound::Double, minBound::Double)
--test82 = show (maxBound::Double)
test83 = show $ (1 + 2::Double)
test84 = show $ (1 - 2::Double)
test85 = show $ (3 * 5::Double)
test86 = show $ (-(10::Double))
test87 = show $ (10 == (10::Double), 10 == (11::Double))
test88 = show $ [ x / (y::Double) | x <- [-5,0,5], y <- [-3,3] ]
test89 = show $ 1 / (0::Double)

test90 = show $ (pi::Double)
test91 = show $ map sin  [0.0, 0.3, 0.6, 1.0::Double]
test92 = show $ map cos  [0.0, 0.3, 0.6, 1.0::Double]
test93 = show $ map tan  [0.0, 0.3, 0.6, 1.0::Double]
test94 = show $ map asin [0.0, 0.3, 0.6, 1.0::Double]
test95 = show $ map acos [0.0, 0.3, 0.6, 1.0::Double]
test96 = show $ map atan [0.0, 0.3, 0.6, 1.0::Double]
test97 = show $ map exp  [0.0, 0.3, 0.6, 1.0::Double]

test98 = show $ map log  [0.3, 0.6, 1.0, 10.0::Double]
test99 = show $ log 0.0

--primitive primLog10Double "primLog10Double" :: Double -> Double
--test100 = show $ map primLog10Double [0.3, 0.6, 1.0, 10.0]
--test101 = show $ primLog10Double 0.0

test102 = show $ map sqrt [0.0, 0.3, 0.6, 1.0::Double]
test103 = show $ sqrt (-1.0::Double)

-- not in Hugs prelude, rounds towards zero
--primitive primDoubleToInt "primDoubleToInt" :: Double -> Int
--test104 = show $ map primDoubleToInt [-2,-1.6,-1.5,-1.4,-1,0,1,2.0,2.4,2.5,2.6,pi,10]

test105 = show $ floatDigits (1.0 :: Double)
test106 = show $ floatDigits (error "test106" :: Double) -- laziness check

test107 = show $ floatRange (1.0 :: Double)
test108 = show $ floatRange (error "test108" :: Double) -- laziness check

test109 = show $ floatRadix (1.0 :: Double)
test110 = show $ floatRadix (error "test110" :: Double) -- laziness check


-- Char primitives

test120 = show ('a' == 'b', 'b' == 'b', 'b' == 'a')
test121 = show ('a' <= 'b', 'b' <= 'b', 'b' <= 'a')


