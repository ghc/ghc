--!!! test arithmetic operations from "Prelude" (gcd, ldm, etc.)
--
main
  = let
	minInt = minBound :: Int
	maxInt = maxBound :: Int
    in
    putStr
       (-- w/ Ints
	show [
	    minInt, maxInt,

	    subtract i8 i4,
	    subtract i8m i4,
	    subtract maxInt i4,
	    subtract i0 minInt,

	    gcd i8 i6,
	    gcd i8m i6,
	    gcd i8m i6m,
	    gcd i8m i0,
	    gcd i0 i8m,
	    gcd (4070297::Int) (2695911::Int), -- 52,861
	    
	    lcm i8 i6,
	    lcm i8m i6,
	    lcm i8m i6m,
	    lcm i8m i0,
	    lcm i0 i8m,
	    lcm (4070297::Int) (2695911::Int), -- 207,585,147

	    i8 ^ i0,
	    i8m ^ i4,
	    i4 ^ i6
	    -- ToDo: more stuff

	    ]
    	++ "\n"

        -- w/ Integers
	++ show [
	    toInteger minInt, toInteger maxInt,

	    subtract i8I i4I,
	    subtract i8mI i4I,
	    subtract (toInteger maxInt) i4I,
	    subtract i0I (toInteger minInt),

	    gcd i8I i6I,
	    gcd i8mI i6I,
	    gcd i8mI i6mI,
	    gcd i8mI i0I,
	    gcd i0I i8mI,
	    gcd (4070297::Integer) (2695911::Integer), -- 52,861
	    
	    lcm i8I i6I,
	    lcm i8mI i6I,
	    lcm i8mI i6mI,
	    lcm i8mI i0I,
	    lcm i0I i8mI,
	    lcm (4070297::Integer) (2695911::Integer), -- 207,585,147

	    i8I ^ i0I,
	    i8mI ^ i4I,
	    i4I ^ i6I
	    -- ToDo: more stuff
	    ]
    	++ "\n"
    	)
  where
    i0, i4, i4m, i6, i6m, i8, i8m :: Int
    i0 = 0
    i4 = 4
    i4m = -4
    i6 = 6
    i6m = -6
    i8 = 8
    i8m = -8

    i0I, i4I, i4mI, i6I, i6mI, i8I, i8mI :: Integer
    i0I = 0
    i4I = 4
    i4mI = -4
    i6I = 6
    i6mI = -6
    i8I = 8
    i8mI = -8
