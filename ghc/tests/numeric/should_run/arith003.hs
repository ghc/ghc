-- !!! test arithmetic operations from "Prelude" (gcd, lcm, etc.)
--
main
  = putStr
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

	++ show [
	    quotRem i8 i6,
	    quotRem i8m i6,
	    quotRem i8m i6m,
	    -- quotRem i8m i0,  -- no div by zero
	    quotRem i0 i8m,
	    quotRem (4070297::Int) (2695911::Int), -- 52,861

	    divMod i8 i6,
	    divMod i8m i6,
	    divMod i8m i6m,
	    -- divMod i8m i0,  -- no div by zero
	    divMod i0 i8m,
	    divMod (4070297::Int) (2695911::Int) -- 52,861
            ]
    	++ "\n"

        -- w/ Integers
	++ show [
	    minIntI, maxIntI,

	    subtract i8I i4I,
	    subtract i8mI i4I,
	    subtract maxIntI i4I,
	    subtract i0I minIntI,

	    gcd i8I i6I,
	    gcd i8mI i6I,
	    gcd i8mI i6mI,
	    gcd i8mI i0I,
	    gcd i0I i8mI,
	    gcd (4070297::Integer) (2695911::Integer), -- 52,861
	    gcd minIntI (-1),   -- out of Int range
	    gcd minIntI (1),
	    gcd (-1) minIntI,
	    gcd (1)  minIntI,
	    
	    lcm i8I i6I,
	    lcm i8mI i6I,
	    lcm i8mI i6mI,
	    lcm i8mI i0I,
	    lcm i0I i8mI,
	    lcm (4070297::Integer) (2695911::Integer), -- 207,585,147
	    lcm minIntI (-1),   -- out of Int range
	    lcm minIntI (1),
	    lcm (-1) minIntI,
	    lcm (1)  minIntI,

	    i8I ^ i0I,
	    i8mI ^ i4I,
	    i4I ^ i6I
	    -- ToDo: more stuff
	    ]
    	++ "\n"
        
	++ show [
	    quotRem i8I i6I,
	    quotRem i8mI i6I,
	    quotRem i8mI i6mI,
	    -- quotRem i8mI i0I,  -- no div by zero
	    quotRem i0I i8mI,
	    quotRem (4070297::Integer) (2695911::Integer), -- 52,861
	    quotRem minIntI (-1),   -- out of Int range
	    quotRem minIntI (1),
	    quotRem (-1) minIntI,
	    quotRem (1)  minIntI,

	    divMod i8I i6I,
	    divMod i8mI i6I,
	    divMod i8mI i6mI,
	    -- divMod i8mI i0I,  -- no div by zero
	    divMod i0I i8mI,
	    divMod (4070297::Integer) (2695911::Integer), -- 52,861
	    divMod minIntI (-1),   -- out of Int range
	    divMod minIntI (1),
	    divMod (-1) minIntI,
	    divMod (1)  minIntI
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

minInt = minBound :: Int
maxInt = maxBound :: Int

minIntI = fromIntegral minInt :: Integer
maxIntI = fromIntegral maxInt :: Integer
