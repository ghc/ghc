-- !!! basic Rational operations
--
import Data.Ratio

main
  = putStr
       (-- Ratio Ints
	show [i0a, i0b, i0c, i2a, i2b, im2a, im2b, i_pi, i_misc]
    ++  "\n"
	-- the Ints
    ++  show ((map numerator   [i0a, i0b, i0c, i2a, i2b, im2a, im2b, i_pi, i_misc])
	    ++(map denominator [i0a, i0b, i0c, i2a, i2b, im2a, im2b, i_pi, i_misc]))
    ++  "\n"
	-- Booleans
--  ++	show []
--  ++  "\n"

	-- Rationals (Ratio Integers)
    ++	show [r0a, r0b, r0c, r2a, r2b, rm2a, rm2b, r_pi, r_misc]
    ++  "\n"
	-- the Integers
    ++  show ((map numerator   [r0a, r0b, r0c, r2a, r2b, rm2a, rm2b, r_pi, r_misc])
	    ++(map denominator [r0a, r0b, r0c, r2a, r2b, rm2a, rm2b, r_pi, r_misc]))
    ++  "\n"
	-- Booleans
--  ++	show []
--  ++  "\n"
	)
  where  
    i0a, i0b, i0c, i2a, i2b, im2a, im2b, i_pi, i_misc :: Ratio Int

    i0a	    =    0 % 1
    i0b	    = (-0) % 1
    i0c	    =    0 % (-1)
    i2a	    =    4 % 2
    i2b	    = (-4) % (-2)
    im2a    = (-4) % 2
    im2b    =    4 % (-2)
    i_pi    =   22 % 7
    i_misc  =    2 % 10000

    r0a, r0b, r0c, r2a, r2b, rm2a, rm2b, r_pi, r_misc :: Rational

    r0a	    =    0 % 1
    r0b	    = (-0) % 1
    r0c	    =    0 % (-1)
    r2a	    =    4 % 2
    r2b	    = (-4) % (-2)
    rm2a    = (-4) % 2
    rm2b    =    4 % (-2)
    r_pi    =   22 % 7
    r_misc  =    2 % 10000
