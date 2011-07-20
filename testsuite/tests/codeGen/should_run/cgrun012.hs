{-# LANGUAGE MagicHash #-}
-- !!! move arguments around on the stacks, mainly the B stack

import GHC.Base	( Float#, Double#, Int#, Int(..) )


main = print foo

foo = I#
	( f 1.1##
	    2.1#
	    True
	    3.1##
	    4.1#
	    5.1##
	    6.1##
	    42#	-- the answer!
	    7.1#
	    8.1# )
    where
      f :: Double# -> Float# -> Bool -> Double# -> Float#
	-> Double# -> Double# -> Int# -> Float# -> Float#
	-> Int#
      f b1 s2 t b3 s4 b5 b6 i42 s7 s8
	-- evens, then odds
	= g s2 b3 b5 i42 s8 b1 t s4 b6 s7

      g :: Float# -> Double# -> Double# -> Int# -> Float#
        -> Double# -> Bool -> Float# -> Double# -> Float#
	-> Int#
      g s2 b3 b5 i42 s8 b1 t s4 b6 s7
	-- powers of 2 backwards, then others forwards
	= h s7 b6 t b5 s2 b3 i42 s8 b1 s4

      h :: Float# -> Double# -> Bool -> Double# -> Float#
        -> Double# -> Int# -> Float# -> Double# -> Float#
	-> Int#
      h s7 b6 t b5 s2 b3 i42 s8 b1 s4
	= i42
