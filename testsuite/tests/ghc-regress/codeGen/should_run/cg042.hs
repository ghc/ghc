-- !!! mutable Double array test (ncg test)
--
module Main ( main ) where

import PrelBase --ghc1.3
import IOExts
import ByteArray
import MutableArray
import ST
import Int( fromInt )

import Ratio   -- 1.3
import Array   -- 1.3

main = --primIOToIO (newDoubleArray (0,1) >>= \ arr -> readDoubleArray arr 0) >>= print
 putStr test_doubles


test_doubles :: String
test_doubles
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> ByteArray Int

    f size@(I# size#)
      = runST (
	    -- allocate an array of the specified size
	  newDoubleArray (0, (size-1))	>>= \ arr# ->

	    -- fill in all elements; elem i has "i * pi" put in it
	  fill_in arr# 0# (size# -# 1#) >>

	    -- freeze the puppy:
	  freezeByteArray arr#
	)

    fill_in :: MutableByteArray s Int -> Int# -> Int# -> ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then return ()
	else writeDoubleArray arr_in# (I# first#) ((fromInt (I# first#)) * pi) >>
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: ByteArray Int -> Int# -> Int# -> [Double]
    lookup_range arr from# to#
      = if (from# ># to#)
	then []
	else (indexDoubleArray arr (I# from#))
	     : (lookup_range arr (from# +# 1#) to#)
