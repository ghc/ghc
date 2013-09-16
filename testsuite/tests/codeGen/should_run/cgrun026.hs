{-# LANGUAGE MagicHash #-}

-- !!! simple tests of primitive arrays
--
module Main ( main ) where

import GHC.Exts
import Data.Char 	( chr )

import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

import Data.Ratio

main = putStr
	 (test_chars	++ "\n"  ++
	  test_ints	++ "\n"  ++
	  test_addrs	++ "\n"  ++
	  test_floats	++ "\n"  ++
	  test_doubles	++ "\n"  ++
	  test_ptrs	++ "\n")


-- Arr# Char# -------------------------------------------
-- (main effort is in packString#)

test_chars :: String
test_chars
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> UArray Int Char

    f size@(I# size#)
      = runST (
	    -- allocate an array of the specified size
	  newArray_ (0, (size-1))	>>= \ arr# ->

	    -- fill in all elements; elem i has "i" put in it
	  fill_in arr# 0# (size# -# 1#) >>

	    -- freeze the puppy:
	  freeze arr#
	)

    fill_in :: STUArray s Int Char -> Int# -> Int# -> ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then return ()
	else writeArray arr_in# (I# first#) ((chr (I# first#))) >>
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: UArray Int Char -> Int# -> Int# -> [Char]
    lookup_range arr from# to#
      = if (from# ># to#)
	then []
	else (arr ! (I# from#))
	     : (lookup_range arr (from# +# 1#) to#)

-- Arr# Int# -------------------------------------------

test_ints :: String
test_ints
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> UArray Int Int

    f size@(I# size#)
      = runST (
	    -- allocate an array of the specified size
	  newArray_ (0, (size-1))	>>= \ arr# ->

	    -- fill in all elements; elem i has i^2 put in it
	  fill_in arr# 0# (size# -# 1#) >>

	    -- freeze the puppy:
	  freeze arr#
	)

    fill_in :: STUArray s Int Int -> Int# -> Int# -> ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then return ()
	else writeArray arr_in# (I# first#) (I# (first# *# first#)) >>
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: UArray Int Int -> Int# -> Int# -> [Int]
    lookup_range arr from# to#
      = if (from# ># to#)
	then []
	else (arr ! (I# from#))
	     : (lookup_range arr (from# +# 1#) to#)

-- Arr# Addr# -------------------------------------------

test_addrs :: String
test_addrs
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> UArray Int (Ptr ())

    f size@(I# size#)
      = runST (
	    -- allocate an array of the specified size
	  newArray_ (0, (size-1))	>>= \ arr# ->

	    -- fill in all elements; elem i has i^2 put in it
	  fill_in arr# 0# (size# -# 1#) >>

	    -- freeze the puppy:
	  freeze arr#
	)

    fill_in :: STUArray s Int (Ptr ()) -> Int# -> Int# -> ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then return ()
	else writeArray arr_in# (I# first#)
			    (Ptr (int2Addr# (first# *# first#))) >>
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: UArray Int (Ptr ()) -> Int# -> Int# -> [ Int ]
    lookup_range arr from# to#
      = let
	    a2i (Ptr a#) = I# (addr2Int# a#)
	in
	if (from# ># to#)
	then []
	else (a2i (arr ! (I# from#)))
	     : (lookup_range arr (from# +# 1#) to#)

-- Arr# Float# -------------------------------------------

test_floats :: String
test_floats
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> UArray Int Float

    f size@(I# size#)
      = runST (
	    -- allocate an array of the specified size
	  newArray_ (0, (size-1))	>>= \ arr# ->

	    -- fill in all elements; elem i has "i * pi" put in it
	  fill_in arr# 0# (size# -# 1#) >>

	    -- freeze the puppy:
	  freeze arr#
	)

    fill_in :: STUArray s Int Float -> Int# -> Int# -> ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then return ()
{-	else let e = ((fromIntegral (I# first#)) * pi)
	     in trace (show e) $ writeFloatArray arr_in# (I# first#) e >>
	     fill_in arr_in# (first# +# 1#) last#
-}
	else writeArray arr_in# (I# first#) ((fromIntegral (I# first#)) * pi) >>
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: UArray Int Float -> Int# -> Int# -> [Float]
    lookup_range arr from# to#
      = if (from# ># to#)
	then []
	else (arr ! (I# from#))
	     : (lookup_range arr (from# +# 1#) to#)

-- Arr# Double# -------------------------------------------

test_doubles :: String
test_doubles
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42# 416#) "\n"
  where
    f :: Int -> UArray Int Double

    f size@(I# size#)
      = runST (
	    -- allocate an array of the specified size
	  newArray_ (0, (size-1))	>>= \ arr# ->

	    -- fill in all elements; elem i has "i * pi" put in it
	  fill_in arr# 0# (size# -# 1#) >>

	    -- freeze the puppy:
	  freeze arr#
	)

    fill_in :: STUArray s Int Double -> Int# -> Int# -> ST s ()

    fill_in arr_in# first# last#
      = if (first# ># last#)
	then return ()
	else writeArray arr_in# (I# first#) ((fromIntegral (I# first#)) * pi) >>
	     fill_in arr_in# (first# +# 1#) last#

    lookup_range :: UArray Int Double -> Int# -> Int# -> [Double]
    lookup_range arr from# to#
      = if (from# ># to#)
	then []
	else (arr ! (I# from#))
	     : (lookup_range arr (from# +# 1#) to#)

-- Arr# (Ratio Int) (ptrs) ---------------------------------
-- just like Int# test

test_ptrs :: String
test_ptrs
  = let arr# = f 1000
    in
	shows (lookup_range arr# 42 416) "\n"
  where
    f :: Int -> Array Int (Ratio Int)

    f size
      = runST (
	  newArray (1, size) (3 % 5)	>>= \ arr# ->
	  -- don't fill in the whole thing
	  fill_in arr# 1 400		>>
	  freeze arr#
	)

    fill_in :: STArray s Int (Ratio Int) -> Int -> Int -> ST s ()

    fill_in arr_in# first last
      = if (first > last)
	then return ()
	else writeArray arr_in# first (fromIntegral (first * first)) >>
	     fill_in  arr_in# (first + 1) last

    lookup_range :: Array Int (Ratio Int) -> Int -> Int -> [Ratio Int]
    lookup_range array from too
      = if (from > too)
	then []
	else (array ! from) : (lookup_range array (from + 1) too)
