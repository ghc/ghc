-- Glasgow Haskell 0.403 : BLAS ( Basic Linear Algebra System )
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : vector.hs              DATE : 4-3-1991                 *
-- *                                                                    *
-- * CONTENTS : Vector datatype and operations implemented by using     *
-- *            array type of Haskell.                                  *
-- *            Vectors are index from 1 to N, where N is the dimension *
-- *		of the vector, ie the number of elements in the vector. *
-- **********************************************************************

-- Haskell Arrays:
--
--	Haskell provides indexable arrays, which may be thought of as 
--	functions whose domains are isomorphic to contiguous subsets of 
-- 	the integers. Such a restricted class of functions are intended
--	to be very efficiently implementable; in particular, the programmer
--	has a reasonable expectation of rapid access to the components. 
-- 	To ensure the possibility of such an implementation, arrays are 
--	treated, not as general fucntions, but as data.

--	Data of array type is    Array i e , where i is index type and e
-- 	is element type.

-- 	Index classes of Haskell arrays:
--      ================================
--
--	For an one dimension array, its index is of Int type, Char type,etc.
--	While for a two dimension array, its index is of pair type.
--
--	Index must be within the bound of that array. The bound of an one
--	dimension array is a pair which gives the lower and upper bound
--	of its index value.
--
--	Function ( inRange :: (a,a) -> a -> Bool )  checks if an index is
--	in the range of the bound. Function ( range :: (a,a) -> [a] ) returns
--	a list of all indices in this range.

--	Array construction operations(functions):
--      =========================================
--
--	array 	:: (Ix a) => (a,a) -> [Assoc a b] -> Array a b
--      data Assoc a b = a := b
--
--	(!)	:: (Ix a) => Array a b -> a -> b
--	bounds  :: (Ix a) => Array a b -> (a,a)
--      assocs	:: (Ix a) => (Array a b) -> [Assoc a b]
--	indices :: (Ix a) => (Array a b) -> [a]
--	rangesize :: (Ix a) => (a, a) -> Int
--
--	Array accumulating operations(functions):
--      =========================================
--
--	accumArray :: (Ix a) => (b->c->b) -> b -> (a,a) -> [Assoc a c]
--		                -> Array a b
--	accumArray "accumulating function"  "initial value" 
--	           "bounds" "association list"

--	Array increment update operations(functions):
--      =============================================
--
--	(//)	:: (Ix a) => Array a b -> Assoc a b -> Array a b
--		It takes an array and an Assoc pair and returns an array
--		identical to the first argument except for the one element
--		specified by the second argument.
--
--	accum	:: (Ix a) => (b->c->b) -> Array a b -> [Assoc a c] -> Array a b
--	accum "accumulating function" "array to be updated" "association list"

--	Other operations(functions):
--      ============================
--
--	listArray :: (Ix a) => (a, a) -> [b] -> Array a b
--	elems :: (Ix a) => (Array a a) -> [a]
--
--	amap :: (Ix a) => (b->c) -> Array a b -> Array a c
--      ixmap :: (Ix a, Ix b) => (b,b) -> (b->a) -> Array a c -> Array b c


module Vector(Vec, makevec, boundvec, vecsub, incrvec, updvec, maxupdvec,
              vecprod, displayvec) where

import Data.Array
import Basics

data Vec a = VEC Int (Array Int a)

displayvec :: (Show a) => Vec a -> [Char]

vecprod :: (Num a) => Vec a -> Vec a -> a

updvec  :: Vec a -> [(Int,a)] -> Vec a

maxupdvec  :: (Num a, Ord a) => Vec a -> [(Int,a)] -> Vec a

incrvec :: (Num a) => Vec a -> [(Int,a)] -> Vec a

vecsub  :: Vec a -> Int -> a

boundvec :: Vec a -> Int

makevec :: Int -> (Int -> a) -> Vec a

makevec n f = VEC n (array (1,n) [ (i,f i) | i <- [1..n] ])

boundvec (VEC n _) = n 

vecsub (VEC n va) i = va ! i

updvec (VEC n va) s =
  VEC n (accum f va s)
 where
  f b c = c

maxupdvec (VEC n va) s = VEC n (accum max va s)

incrvec (VEC n va) s = VEC n (accum (+) va s)

vecprod v1 v2 = 
  sum [(vecsub v1 i) * (vecsub v2 i) | i <- [1..n] ]
 where 
  n = boundvec v1

displayvec v =
	"< " ++ 
	concat ([(showrj 8 (vecsub v i) ) | i<- [1..n] ] ) ++ 
	">\n"
	where
	n = boundvec v

