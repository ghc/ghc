-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module ElecField (elecField) where

import	PicType
import	Consts
import Data.Array

-- Phase III: Calculate electric fields
-- the x and y components of the electric field are approximated
-- by the first partial difference in each dimension

elecField :: Phi -> Electric
elecField phi =
	(array ((0,0), (n,n))
	([((i,j) , (phi!(i-1,j) - phi!(i,j)))
			| i <- [1..n], j <- [0..n]]++
	 [((0,j) , (phi!(n,j) - phi!(0,j)))
			| j <- [0..n]]),

	array ((0,0), (n,n))
	([((i,j) , (phi!(i,j+1) - phi!(i,j)))
			| i <- [0..n], j <- [0..(n-1)]]++
	 [((i,n) , (phi!(i,0) - phi!(i,n)) )
			| i <- [0..n]]))
	where
	    n = nCell-1
