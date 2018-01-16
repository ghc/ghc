{-
	Some subroutines

	XZ, 24/10/91
-}

{-
	Modified to employ S_array.

	XZ, 7/2/92
-}

module Asb_routs
	( get_asb_table, get_val, list_inner_prod, list_match_prod, add_mat, add_u, mult )
	where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import Data.Ix

-----------------------------------------------------------
-- Generating a lookup table for assembling a system     --
-- matrix using a corresponging steering vector.         --
-- The result is an array of tuple list.                 --
-- The ith entry of the array contains all elements      --
-- which have node i on their edges.                     --
-- The 1st item of the tuples is the element identity    --
-- and the 2nd is the node local number.                 --
-- Called at the data setup stage.                       --
-----------------------------------------------------------

get_asb_table :: Int -> Int -> Int -> (My_Array Int [Int]) ->
	(My_Array Int [(Int,Int)])

get_asb_table total e_total nodel steer =
	s_accumArray (++) [] (1,total)
	(
		concat
		[ zipWith f1 (steer!^e) (map (\z->[(e,z)]) range_nodel)
			| e <- range (1,e_total)
		]
	)
	where
	range_nodel = range (1,nodel)
	f1 = \x y->(x,y)

-----------------------------------------------------------
-- syntaxes for generating velocity and pressure         --
-- assembling table:                                     --
-- v_asb_table =                                         --
--      get_asb_table n_total e_total v_nodel v_steer    --
-- p_asb_table =                                         --
--      get_asb_table p_total e_total p_nodel p_steer    --
-- Selecting some values from an array and putting them  --
-- into a list.  Used mainly for assembling RHS and      --
-- Jacobi iteration.                                     --
-----------------------------------------------------------

get_val :: (My_Array Int Frac_type) -> [Int] -> [Frac_type]
get_val arr steer = [arr!^n|n<-steer]

-----------------------------------------------------------
-- Inner-production of 2 list vectors.  Used mainly for  --
-- assembling RHS, Choleski decomposition and Jacobi     --
-- iteration.                                            --
--   Two versions: 1: lazy;                              --
--                 2: 2nd arg forced, possibly save      --
--                    some calculation.                  --
-----------------------------------------------------------

list_inner_prod :: [Frac_type] -> [Frac_type] -> Frac_type
list_inner_prod = \x y -> sum (zipWith (*) x y)

list_match_prod :: [Frac_type] -> [Frac_type] -> Frac_type
list_match_prod =
	\x y -> sum (zipWith mult x y)

-----------------------------------------------------------
-- modified (*): check first if the 2nd arg is 0         --
-----------------------------------------------------------

mult _ 0 = 0
mult x y = x * y

-----------------------------------------------------------
-- adding 2 vectors.  Used mainly in the TG iteration.   --
-----------------------------------------------------------

add_mat
	:: (My_Array Int Frac_type) -> (My_Array Int Frac_type)
	-> (My_Array Int Frac_type)
add_mat a b =
	s_listArray (s_bounds a) (zipWith (+) (s_elems a) (s_elems b))

-----------------------------------------------------------
-- Adding 2 vector pairs.  Used in TG iteration and      --
-- Jacobi iteration.                                     --
-----------------------------------------------------------

add_u
	:: (My_Array Int Frac_type,My_Array Int Frac_type)
	-> (My_Array Int Frac_type,My_Array Int Frac_type)
	-> (My_Array Int Frac_type,My_Array Int Frac_type)
add_u = \ a b ->
	(
		add_mat (fst a) (fst b),
		add_mat (snd a) (snd b)
	)
