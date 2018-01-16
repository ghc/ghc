module Defs where

import S_Array
import Parallel

type Frac_type = Float

-----------------------------------------------------------
-- number of velocity nodes in an element                --
-----------------------------------------------------------

v_nodel = 6 :: Int

-----------------------------------------------------------
-- number of pressure nodes in an element                --
-----------------------------------------------------------

p_nodel = 3 :: Int

-----------------------------------------------------------
-- Data types for node network                           --
-----------------------------------------------------------

type Node_Lists = (My_Array Int P_Node, My_Array Int V_Node)

type P_Node = ([(Elem,Int)],Bool)

type V_Node =
	([(Elem,Int)],((Frac_type,Frac_type),(Bool,(Bool,Bool))))

type Triple_F = (Frac_type,Frac_type,Frac_type)

type Det_Fac = (Frac_type,(Triple_F,Triple_F))

type Elem = ([Int],Det_Fac)

get_val :: (My_Array Int a) -> [Int] -> [a]
get_val arr steer = [arr!^n|n<-steer]

-----------------------------------------------------------
-- Inner-production of 2 list vectors.  Used mainly for  --
-- assembling RHS, Choleski decomposition and Jacobi     --
-- iteration.                                            --
-----------------------------------------------------------

list_inner_prod :: [Frac_type] -> [Frac_type] -> Frac_type
list_inner_prod x y = sum (zipWith (*) x y)

-----------------------------------------------------------
-- adding 2 vectors.  Used mainly in the TG iteration.   --
-----------------------------------------------------------

add_p
	:: (My_Array Int Frac_type) -> (My_Array Int Frac_type)
	-> (My_Array Int Frac_type)
add_p = arr_merg (+)

-----------------------------------------------------------
-- Adding 2 vector pairs.  Used in TG iteration and      --
-- Jacobi iteration.                                     --
-----------------------------------------------------------

add_u
	:: (My_Array Int (Frac_type,Frac_type))
	-> (My_Array Int (Frac_type,Frac_type))
	-> (My_Array Int (Frac_type,Frac_type))
add_u = arr_merg (\(x,y) (u,v)->(x+u,y+v))

assemble :: (Normal a, Normal d) => ([a] -> c -> d) -> (b -> a) -> S_array ([b], c) -> S_array d

assemble fn fe =
	s_amap ( \(asb,rest)->
	let l = map fe asb in
	  (normal l) `par` (fn l rest))		-- FOR PARALLEL VERSION
	
