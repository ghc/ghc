{-
	convection term

	XZ, 24/10/91
-}

{-
	Modified to employ S_array.
	The way in which the c_mat is constructed is also changed.
	The new implementation is more efficient.

	XZ, 7/2/92
-}

{-
	Removed a bug

	XZ, 25/2/92
-}

module C_matrix ( c_mat ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto
import Asb_routs

-----------------------------------------------------------
-- Element convection matrix.                            --
-- Used in "c_mat".                                      --
-----------------------------------------------------------

cc_list ::
	[ 
		(
			[Frac_type],
			[Frac_type],
			[Frac_type],
			[(Frac_type,Frac_type)],
			[(Frac_type,Frac_type)],
			[(Frac_type,Frac_type)]
		)
	]
cc_list =
	[
		select [a11,a12,a13,a14,a15,a16],
		select [a12,a22,a23,a24,a25,a26],
		select [a13,a23,a33,a34,a35,a36],
		select [a14,a24,a34,a44,a45,a46],
		select [a15,a25,a35,a45,a55,a56],
		select [a16,a26,a36,a46,a56,a66]
	]
	where
	select = \tup_list ->
		(
			map select1 tup_list,
			map select2 tup_list,
			map select3 tup_list,
			map select4 tup_list,
			map select5 tup_list,
			map select6 tup_list
		)
	select1 = \(x,_,_,_,_,_) -> x
	select2 = \(_,x,_,_,_,_) -> x
	select3 = \(_,_,x,_,_,_) -> x
	select4 = \(_,_,_,x,_,_) -> x
	select5 = \(_,_,_,_,x,_) -> x
	select6 = \(_,_,_,_,_,x) -> x

c4 = 4 :: Frac_type
c9 = (-9) :: Frac_type
c11 = 11 :: Frac_type
c12 = 12 :: Frac_type
c16 = (-16) :: Frac_type
c18 = (-18) :: Frac_type
c20 = (-20) :: Frac_type
c24 = 24 :: Frac_type
c32 = (-32) :: Frac_type
c48 = (-48) :: Frac_type
c78 = 78 :: Frac_type
c80 = 80 :: Frac_type
c96 = (-96) :: Frac_type
c120 = 120 :: Frac_type
c128 = 128 :: Frac_type
c160 = 160 :: Frac_type
c192 = 192 :: Frac_type
c384 = 384 :: Frac_type

a11 = (c78,c18,c18,(c24,c24),(c24,c120),(c24,c120))

a12 = (c9,c9,c11,(c4,c16),(c4,c16),(c16,c16))

a13 = (c9,c11,c9,(c16,c4),(c16,c16),(c4,c16))

a14 = (c12,c20,c20,(c48,c48),(c48,c16),(c48,c16))

a15 = ((-c48),c16,c32,(c32,c16),(c32,(-c48)),(c16,(-c48)))

a16 = ((-c48),c32,c16,(c16,c32),(c16,(-c48)),(c32,(-c48)))

a22 = (c18,c78,c18,(c24,c120),(c24,c24),(c120,c24))

a23 = (c11,c9,c9,(c16,c16),(c16,c4),(c16,c4))

a24 = (c16,(-c48),c32,(c32,(-c48)),(c32,c16),((-c48),c16))

a25 = (c20,c12,c20,(c48,c16),(c48,c48),(c16,c48))

a26 = (c32,(-c48),c16,(c16,(-c48)),(c16,c32),((-c48),c32))

a33 = (c18,c18,c78,(c120,c24),(c120,c24),(c24,c24))

a34 = (c16,c32,(-c48),((-c48),c32),((-c48),c16),(c32,c16))

a35 = (c32,c16,(-c48),((-c48),c16),((-c48),c32),(c16,c32))

a36 = (c20,c20,c12,(c16,c48),(c16,c48),(c48,c48))

a44 = (c96,c160,c160,(c384,c384),(c384,c128),(c384,c128))

a45 = ((-c16),(-c16),c80,(c192,c128),(c192,c128),(c128,c128))

a46 = ((-c16),c80,(-c16),(c128,c192),(c128,c128),(c192,c128))

a55 = (c160,c96,c160,(c384,c128),(c384,c384),(c128,c384))

a56 = (c80,(-c16),(-c16),(c128,c128),(c128,c192),(c128,c192))

a66 = (c160,c160,c96,(c128,c384),(c128,c384),(c384,c384))

-----------------------------------------------------------
-- Converction term.                                     --
-- Used in "get_rh1".                                    --
-- Calls "cc_mat".                                        --
-----------------------------------------------------------

c_mat
	:: My_Array Int (((Frac_type,Frac_type,Frac_type),
		(Frac_type,Frac_type,Frac_type))
	-> ([Frac_type],[Frac_type]) -> [Frac_type])

c_mat = -- parameter: element_factors velocities
	s_listArray bnds
	( map
		(
			\ (col1,col2,col3,col4,col5,col6) ->

			( \ (gd1,gd2,gd3) u ->
			     map (\ col->list_match_prod col u)
			     [
				     map ((*) gd1) col1,
				     map ((*) gd2) col2,
				     map ((*) gd3) col3,
				     map (f' gd2 gd3) col4,
				     map (f' gd1 gd3) col5,
				     map (f' gd1 gd2) col6
			     ] ) `bindTo` ( \ f ->

			(\ (y1,y2) (u1,u2) -> zipWith (+) (f y1 u1) (f y2 u2)) )
		) cc_list
	)
	where
	f' = \g1 g2 x -> (fst x) * g1 + (snd x) * g2
	bnds = (1,v_nodel)
    	bindTo x k = k x -- essentially Haskell 1.0 "let"
