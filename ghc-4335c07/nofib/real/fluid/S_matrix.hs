{-
	Diffusion matrix

	XZ, 24/10/91
-}

{-
	Modified to adopt S_array

	The way in which the matrix is constructed has been
	changed.

	XZ, 19/2/92
-}

module S_matrix ( s_mat ) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto

-----------------------------------------------------------
-- Diffusion matrix.                                     --
-- Used in assembling rh1.                               --
-- Parameters:                                           --
--   gdij : jth entry of ith element factor component    --
-----------------------------------------------------------

s_mat
	:: My_Array Int (((Frac_type,Frac_type,Frac_type),
			(Frac_type,Frac_type,Frac_type)) -> [Frac_type])
s_mat =
	s_listArray (1,v_nodel)
	[
		\u -> cons u [f11,f12,f13,f0,f15,f16],
		\u -> cons u [f12,f22,f23,f24,f0,f16],
		\u -> cons u [f13,f23,f33,f24,f15,f0],
		\u -> cons u [f0,f24,f24,f44,f45,f46],
		\u -> cons u [f15,f0,f15,f45,f55,f56],
		\u -> cons u [f16,f16,f0,f46,f56,f66]
	]
	where
	s1 = \(x,_,_) -> x
	s2 = \(_,y,_) -> y
	s3 = \(_,_,z) -> z
	ff1 = \x y u v -> x*y+u*v
	ff2 = \x y u v -> (ff2' x y) + (ff2' u v)
		where ff2' = \x y -> x*(x+y)+y*y
	ff3 = \x y z u v w -> (ff3' x y z) + (ff3' u v w)
		where ff3' = \x y z -> x*y+(x+z)*(y+z)
	cons = \u -> map (\f->f u)
	f0 = \x -> 0
	f11 (x,y) = 3 * ( ff1 c1 c1 c2 c2 )
		where
		c1 = s1 x
		c2 = s1 y
	f12 = \(x,y) -> - ( ff1 (s1 x) (s2 x) (s1 y) (s2 y) )
	f13 = \(x,y) -> - ( ff1 (s1 x) (s3 x) (s1 y) (s3 y) )
	f15 = \x -> (-4) * (f13 x)
	f16 = \x -> (-4) * (f12 x)
	f22 (x,y) = 3 * ( ff1 c1 c1 c2 c2 )
		where
		c1 = s2 x
		c2 = s2 y
	f23 = \(x,y) -> - ( ff1 (s2 x) (s3 x) (s2 y) (s3 y) )
	f24 = \x -> (-4) * (f23 x)
	f33 (x,y) = 3 * ( ff1 c1 c1 c2 c2 )
		where
		c1 = s3 x
		c2 = s3 y
	f44 = \(x,y) -> 8 * ( ff2 (s2 x) (s3 x) (s2 y) (s3 y) )
	f45 = \(x,y)->4*(ff3 (s1 x) (s2 x) (s3 x) (s1 y) (s2 y) (s3 y))
	f46 = \(x,y)->4*(ff3 (s1 x) (s3 x) (s2 x) (s1 y) (s3 y) (s2 y))
	f55 = \(x,y) -> 8 * ( ff2 (s1 x) (s3 x) (s1 y) (s3 y) )
	f56 = \(x,y)->4*(ff3 (s2 x) (s3 x) (s1 x) (s2 y) (s3 y) (s1 y))
	f66 = \(x,y) -> 8 * ( ff2 (s1 x) (s2 x) (s1 y) (s2 y) )
