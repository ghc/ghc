{-
	Solution error computation for both Jacobi and main
	iterations

	XZ 25/2/92
-}

module Tol_cal (tol_cal) where

import Defs
import S_Array	-- not needed w/ proper module handling
import Norm	-- ditto

tol_cal :: [Frac_type] -> [Frac_type] -> Bool -> Frac_type
tol_cal x dif jcb_itn =
	case (x_abs_max<small) || (x_sq_sum<small) of
		True      ->
			if jcb_itn
			then error "Jacobi iteration: solution too small!"
			else error "main iteration: solution too small!"
		otherwise ->
			if jcb_itn || (x_abs_max>=(1::Frac_type))
			then sqrt ( sq_sum_abs_dif/x_sq_sum )
			else sqrt ( sq_sum_abs_dif )
	where
	sq_sum_abs_dif = sq_sum abs_dif
	x_abs_max = maximum (map abs x)
	x_sq_sum = sq_sum x
	abs_dif = map abs dif
	sq_sum = \y -> sum (map (\x->x*x) y)
