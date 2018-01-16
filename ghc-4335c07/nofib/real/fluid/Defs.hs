{-
	Constant definition module.  Used every where.

	XZ, 24/10/91
-}

{-
	Modified to define My_Array

	XZ, 19/2/92
-}

module Defs (
	Frac_type, My_Array, v_nodel, p_nodel,
	large_scalor, small, large
	) where

import S_Array
import Norm	-- not needed w/ proper module handling

type Frac_type = Float
type My_Array a b = S_array b

-----------------------------------------------------------
-- number of velocity nodes in an element                --
-----------------------------------------------------------

v_nodel = 6 :: Int

-----------------------------------------------------------
-- number of pressure nodes in an element                --
-----------------------------------------------------------

p_nodel = 3 :: Int

large_scalor = 1.0e20 :: Frac_type

large = 1.0e20 :: Frac_type

small = 1.0e-40 :: Frac_type
