module Data8 where

import Defs
import S_Array
import Quad_def

mon :: Bool
mon = False
simpl :: Bool
simpl = True
m_iter :: Int
m_iter = 10
m_toler :: Frac_type
m_toler = 1.00000000e-6
max_jcb_iter:: Int
max_jcb_iter = 3
jcb_toler :: Frac_type
jcb_toler = 9.99999978e-3
relax :: Frac_type
relax = 1.00000000
dlt_t:: Frac_type
dlt_t = 9.99999978e-3
n_total :: Int
n_total = 25
p_total :: Int
p_total = 9
coord :: () -> My_Array Int (Frac_type, Frac_type)
coord () = Mk_t_Array (1, 25) Nothing 
	(let
	t1111 = (Fork 1 (Leaf (0.00000000, 0.00000000)) (Leaf (1.00000000, 0.00000000)
		))
	t1112 = (Fork 1 (Leaf (2.00000000, 0.00000000)) (Leaf (2.00000000, 5.00000000e-1)
		))
	t1121 = (Fork 1 (Leaf (1.00000000, 5.00000000e-1))
		 (Leaf (0.00000000, 5.00000000e-1)))
	t1122 = (Fork 1 (Leaf (0.00000000, 1.00000000)) (Leaf (1.00000000, 1.00000000)
		))
	t1211 = (Fork 1 (Leaf (2.00000000, 1.00000000)) (Leaf (5.00000000e-1, 0.00000000)
		))
	t1212 = (Fork 1 (Leaf (1.50000000, 0.00000000)) (Leaf (2.00000000, 2.50000000e-1)
		))
	t1221 = (Fork 1 (Leaf (1.50000000, 2.50000000e-1))
		 (Leaf (1.00000000, 2.50000000e-1)))
	t1222 = (Fork 1 (Leaf (5.00000000e-1, 2.50000000e-1)
		) (Leaf (0.00000000, 2.50000000e-1)))
	t2111 = (Fork 1 (Leaf (5.00000000e-1, 5.00000000e-1)
		) (Leaf (1.50000000, 5.00000000e-1)))
	t2112 = (Fork 1 (Leaf (2.00000000, 7.50000000e-1))
		 (Leaf (1.50000000, 7.50000000e-1)))
	t2121 = (Fork 1 (Leaf (1.00000000, 7.50000000e-1))
		 (Leaf (5.00000000e-1, 7.50000000e-1)))
	t2122 = (Fork 1 (Leaf (0.00000000, 7.50000000e-1))
		 (Leaf (5.00000000e-1, 1.00000000)))
	t2211 = (Fork 1 (Leaf (1.50000000, 1.00000000)) (Null )
		)
	t2212 = (Null )

	in
	(Fork 16 (Fork 8 (Fork 4 (Fork 2 t1111 t1112) (Fork 2 t1121 t1122)
		) (Fork 4 (Fork 2 t1211 t1212) (Fork 2 t1221 t1222)
		)) (Fork 8 (Fork 4 (Fork 2 t2111 t2112) (Fork 2 t2121 t2122)
		) (Fork 4 (Fork 2 t2211 t2212) (Null ))))	)
v_steer :: () -> [[Int]]
v_steer () = [[1, 2, 5, 14, 15, 10], [2, 3, 4, 12, 13, 11], [1, 5, 6, 17, 16, 15], [2, 4, 5, 18, 14, 13], [6, 5, 8, 21, 22, 17], [5, 4, 9, 19, 20, 18], [6, 8, 7, 24, 23, 22], [5, 9, 8, 25, 21, 20]]
bry_nodes :: () -> ([Bool], ([Bool], [Bool]))
bry_nodes () = ([True, True, True, True, False, True, True, True, True, True, True, True, False, False, False, True, False, False, True, False, False, False, True, True, True], ([True, True, True, False, False, True, True, True, True, True, True, False, False, False, False, True, False, False, False, False, False, False, True, True, True], [True, True, True, True, False, True, True, True, True, True, True, True, False, False, False, True, False, False, True, False, False, False, True, True, True])
		)
p_fixed :: [Int]
p_fixed = [4]
init_vec :: () -> (My_Array Int Frac_type, My_Array Int (Frac_type, Frac_type)
		)
init_vec () = (Mk_t_Array (1, 9) Nothing 
	(let
	t1111 = (Leaf 0.00000000)
	t1112 = (Leaf 0.00000000)
	t1121 = (Leaf 0.00000000)
	t1122 = (Leaf 0.00000000)
	t1211 = (Leaf 0.00000000)
	t1212 = (Leaf 0.00000000)
	t1221 = (Leaf 0.00000000)
	t1222 = (Leaf 0.00000000)
	t2111 = (Leaf 0.00000000)
	t2112 = (Null )

	in
	(Fork 8 (Fork 4 (Fork 2 (Fork 1 t1111 t1112) (Fork 1 t1121 t1122)
		) (Fork 2 (Fork 1 t1211 t1212) (Fork 1 t1221 t1222)
		)) (Fork 4 (Fork 2 (Fork 1 t2111 t2112) (Null )) (Null )
		))	), Mk_t_Array (1, 25) (Just (0.00000000, 0.00000000)
		)
	(let
	t1121 = (Fork 1 (Null ) (Leaf (1.00000000, 0.00000000)
		))
	t1122 = (Null )
	t1221 = (Null )
	t1222 = (Fork 1 (Null ) (Leaf (7.50000000e-1, 0.00000000)
		))
	t2121 = (Null )
	t2122 = (Fork 1 (Leaf (7.50000000e-1, 0.00000000))
		 (Null ))

	in
	(Fork 16 (Fork 8 (Fork 4 (Null ) (Fork 2 t1121 t1122)
		) (Fork 4 (Null ) (Fork 2 t1221 t1222))) (Fork 8 (Fork 4 (Null )
		 (Fork 2 t2121 t2122)) (Null )))	))
tri_fac :: () -> TriMat Frac_type
tri_fac () = 
	(TriM (TriM (TriM (TriM (SingTM 1.11803401 ) (SingM (-2.23606795e-1)
		 ) (SingTM 1.56524765 )) (RectM (ZeroM ) (SingM (-1.59719139e-1)
		 ) (ZeroM ) (ZeroM )) (TriM (SingTM 1.10656667 ) (SingM (-9.03696120e-1)
		 ) (SingTM 1.58113884e10 ))) (RectM (RectM (ZeroM )
		 (SingM (-1.27775311) ) (SingM (-8.94427180e-1) ) (SingM (-1.27775297e-1)
		 )) (RectM (SingM (-1.84427768e-1) ) (SingM (-4.21636996e-11)
		 ) (SingM (-1.84427761e-2) ) (SingM (-1.05409247e-12)
		 )) (ZeroM ) (ZeroM )) (TriM (TriM (SingTM 1.82574189 )
		 (SingM (-3.65148336e-1) ) (SingTM 1.24498999 )) (RectM (ZeroM )
		 (SingM (-8.03219318e-1) ) (SingM (-1.09544516) ) (SingM (-3.21287692e-1)
		 )) (TriM (SingTM 7.77713776e-1 ) (SingM (-6.53279543e-1)
		 ) (SingTM 8.77496421e-1 )))) (RectM (RectM (ZeroM )
		 (RectM (ZeroM ) (SingM (-6.32455546e-11) ) (ZeroM )
		 (ZeroM )) (ZeroM ) (ZeroM )) (RectM (RectM (SingM (-1.46059336e-21)
		 ) (SingM (-4.81931510e-22) ) (ZeroM ) (ZeroM )) (RectM (SingM (-4.97736698e-22)
		 ) (SingM (-2.84901440e-1) ) (ZeroM ) (ZeroM )) (ZeroM )
		 (ZeroM )) (ZeroM ) (ZeroM )) (TriM (TriM (TriM (SingTM 1.08112490 )
		 (ZeroM ) (ZeroTM )) (ZeroM ) (ZeroTM )) (ZeroM ) (ZeroTM )
		))
