module LinearAlgebra where

import Data.Array
import Data.List

import Types
	
apply :: Num a => Matrix a -> Vector a -> Vector a
apply m v		| cm == cv	= accumArray (+) 0 (1, rm) [(r, v ! c * m ! (r, c)) | (r, c) <- indices m]
			| otherwise	= error "apply: matrix and vector dimensions are not compatible"
  where	
 	((1, 1), (rm, cm))		= bounds m
	(    1 ,      cv )		= bounds v

m_mul :: Num a => Matrix a -> Matrix a -> Matrix a
m_mul a b		| ca == rb	= accumArray (+) 0 ((1, 1), (ra, cb)) [((r, c), a ! (r, t) * b ! (t, c)) | r <- [1..ra], t <- [1..ca], c <- [1..cb]]
			| otherwise	= error "m_mul: matrix dimensions are not compatible"
  where
  	((1, 1), (ra, ca))		= bounds a
	((1, 1), (rb, cb))		= bounds b

m_add :: Num a => Matrix a -> Matrix a -> Matrix a
m_add 					= m_zipWith (+)

m_sub :: Num a => Matrix a -> Matrix a -> Matrix a
m_sub		 			= m_zipWith (-)

m_map :: (a -> b) -> Matrix a -> Matrix b
m_map f a				= listArray (bounds a) (map f (elems a))

m_zipWith :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
m_zipWith f a b		| compatible	= listArray (bounds a) (zipWith f (elems a) (elems b))
			| otherwise	= error "m_zipWith: matrix dimensions are not compatible"
  where
  	compatible			= bounds a == bounds b

v_add :: Num a => Vector a -> Vector a -> Vector a
v_add					= v_zipWith (+)

v_sub :: Num a => Vector a -> Vector a -> Vector a
v_sub					= v_zipWith (-)

v_map :: (a -> b) -> Vector a -> Vector b
v_map f a				= listArray (bounds a) (map f (elems a))

v_zipWith f a b		| compatible	= listArray (bounds a) (zipWith f (elems a) (elems b))
			| otherwise	= error "v_zipWith: vector dimensions are not compatible"
  where
  	compatible			= bounds a == bounds b

m_transpose :: Matrix a -> Matrix a
m_transpose m 				= let ((1, 1), (r, c)) = bounds m in array ((1, 1), (c, r)) [((c, r), v) | ((r, c), v) <- assocs m]

m_is_square :: Matrix a -> Bool
m_is_square m 				= let ((1, 1), (r, c)) = bounds m in r == c

m_zero :: Num a => Index -> Matrix a
m_zero s				= accumArray (+) 0 ((1, 1), (s, s)) []

m_unit :: Num a => Index -> Matrix a
m_unit s 				= accumArray (+) 0 ((1, 1), (s, s)) [((i, i), 1) | i <- [1 .. s]]

nullspace :: Matrix Exact -> Matrix Exact
nullspace 				= m_transpose . left_nullspace . m_transpose

left_nullspace :: Matrix Exact -> Matrix Exact
left_nullspace m			= let (rows, _, i) = gauss_jordan m in m_select_rows rows i

m_select_rows :: [Index] -> Matrix a -> Matrix a
m_select_rows rows matrix		= listArray ((1, 1), (length rows, size)) [matrix ! (r, c) | r <- rows, c <- [1 .. size]]
  where
	size				= (snd . snd . bounds) matrix

m_inv :: Matrix Exact -> Matrix Exact
m_inv m			| l == []	= i
			| otherwise	= error "m_inv: matrix isn't invertible"
  where
  	(l, u, i)			= gauss_jordan m
  	((1,1), (r, c))			= bounds m

gauss_jordan :: Matrix Exact -> ([Index], Matrix Exact, Matrix Exact)
gauss_jordan m		| m_is_square m	= (foldr1 (.) [step c | c <- reverse [1 .. size]]) ([1 .. size], m, m_unit size)
			| otherwise	= error "gauss_jordan: not a square matrix"
  where
  	step c (rs, m0, i0)		= if v /= 0 then (delete c rs, m2, i2) else (rs, m0, i0)
	  where
		(m2, i2)		= (sweep     m1, sweep     i1)
		(m1, i1)		= (swap_norm m0, swap_norm i0)
		swap_norm		= (multiply c (1 / v)) . (if r /= c then swap r c else id)
	  	sweep			= eliminate c m1
	  	(r, v)			= pivot c rs m0
	
	pivot c rs m0			= foldl1 max' [(r, m0 ! (r, c)) | r <- rs]
	max' (r1, v1) (r2, v2)		= if (abs(v1) >= abs(v2)) then (r1, v1) else (r2, v2)
	
	swap      r s  m		= m // concat ([[((r, c), m ! (s, c)),      ((s, c), m ! (r, c))] | c <- [1 .. size]])
	multiply  r f  m		= m //         [ ((r, c),              f           * m ! (r, c))  | c <- [1 .. size]]
	eliminate w m1 m		= m //         [ ((r, c), m ! (r, c) - m1 ! (r, w) * m ! (w, c))  | r <- [1 .. size], r /= w, c <- [1 .. size]]

	size				= (snd . snd . bounds) m
