-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module Utils (applyOpToMesh, coarseMesh, fineMesh, genRand, log2) where

import	PicType
import Data.Array
infix 1 =:
(=:) a b = (a,b)

-- apply the given operator to a mesh of given size
-- operator is applied to the position and to the 8 surrounding positions
-- so value(i,j) is decided by (i-1,j-1), (i,j-1), (i+1,j-1), etc.
-- corners and edges are handled as if the mesh was a torus

applyOpToMesh :: (Mesh -> Range -> Value) -> Mesh -> Indx -> Mesh
applyOpToMesh operator mesh n' =
	array ((0,0), (n,n))

	(-- corners (nw, ne, sw, se)
	[(0,0) =: operator mesh [n,  0, 1,  n, 0, 1]] ++
	[(0,n) =: operator mesh [n,  0, 1, n1, n, 0]] ++
	[(n,0) =: operator mesh [n1, n, 0,  n, 0, 1]] ++
	[(n,n) =: operator mesh [n1, n, 0, n1, n, 0]] ++

	-- edges (top row, bottom row, left column, right column)
	[(0,j) =: operator mesh [n, 0, n1, (j-1), j, (j+1)] | j<-[1..n1]] ++
	[(n,j) =: operator mesh [n1, n, 0, (j-1), j, (j+1)] | j<-[1..n1]] ++
	[(i,0) =: operator mesh [(i-1), i, (i+1), n,  0, 1] | i<-[1..n1]] ++
	[(i,n) =: operator mesh [(i-1), i, (i+1), n1, n, 0] | i<-[1..n1]] ++

	-- internal
	[(i,j) =: operator mesh [(i-1), i, (i+1), (j-1), j, (j+1)]
					| i <- [1..n1], j <- [1..n1]])
	where
	    n = n'-1
	    n1 = n'-2


-- project a mesh onto a mesh of half the rank
-- a  b  c  d  e  f		a  c  e
-- g  h  i  j  k  l	=>	m  o  q
-- m  n  o  p  q  r		y  0  2
-- s  t  u  v  w  x
-- y  z  0  1  2  3
-- 4  5  6  7  8  9

coarseMesh :: Mesh -> Indx -> Mesh
coarseMesh mesh n =
	array ((0,0), (nHalf,nHalf))
		[(i,j) =: mesh!(i*2, j*2) | i <- [0..nHalf], j <- [0..nHalf]]
	where
	    nHalf = n `div` 2 - 1


-- interpolate a mesh of half rank onto a full mesh
-- values aren't just copied but are a function of the letter shown
-- a  b  c  d  e  f		A  B  C
-- g  h  i  j  k  l	<=	D  E  F
-- m  n  o  p  q  r		G  H  I
-- s  t  u  v  w  x
-- y  z  0  1  2  3
-- 4  5  6  7  8  9
--
-- a = A, c = B, e = C, m = D, o = E, q = F, y = G, 0 = H, 2 = I
-- g = .5(A+D), i = .5(B+E), k = .5(C+F), b = .5(A+B), d = .5(B+C), f = .5(C+A)
-- s = .5(D+G), u = .5(E+H), w = .5(F+I), n = .5(D+E), p = .5(E+F), r = .5(F+D)
-- 4 = .5(G+A), 6 = .5(H+B), 8 = .5(I+C), z = .5(G+H), 1 = .5(H+I), 3 = .5(I+G)
-- h = .25(A+B+D+E), j = .25(B+C+E+F), l = .25(C+A+F+D) ...

fineMesh :: Mesh -> Indx -> Mesh
fineMesh mesh nHalf' =
	array ((0,0), (n,n))

	(-- corners (nw, ne, sw, se)
	[(0,0)	=: 3] ++
	[(0,n)	=: 3] ++
	[(n,0)	=: 3] ++
	[(n,n)	=: 3] ++

	-- edges (north, south)
	[(0,2*j)	=: 4| j<-[1..nHalf]] ++
	[(0,2*j-1)	=: 4| j<-[1..nHalf]] ++
	[(n,2*j)	=: 4| j<-[1..nHalf]] ++
	[(n,2*j-1)	=: 4| j<-[1..nHalf]] ++

	-- edges (west, east)
	[(2*i,0)	=: 5| i<-[1..nHalf]] ++
	[(2*i-1,0)	=: 5| i<-[1..nHalf]] ++
	[(2*i,n)	=: 5| i<-[1..nHalf]] ++
	[(2*i-1,n)	=: 5| i<-[1..nHalf]] ++

	-- interior
	[(2*i,2*j)	=: 6| i<-[1..nHalf], j<-[1..nHalf]] ++
	[(2*i,2*j-1)	=: 6| i<-[1..nHalf], j<-[1..nHalf]] ++
	[(2*i-1,2*j)	=: 6| i<-[1..nHalf], j<-[1..nHalf]] ++
	[(2*i-1,2*j-1)	=: 6| i<-[1..nHalf], j<-[1..nHalf]])
{-
	array ((0,0), (n,n))

	(-- corners (nw, ne, sw, se)
	[(0,0)	=: mesh!(0,0)] ++
	[(0,n)	=: 0.5*(mesh!(0,0) + mesh!(0,nHalf))] ++
	[(n,0)	=: 0.5*(mesh!(0,0) + mesh!(nHalf,0))] ++
	[(n,n)	=: 0.25*(mesh!(0,0) + mesh!(0,nHalf) + mesh!(nHalf,0) + 
			 mesh!(nHalf,nHalf))] ++

	-- edges (north, south)
	[(0,2*j)	=: mesh!(0,j)| j<-[1..nHalf]] ++
	[(0,2*j-1)	=: 0.5*(mesh!(0,j) + mesh!(0,j-1)) | j<-[1..nHalf]] ++
	[(n,2*j)	=: 0.5*(mesh!(0,j) + mesh!(nHalf,j)) | j<-[1..nHalf]] ++
	[(n,2*j-1)	=: 0.25*(mesh!(0,j) + mesh!(0,j-1) + mesh!(nHalf,j) + 
				 mesh!(nHalf,j-1)) | j<-[1..nHalf]] ++

	-- edges (west, east)
	[(2*i,0)	=: mesh!(i,0) | i<-[1..nHalf]] ++
	[(2*i-1,0)	=: 0.5*(mesh!(i,0) + mesh!(i,nHalf)) | i<-[1..nHalf]] ++
	[(2*i,n)	=: 0.5*(mesh!(i,0) + mesh!(i,nHalf)) | i<-[1..nHalf]] ++
	[(2*i-1,n)	=: 0.25*(mesh!(i,0) + mesh!(i,nHalf) + mesh!(i-1,0) + 
				 mesh!(i-1,nHalf)) | i<-[1..nHalf]] ++

	-- interior
	[(2*i,2*j)	=: mesh!(i,j) | i<-[1..nHalf], j<-[1..nHalf]] ++
	[(2*i,2*j-1)	=: 0.5*(mesh!(i,j) + mesh!(i,j-1)) 
				| i<-[1..nHalf], j<-[1..nHalf]] ++
	[(2*i-1,2*j)	=: 0.5*(mesh!(i,j) + mesh!(i-1,j)) 
				| i<-[1..nHalf], j<-[1..nHalf]] ++
	[(2*i-1,2*j-1)	=: 0.25*(mesh!(i,j) + mesh!(i,j-1) + mesh!(i-1,j) + 
				mesh!(i-1,j-1)) | i<-[1..nHalf], j<-[1..nHalf]])
-}
	where
	    nHalf = nHalf'-1
	    n = 2 * nHalf' - 1


-- random number generator

genRand :: Value -> Value
genRand seed =
	r1 / 655357
	where
	    r1 = (31453257*seed + 271829) `fiRem` 655357
	    x `fiRem` m = x - fromIntegral ((truncate x `div` m) * m)


log2 :: Int -> Int
log2 n = log2' n 0
	where
	    log2' n accum 
		| n > 1		= log2' (n `div` 2) (accum+1)
		| otherwise	= accum
