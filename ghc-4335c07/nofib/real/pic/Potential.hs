-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module Potential (potential) where

import	PicType
import	Consts
import	Utils
import Data.Array

-- Given charge density matrix, rho
-- Compute new electrostatic potential phi' where del2(phi') = rho
-- phi from the previous timestep is used as the initial value
-- assume:	phi = phi' + error
-- then:	d_phi = Laplacian(phi) = Laplacian(phi') + Laplacian(error)
--	 	d_error = d_phi - rho = Laplacian(error)
-- 		error' = InvLaplacian(d_error) = InvLaplacian(Laplacian(error))
-- 		phi' = phi - error'

potential :: Phi -> Rho -> Indx -> Indx -> Phi
potential phi rho depth nIter
    | nIter == 0	= phi
    | otherwise		= potential phi' rho depth (nIter-1)
			  where
			      phi' = vCycle rho phi nCell depth


-- vCycle is a multigrid laplacian inverse
-- Given d_phi, find phi where Laplacian(phi) = d_phi
-- Algorithm is to invert d_phi on a course mesh and interpolate to get phi

vCycle :: Phi -> Rho -> Indx -> Indx -> Phi
vCycle phi rho n depth =
	if (depth == 0)
	    then relax phi' rho n
	    else correct phi' eCoarse n nHalf
	where
	    nHalf = n `div` 2
	    nHalf' = nHalf-1
	    phi' = relax phi rho n
	    rho' = residual phi' rho n
	    rCoarse = coarseMesh rho' n
	    eZero = array ((0,0), (nHalf',nHalf')) 
			[((i,j), 0.0) | i<-[0..nHalf'], j<-[0..nHalf']]
	    eCoarse = vCycle eZero rCoarse nHalf (depth-1)


-- laplacian operator
-- mesh configuration where e=(i,j) position, b + d + f + h - 4e
-- a   b   c
-- d   e   f
-- g   h   i
 
laplacianOp :: Mesh -> Range -> Value
laplacianOp mesh [iLo, i, iHi, jLo, j, jHi] =
        -(mesh!(iLo,j)+mesh!(i,jHi)+mesh!(i,jLo)+mesh!(iHi,j)-4*mesh!(i,j))


-- subtract laplacian of mesh from mesh'
-- residual = mesh' - Laplacian(mesh)

residual :: Phi -> Rho -> Indx -> Rho
residual mesh mesh' n =
	applyOpToMesh (residualOp mesh') mesh n
	where
	    residualOp mesh' mesh [iLo, i, iHi, jLo, j, jHi] =
		mesh'!(i,j) - laplacianOp mesh [iLo, i, iHi, jLo, j, jHi]


relax :: Phi -> Rho -> Indx -> Phi
relax mesh mesh' n =
	applyOpToMesh (relaxOp mesh') mesh n
	where
	    relaxOp mesh' mesh [iLo, i, iHi, jLo, j, jHi] =
		0.25 * mesh'!(i,j) + 
	        0.25 * (mesh!(iLo,j)+mesh!(i,jLo)+mesh!(i,jHi)+mesh!(iHi,j))

correct :: Phi -> Mesh -> Indx -> Indx -> Phi
correct phi eCoarse n' nHalf =
	array ((0,0), (n,n))
	[((i,j) , phi!(i,j) + eFine!(i,j)) | i <- [0..n], j <- [0..n]]
	where
	    eFine = fineMesh eCoarse nHalf
	    n = n'-1
