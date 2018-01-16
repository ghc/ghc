-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
-- Copyright, 1990, The Regents of the University of California.
-- This software was produced under a U.S. Government contract (W-7405-ENG-36)
-- by the Los Alamos National Laboratory, which is operated by the University
-- of California for the U.S. Department of Energy.  The U.S. Government is
-- licensed to use, reproduce, and distribute this software.  Permission is
-- granted to the public to copy and use this software without charge, provided
-- that this notice and any statement of authorship are reproduced on all
-- copies.  Neither the Government nor the University makes any warranty,
-- express or implied, or assumes any liability for the use of this software.

module Pic (pic) where

import	PicType
import	Consts
import	Utils
import	ChargeDensity
import	Potential
import	ElecField
import	PushParticle
import Data.Array

-- PIC, particle in cell, a basic electrodynamics application
-- Given an initial configuration of particles, follow how they move under the
--     electric field they induce
-- Torroidal boundary conditions are assumed, so wrap in both dimensions
-- given nPart the number of particles considered
-- given nStep the number of time steps to put the particles through
-- given nCell the dimension of the matrix of cells

pic :: Indx -> [Char]
pic nPart =
	show dt'
	where
	    partHeap = initParticles nPart
	    dt = 0.001
	    phi = initPhi partHeap
	    (dt', phi', partHeap') = timeStep partHeap phi dt 0 nStep


-- during each time step perform the following calculations
-- calculate the charge density (rho), using position of particles
-- calculate the new potential (phi), by solving Laplace's equation
-- 	del2(phi) = rho , using rho and phi of last timestep
-- calculate the electric field, E = del(phi), using phi of this time step
-- push each particle some distance and velocity using electric field, for a
-- 	timestep deltaTime, small enough that no particle moves more than the
-- 	width of a cell
-- an NxN mesh is used to represent value of x and y in the interval [0,1]
-- so delta_x = delta_y = 1/n
--
-- phi ((0,0), (n,n)) = electrostatic potential at grid point (i,j)
-- rho ((0,0), (n,n)) = charge density at grid point (i,j)
-- xElec ((0,0), (n,n)) = x component of electric field between (i,j) (i,j+1)
-- yElec ((0,0), (n,n)) = y component of electric field between (i,j) (i+1,j)
-- [xyPos] = (x,y) coordinate of particle displacement in units of delta_x
-- [xyVel] = (x,y) coordinate of particle velocity in units of delta_x/sec

timeStep :: ParticleHeap -> Phi -> Value -> Indx -> Indx -> (Value, Phi, ParticleHeap)
timeStep partHeap phi dt depth step
	| step == 0		= (dt, phi, partHeap)
	| otherwise		=
	    timeStep partHeap' phi' dt' depth' (step-1)
	    where
		rho = chargeDensity partHeap
		phi' = potential phi rho depth 1
		xyElec = elecField phi'
		(maxVel, maxAcc, partHeap') =pushParticle partHeap xyElec dt 0 0
		dt' = (sqrt (maxVel*maxVel + 2*maxAcc) - maxVel) / maxAcc
		depth' = (depth+1) `rem` maxDepth


initParticles :: Indx -> ParticleHeap
initParticles nPart =
	(xyPos, xyVel)
	where
	    nCellD = fromIntegral nCell
	    nPartD = fromIntegral (nPart+1)
	    xyPos = [(xPos,yPos) | 
			i <- [1..nPart],
			xPos <- [nCellD * genRand (fromIntegral i/ nPartD)],
			yPos <- [nCellD * genRand xPos]]
	    xyVel = [(0.0,0.0) | i <- [1..nPart]]


initPhi :: ParticleHeap -> Phi
initPhi partHeap =
	potential phi0 rho maxDepth 1
	where
	    rho = chargeDensity partHeap
	    phi0 = array ((0,0), (n,n))
		   [((i,j), 0.0) | i <- [0..n], j <- [0..n]]
	    n = nCell-1
