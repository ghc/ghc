--
--	Patricia Fasel
--	Los Alamos National Laboratory
--	1990 August
--
module PushParticle (pushParticle) where

import	PicType
import	Consts
import Data.Array--1.3

-- Phase IV: Particle push
-- Each particle has an initial velocity determined by its motion during the
--     previous timestep and is accelerated by the field induced by the 
--     collection of particles
--
-- Compute the acceleration of each particle
-- Find the maximum acceleration in x and y directions
-- Determine a safe delta t, such that no particle will move a
--     distance greater than the cell
-- Compute new position and velocity of each particle

pushParticle :: ParticleHeap -> Electric -> Value -> Value -> Value -> (Value, Value, ParticleHeap) 
pushParticle ([], []) xyElec dt maxAcc maxVel = (maxAcc, maxVel, ([], []))
pushParticle (((xPos,yPos):xyPos), ((xVel,yVel):xyVel))
	     (xElec, yElec) dt maxAcc maxVel =
	(maxAcc'', maxVel'', 
	    (((xPos',yPos'):xyPos'), ((xVel',yVel'):xyVel')))
	where
	    i = truncate xPos
	    j = truncate yPos
	    i1 = (i+1) `rem` nCell
	    j1 = (j+1) `rem` nCell
	    dx = xPos - fromIntegral i
	    dy = yPos - fromIntegral j
	    xAcc = (charge/mass) * (xElec!(i,j)*(1-dy) + xElec!(i,j1)*dy)
	    yAcc = (charge/mass) * (yElec!(i,j)*(1-dx) + yElec!(i1,j)*dx)
	    xTV = xAcc*dt + xVel
	    yTV = yAcc*dt + yVel
	    xT = xTV*dt + xPos
	    yT = yTV*dt + yPos
	    maxAcc' = max maxAcc (max (abs xAcc) (abs yAcc))
	    maxVel' = max maxVel (max (abs xTV) (abs yTV))
	    (xVel',yVel') = (xTV, yTV)
	    xPos' = if (xT >= fromIntegral nCell) 
			then xT - fromIntegral nCell
			else if (xT < 0.0) 
			    then xT + fromIntegral nCell
			    else xT
	    yPos' = if (yT >= fromIntegral nCell) 
			then yT - fromIntegral nCell
			else if (yT < 0.0) 
			    then yT + fromIntegral nCell
			    else yT
	    (maxAcc'', maxVel'', (xyPos', xyVel')) =
		pushParticle (xyPos, xyVel) (xElec, yElec) dt maxAcc' maxVel'
