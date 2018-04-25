-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module ChargeDensity (chargeDensity) where

import	PicType
import	Consts
import Data.Array

-- Phase I: calculate the charge density rho
-- Each particle represents an amount of charge distributed over an entire cell.
-- In discretizing the charge density at the grid points we split the cell into
--    four rectangles and assign to each corner an amount of charge proportional
--    to the area of the opposite diagonal sub rectangle
-- So each particle contributes a portion of charge to four quadrants
-- particle position is (i+dx, j+dy)
-- nw quadrant (1-dx)(1-dy)(charge) is added to rho(i,j)
-- ne quadrant (dx)(1-dy)(charge) is added to rho(i,j+1)
-- sw quadrant (1-dx)(dy)(charge) is added to rho(i+1,j)
-- se quadrant (dx)(dy)(charge) is added to rho(i+1,j+1)
-- wrap around used for edges and corners

chargeDensity :: ParticleHeap -> Rho
chargeDensity (xyPos, xyVel) =
	accumArray (+) 0 ((0,0), (n,n)) (accumCharge xyPos)
	where
	    n = nCell-1


-- for each particle, calculate the distribution of density
-- based on (x,y), a proportion of the charge goes to each rho

accumCharge :: [Position] -> [MeshAssoc]
accumCharge [] = []
accumCharge ((x,y):xys) =
	[((i ,j ) , charge * (1-dx) * (1-dy))] ++
	[((i',j ) , charge * dx * (1-dy))] ++
	[((i ,j') , charge * (1-dx) * dy)] ++
	[((i',j') , charge * dx * dy)] ++
	accumCharge xys
	where
	    i = truncate x
	    i' = (i+1) `rem` nCell
	    j = truncate y
	    j' = (j+1) `rem` nCell
	    dx = x - fromIntegral i
	    dy = y - fromIntegral j
