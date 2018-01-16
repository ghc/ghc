--	
--	Patricia Fasel
--	Los Alamos National Laboratory
--	PIC: Particle in Cell
--	1990 August
--
module Consts (charge, mass, nCell, nStep, maxDepth) where

import PicType
import Utils

nCell, nStep, maxDepth :: Indx
charge, mass :: Value
nCell		= 16			-- number of Cells
nStep		= 10			-- number of time steps
maxDepth	= (log2 nCell) - 1	--
charge		= 1.0
mass		= 1.0
