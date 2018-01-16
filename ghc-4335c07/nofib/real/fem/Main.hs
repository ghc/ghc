-- Glasgow Haskell 0.403 : FINITE ELEMENT PROGRAM V2
-- **********************************************************************
-- *                                                                    *
-- * FILE NAME : main.hs                DATE : 13-3-1991                *
-- *                                                                    *
-- * CONTENTS : Main program of FEM.                                    *
-- *                                                                    *
-- **********************************************************************

import Database
import Vector
import Displacement
import Elemforce
import PrintSource
import Printuvwforce


main = getContents >>= \ s -> process s

process :: [Char] ->  IO ()

process s =
	putStr a
        where
		a  = source_data db ++
		     uvwresult db uvwres ++ 
		     forceresult db frc     
		db = (idatabase s, rdatabase s)
		uvwres = uvw db
		frc    = forces db uvwres

