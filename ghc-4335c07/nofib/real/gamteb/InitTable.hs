-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
module InitTable (ergs, xComp, xPair, xPhot) where

import GamtebType
import Consts
import Data.Array

-- initialize the cross section tables
-- these tables are constant, used with the energy and energy index
-- in calculating probabilities

ergs :: Array Indx Value
ergs = array (1,numLev) (zipWith (,) [1..numLev] (map f2 erg))
	where erg = 
		[0.001, 0.0015, 0.002, 0.003, 0.004, 0.005, 0.006, 0.008,
		0.01, 0.015, 0.02, 0.03, 0.04, 0.05, 0.06, 0.08,
		0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8,
		1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0,
		10.0, 15.0, 20.0]
	
xComp :: Array Indx Value
xComp = array (1,numLev) (zipWith (,) [1..numLev] (map f1 xc))
	where xc = 
		[0.015, 0.0296, 0.0451, 0.0717, 0.0913, 0.105, 0.115, 0.128, 
		0.137, 0.152, 0.160, 0.165, 0.165, 0.163, 0.160, 0.153, 
		0.146, 0.133, 0.122, 0.106, 0.0953, 0.0867, 0.0802, 0.0707, 
		0.0637, 0.0516, 0.0440, 0.0346, 0.0289, 0.025, 0.0221, 0.0181, 
		0.0154, 0.0114, 0.00913]
	
xPair :: Array Indx Value
xPair = array (1,numLev) (zipWith (,) [1..numLev] (map f1 xp))
	where xp = 
		[0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
		0.0000792, 0.000316, 0.000923, 0.00153, 0.00208, 0.00256, 
		0.00343, 0.00414, 0.00547, 0.00652]
	
xPhot :: Array Indx Value
xPhot = array (1,numLev) (zipWith (,) [1..numLev] (map f1 xpe))
	where xpe = 
		[2010.0, 632.0, 280.0, 87.7, 37.3, 18.9, 10.4, 4.01, 
		1.91, 0.489, 0.192, 0.0491, 0.0186, 0.00887, 0.00481, 
		0.00179, 0.000862, 0.000234, 0.0000918,
		0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 
		0.0, 0.0, 0.0, 0.0]
	
f1 :: Value -> Value
f1 x = if (x < small)
	then nothing
	else log (2.2*x)

f2 :: Value -> Value
f2 x = if (x < small)
	then nothing
	else log x
