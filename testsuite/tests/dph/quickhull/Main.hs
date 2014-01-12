
import qualified Types as QH
import QuickHullVect (quickhull)

import qualified Data.Array.Parallel.Unlifted 	    as U
import qualified Data.Array.Parallel.Prelude 	    as P

import qualified Data.Array.Parallel.PArray         as P
import Data.Array.Parallel.PArray		    (PArray)

import System.Environment
import Data.List

import SVG
import TestData


-----
runQuickhull :: PArray QH.Point -> [(Double, Double)]
runQuickhull pts 
 = let result = quickhull pts
       resxs  = P.toUArray (QH.xsOf result)
       resys  = P.toUArray (QH.ysOf result)
   in  U.index "runQuickhull" resxs 0 `seq` (zip (U.toList resxs) (U.toList resys))


-- Main Program ---------------------------------------------------------------
main 
 = do	args	<- getArgs
	let n = case args of
		 [s]	-> read s
		 _	-> 1000

	paInput <- toPArrayPoints 
		$  genPointsCombo n
	
	let psHull  = runQuickhull paInput
	    psInput = P.toList paInput
	
	putStr 
	 $ makeSVG 
		(roundPoints psInput)
		(roundPoints psHull)
