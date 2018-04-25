
import Vectorised
import SVG
import Timing
import Points2D.Generate
import Points2D.Types
import System.Environment
import Data.Array.Parallel.PArray	as P

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [pointCount]	
	    -> run (read pointCount) Nothing
	
	  [pointCount, fileSVG]
	    -> run (read pointCount) (Just fileSVG)

	  _ -> do
		putStr usage
		return ()


-- | Command line usage information.
usage :: String
usage	= unlines
	[ "Usage: quickhull <points> [out.svg]"	]


-- | Run the benchmark.
run 	:: Int 			-- ^ How many points to use.
	-> Maybe String 	-- ^ File name to dump an SVG of the output to.
	-> IO ()
	
run pointCount mFileSVG
 = do
	vPoints	<- pointsPArrayOfUArray
		$ genPointsDisc pointCount (400, 400) 350 

	-- Force points to create the input vector.
	vPoints `seq` return ()

	-- Compute the convex hull.
	(vHull, tElapsed)
		<- time 
		$  let 	vHull	= quickhullPA vPoints
		   in	vHull `seq` return vHull
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed
	
	-- If we were asked for an SVG then write it out to file.
	maybe 	(return ())
	 	(\fileSVG -> 
			writeFile fileSVG
			 $ makeSVG 	(roundPoints $ P.toList vPoints) 
					(roundPoints $ P.toList vHull))
		mFileSVG
