
import Vector
-- Disabled: see Vectorised
--import Vectorised
import Vector1
import Vectorised1
import Timing
import Points2D.Generate
import Points2D.Types
import System.Environment
import Data.Array.Parallel.PArray	as P

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [alg,pointCount]	
	    -> run alg (read pointCount)

	  _ -> do
		putStr usage
		return ()


-- | Command line usage information.
usage :: String
usage	= unlines
	[ "Usage: closestpairs <vector|vectorised|vector1|vectorised1> <points>"	]


-- | Run the benchmark.
run 	:: String
        -> Int 			-- ^ How many points to use.
	-> IO ()
	

run "vectorised" pointCount
 = do
   putStrLn "Disabled: see Vectorised.hs"
 {-
	vPoints	<- pointsPArrayOfUArray
		$ genPointsDisc pointCount (400, 400) 350 

	-- Force points to create the input vector.
	vPoints `seq` return ()

	-- Compute the convex hull.
	(pair, tElapsed)
		<- time 
		$  let 	pair	= closestPA vPoints
		   in	pair `seq` return pair
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed
    -}


run "vector" pointCount
 = do
	let vPoints	= genPointsDisc pointCount (400, 400) 350 

	-- Force points to create the input vector.
	vPoints `seq` return ()

	-- Compute the convex hull.
	(pair, tElapsed)
		<- time 
		$  let 	pair	= closestV vPoints
		   in	pair `seq` return pair
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed


run "vectorised1" pointCount
 = do
	vPoints	<- pointsPArrayOfUArray
		$ genPointsDisc pointCount (400, 400) 350 

	-- Force points to create the input vector.
	vPoints `seq` return ()

	-- Compute the convex hull.
	(pair, tElapsed)
		<- time 
		$  let 	pair	= closest1PA vPoints
		   in	pair `seq` return pair
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed

run "vector1" pointCount
 = do
	let vPoints	= genPointsDisc pointCount (400, 400) 350 

	-- Force points to create the input vector.
	vPoints `seq` return ()

	-- Compute the convex hull.
	(pair, tElapsed)
		<- time 
		$  let 	pair	= closest1V vPoints
		   in	pair `seq` return pair
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed


