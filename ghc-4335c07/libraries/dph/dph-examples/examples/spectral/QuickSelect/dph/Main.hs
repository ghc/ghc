
import Vectorised
import Vector
import Timing
import Randomish
import System.Environment
import Data.Vector.Unboxed		(Vector)
import Data.Array.Parallel	as P
import Data.Array.Parallel.PArray	as P
import qualified Data.Vector.Unboxed	as V
import Data.Maybe
import Data.List (sort)

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [alg, len]	-> run alg (read len) 
	  _	        -> putStr usage


-- | Command line usage information.
usage :: String
usage	= unlines
	[ "Usage: quickselect <vector|vectorised> <length>"
        , "Generate random vector of size <length> and take median." ]


-- | Run the benchmark.
run :: String -> Int -> IO ()
run "vectorised" len
 = do	-- Create the input vector
	let vInts 	= fromUArray
			$ randomishDoubles len 0 1 1234

	vInts `seq` return ()

        let k = len `div` 2

	-- Compute the convex hull.
	(med, tElapsed)
		<- time 
		$  let 	med	= quickselectPA vInts k
		   in	med `seq` return med
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed

run "vector" len
 = do	-- Create the input vector
	let vInts 	= randomishDoubles len 0 1 1234

	vInts `seq` return ()

        let k = len `div` 2

	-- Compute the convex hull.
	(med, tElapsed)
		<- time 
		$  let 	med	= quickselect vInts k
		   in	med `seq` return med
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed
