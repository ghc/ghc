
import Vectorised
import Timing
import Randomish
import System.Environment
import Data.Vector.Unboxed		(Vector)
import Data.Array.Parallel	as P
import Data.Array.Parallel.PArray	as P
import qualified Data.Vector.Unboxed	as V
import Data.Maybe

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [len]	-> run (read len) 
	  _	-> putStr usage


-- | Command line usage information.
usage :: String
usage	= unlines
	[ "Usage: quicksort <length>" ]


-- | Run the benchmark.
run :: Int -> IO ()
run len
 = do	-- Create the input vector
	let vInts 	= fromUArray
			$ randomishDoubles len 0 1 1234

	vInts `seq` return ()

	-- Compute the convex hull.
	(vSorted, tElapsed)
		<- time 
		$  let 	vSorted	= quicksortPA vInts
		   in	vSorted `seq` return vSorted
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed
	
	-- Check they're really sorted.
	print	$ isSorted $ P.toUArray vSorted
	

-- | Check if a vector is sorted (monotonically increasing)
isSorted :: Vector Double -> Bool
isSorted = isJust . V.fold1M (\x y -> if y >= x then Just y else Nothing)