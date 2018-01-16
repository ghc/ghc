
import Vectorised
import Timing
import Randomish
import System.Environment
import Data.Vector.Unboxed		(Vector)
import Data.Array.Parallel	as P
import Data.Array.Parallel.PArray	as P
import qualified Data.Vector.Unboxed	as V
import qualified Vector 		as V
import Data.Maybe

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [alg, len]	-> run alg (read len) 
	  _		-> usage

usage	
 = putStr $ unlines
	[ "Usage: primes alg <length>"
	, "       alg one of " ++ show ["vectorised", "vector"] ]

-- | Run the benchmark
run :: String -> Int -> IO ()

run "vectorised" len
 = do	(vPrimes, tElapsed)
	 <- time $ let 	vPrimes	= primesPA len
		   in	vPrimes `seq` return vPrimes
				
	-- Print how long it took.
	putStr $ prettyTime tElapsed

	-- Print a checksum of all the primes.
	putStrLn $ show $ sum $ P.toList vPrimes

run "vector" len
 = do	(vPrimes, tElapsed)
	 <- time $ let 	vPrimes	= V.primes len
		   in	vPrimes `seq` return vPrimes

	-- Print how long it took.
	putStr $ prettyTime tElapsed

	-- Print a checksum of all the primes.
	putStrLn $ show $ V.sum $ vPrimes
	

run _ _	= usage



