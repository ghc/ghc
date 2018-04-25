
-- | Test maps, sums and enumerations.
import Timing
import Randomish
import System.Environment
import qualified Vector	                as V
import qualified Vectorised	        as Z
import qualified Data.Vector.Unboxed	as V

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [alg, len] 	-> run alg (read len) 
	  _		-> usage

usage
 = putStr $ unlines
 	[ "usage: sumsq <alg> <length>"
 	, "  alg one of " ++ show ["vectorised", "vector"] ]
	
run alg num
 = do	(result, tElapsed) <- runAlg alg num

	putStr	$ prettyTime tElapsed
	print result

runAlg "vectorised" num
 =	time	$ let result	= Z.sumSq num
		  in  result `seq` return result

runAlg "vector" num
 =	time	$ let result	= V.sumSq num
		  in  result `seq` return result
		
