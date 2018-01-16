
import Timing
import Randomish
import System.Environment
import Data.Array.Parallel	as P
import Data.Array.Parallel.PArray	as P
import qualified Data.Vector.Unboxed	as V
import qualified Vector	                as V
import qualified Vectorised	        as Z

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [alg, len] 	-> run alg (read len) 
	  _		-> usage

usage
 = putStr $ unlines
 	[ "usage: dotp <alg> <length>"
 	, "  alg one of " ++ show ["vectorised", "vector"] ]
	
run alg len
 = do	let vec1 = randomishDoubles len 0 1 1234
	let vec2 = randomishDoubles len 0 1 12345

	(result, tElapsed) <- runAlg alg vec1 vec2

	putStr	$ prettyTime tElapsed
	putStr	$ (take 12 $ show result) ++ "\n"

runAlg "vectorised" vec1 vec2 
 = do	let arr1 = P.fromUArray vec1
	let arr2 = P.fromUArray vec2
	arr1 `seq` arr2 `seq` return ()

	time	$ let result	= Z.dotPA arr1 arr2
		  in  result `seq` return result

runAlg "vector" vec1 vec2
 = do	vec1 `seq` vec2 `seq` return ()

	time	$ let result	= V.dotV vec1 vec2 
		  in  result `seq` return result
		
