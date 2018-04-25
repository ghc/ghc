
-- | Test filter operations.
--   These are fairly involved because the implementation uses selectors and
--   the packByTag array primitive.
import Vectorised			as Z
import Vector	        		as V
import qualified Data.Vector.Unboxed	as V
import Timing
import System.Environment
import Data.Array.Parallel.PArray	as P

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [alg, len]	-> run alg (read len)
	  _ 		-> usage

usage
 = putStr $ unlines
 	[ "usage: evens <alg> <length>"
 	, "  alg one of " ++ show ["vectorised", "vector"] ]

run alg len
 = do	let vec	= V.fromList [0..len - 1]
	runAlg alg vec
	
runAlg "vectorised" vec
 = do	let arr	= P.fromUArray vec
	arr `seq` return ()

	(arr', tElapsed)
	 <- time $ let	arr' = evensPA arr
		   in	arr' `seq` return arr'
					
	putStr   $ prettyTime tElapsed
	putStrLn $ show $ P.length arr'

runAlg "vector" vec
 = do	vec `seq` return ()
	
	(vec', tElapsed)
	 <- time $ let	vec' = evensV vec
		   in	vec' `seq` return vec'
		
	putStr   $ prettyTime tElapsed
	putStrLn $ show $ V.length vec'
	