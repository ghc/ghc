
import Util
import Timing
import System.Environment
import System.Random
import Control.Exception
import qualified MergeSort			as V
import qualified LegacyList.OddEven 		as LO
import qualified LegacyList.Step		as LS
import qualified Data.Array.Parallel.PArray	as P
import qualified Data.Array.Parallel.Unlifted	as U

type Algorithm	
	= P.PArray Int -> P.PArray Int

algorithms
 = 	[ ("oddeven",		V.sortCorePA)
   	, ("list-oddeven",	P.fromList . LO.sort . P.toList)
	, ("list-step",		P.fromList . LS.sort . P.toList) ]

main 
 = do	args	<- getArgs
	case args of
	 [alg, count] 	
	  | Just fun	<- lookup alg algorithms	
	  -> run fun (read count)

 	 _		-> usage


usage :: IO ()
usage 
 = putStr $  "usage: mergesort <algorithm> <count>\n"
	  ++ "  algorithms = " ++ (show $ map fst algorithms) ++ "\n\n"


run :: Algorithm -> Int -> IO ()
run alg count
 | not $ isPowerOfTwo count
 = error "mergesort: length of array must be a power of two."

 | otherwise
 = do	let gen		=  mkStdGen 12345
	let arrElems	=  (P.fromUArrPA' $ U.randomRs count (0, 100) gen)  :: P.PArray Int
	evaluate $ P.nf arrElems
		
	(arrSorted, tElapsed)
		<- time
		$  let	parr'	= alg arrElems
		   in	parr' `seq` return parr'
		
	putStr	$ prettyTime tElapsed
	print	$ P.toList arrSorted
	print	$  (isSorted $ P.toList arrSorted) 
		&& (P.length arrSorted == P.length arrElems)

