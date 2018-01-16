
import Vector
import Vectorised
import Timing
import Data.Array.Parallel
import qualified Data.Array.Parallel.Prelude.Word8    as W
import qualified Data.Array.Parallel.PArray         as P
import qualified Data.Array.Parallel.Unlifted        as U
import Data.Char
import System.Environment
import qualified Data.Vector.Unboxed    as VU

str    =  "When   I   look  into  the   looking glass I'm always sure to see"
       ++ " no matter how I dodge         about, me looking      back at me."

search = "look"

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [alg,replCount]	
	    -> run alg (read replCount)

	  _ -> do
		putStr usage
		return ()


-- | Command line usage information.
usage :: String
usage	= unlines
	[ "Usage: stringsearch <vector|vectorised> <points>"	]


run "vectorised"  n
 = do
    let str' = concat $ replicate n str

    -- convert string to a PArray
    let arrOfStr = P.fromUArray . U.map W.fromInt . U.fromList . map ord

    let paStr   :: PArray W.Word8
        paStr = arrOfStr str'
    
    let paSearch:: PArray W.Word8
        paSearch = arrOfStr search
    
    paStr `seq` paSearch `seq` return ()

    -- find indices of search in string
    (res, tElapsed)
        <- time
        $  let s = P.toUArray $ searchPA paSearch paStr
           in  s `seq` return s
    
    putStr $ "results  = " ++ show res ++ "\n"
    putStr $ prettyTime tElapsed

run "vector"  n
 = do
    let str' = concat $ replicate n str

    -- convert string to a PArray
    let arrOfStr = VU.fromList

    let paStr   :: VU.Vector Char
        paStr = arrOfStr str'
    
    let paSearch:: VU.Vector Char
        paSearch = arrOfStr search
    
    paStr `seq` paSearch `seq` return ()

    -- find indices of search in string
    (res, tElapsed)
        <- time
        $  let s = searchV paSearch paStr
           in  s `seq` return s

    putStr $ "results  = " ++ show res ++ "\n"
    putStr $ prettyTime tElapsed


