
import Util
import Timing
import Randomish
import System.Environment
import Control.Exception
import qualified TreeLookupVectorised           as TD
import qualified Data.Array.Parallel.PArray     as P
import qualified Data.Vector.Unboxed            as V


main 
 = do   args    <- getArgs
        
        case args of
         [alg, count] -> run alg (read count)
         _            -> usage


run "vectorised" count
 = do   let arr = P.fromListPA [0 .. count - 1]
        arr `seq` return ()     
                
        (arrResult, tElapsed)
         <- time
         $  let  arr'    = TD.treeLookupPA arr arr
            in   P.nfPA arr' `seq` return arr'

        print   $ P.lengthPA arrResult
        putStr  $ prettyTime tElapsed

run _ _
 = usage


usage   = putStr $ unlines
        [ "usage: indices <algorithm> <count>\n"
        , "  algorithm one of " ++ show ["vectorised"]
        , ""]
