
import Util
import Timing
import Randomish
import System.Environment
import System.Random
import Control.Exception
import qualified Vector                         as RV
import qualified Vectorised                     as RD
import qualified Data.Array.Parallel.PArray     as P
import qualified Data.Vector.Unboxed            as V


main 
 = do   args    <- getArgs
        
        case args of
         [alg, count]   
           |  not $ isPowerOfTwo (read count)
           -> error "reverse: length of array must be a power of two."

           | otherwise -> run alg (read count)

         _ -> usage


run "vectorised" count
 = do   let arr = P.fromList [0 .. count - 1]
        arr `seq` return ()     
                
        (arrReversed, tElapsed)
         <- time
         $  let  arr'    = RD.treeReversePA arr
            in   arr' `seq` return arr'

        print arrReversed
        putStr  $ prettyTime tElapsed

run "vector" count
 = do   let arr = V.fromList [0 .. count - 1]
        arr `seq` return ()
        
        (arrReversed, tElapsed)
         <- time
         $  let arr'    = RV.treeReverse arr
            in  arr' `seq` return arr'
            
        print arrReversed
        putStr  $ prettyTime tElapsed

run _ _
 = usage


usage   = putStr $ unlines
        [ "usage: reverse <algorithm> <count>\n"
        , "  algorithm one of " ++ show ["vectorised"]
        , ""]
