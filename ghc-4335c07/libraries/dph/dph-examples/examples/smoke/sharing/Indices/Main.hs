
-- | This divide and conquer program accesseses top-level array from the
--   computations at the bottom of the tree. In lifted backends that
--   don't manage sharing properly, this program will blow up when it tries
--   to replicate the top-level a array at every step in the division phase.
import Util
import Timing
import Randomish
import System.Environment
import Control.Exception
import qualified Vector                         as IU
import qualified Vectorised                     as ID
import qualified Data.Array.Parallel.PArray     as P
import qualified Data.Vector.Unboxed            as U

main
 = do   args    <- getArgs
        
        case args of
         [alg, countMin, countMax] -> run alg (read countMin) (read countMax)
         _            -> usage


-- Vectorised Nested Data Parallel Version.
run "vectorised" count countMax
 | count > countMax = return ()
 | otherwise
 = do   let arr = P.fromList [0 .. count - 1]
        arr `seq` return ()     
                
        (arrResult, tElapsed)
         <- time
         $  let  arr'    = ID.indicesPA arr arr
            in   P.nf arr' `seq` return arr'

--        print   $ P.length arrResult
--        putStr  $ prettyTime tElapsed

        putStrLn $  (show $ P.length arrResult) 
                 ++ "\t " 
                 ++ (show  $ wallTime milliseconds tElapsed)
        run "vectorised" (count * 2) countMax

-- Sequential version using Data.Vector
run "vector" count countMax
 | count > countMax = return ()
 | otherwise
 = do   let arr = U.fromList [0 .. count - 1]
        arr `seq` return ()     
                
        (arrResult, tElapsed)
         <- time
         $  let  arr'    = U.force $ IU.treeLookup arr arr
            in   arr' `seq` return arr'

--        print   $ U.length arrResult
--        putStr  $ prettyTime tElapsed
        putStrLn $  (show $ U.length arrResult) 
                 ++ "\t " 
                 ++ (show  $ wallTime milliseconds tElapsed)

        run "vector" (count * 2) countMax

run _ _ _
 = usage


usage   = putStr $ unlines
        [ "usage: indices <algorithm> <countMin> <countMax>\n"
        , "  algorithm one of " ++ show ["vectorised", "vector"]
        , ""]
