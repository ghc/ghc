
-- | In with lifted backends that don't manage sharing properly, the rank
--   example suffers similar work complexity problems as the indices example.
import Util
import Timing
import Randomish
import System.Environment
import Control.Exception
import qualified Vectorised                     as RD
import qualified Data.Array.Parallel.PArray     as PA
import qualified Data.Vector.Unboxed            as V

main 
 = do   args    <- getArgs
        
        case args of
         [alg, count] -> run alg (read count)
         _            -> usage


run "vectorised" count
 = do   let arr = PA.fromList [0 .. count - 1]
        arr `seq` return ()     
                
        (arrRanks, tElapsed)
         <- time
         $  let  arr'    = RD.ranksPA arr
            in   PA.nf arr' `seq` return arr'

        print   $ PA.length arrRanks
        putStr  $ prettyTime tElapsed

run _ _
 = usage


usage   = putStr $ unlines
        [ "usage: rank <algorithm> <count>\n"
        , "  algorithm one of " ++ show ["vectorised"]
        , ""]
