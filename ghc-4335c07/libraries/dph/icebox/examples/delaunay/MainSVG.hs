import Types
import Delaunay

import qualified Data.Array.Parallel.Unlifted as U
import qualified Data.Array.Parallel.Prelude  as P
import qualified Data.Array.Parallel.PArray as PA

import SVG

import System.Environment
import System.IO

main = do
         [arg] <- getArgs
         h <- openBinaryFile arg ReadMode
         pts <- U.hGet h
         let points = P.fromUArrPA_2' pts
             edges  = PA.toList (delaunayPoints points)
         putStrLn $ svg (PA.toList points) edges


