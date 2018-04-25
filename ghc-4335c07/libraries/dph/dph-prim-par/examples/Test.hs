
module Test 
        ( module Data.Array.Parallel.Unlifted.Distributed
        , module Data.Array.Parallel.Unlifted.Parallel
        , module Data.Array.Parallel.Pretty
        , module Data.Array.Parallel.Unlifted.Distributed.Types
        , usegd
        , upssegd
        , pdatas
        , results)
where
import Data.Array.Parallel.Pretty                       hiding (empty)
import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Distributed.Types
import Data.Array.Parallel.Unlifted.Sequential.USegd


import qualified Data.Array.Parallel.Unlifted.Parallel.UPSegd   as UPSegd
import qualified Data.Array.Parallel.Unlifted.Parallel.UPSSegd  as UPSSegd
import qualified Data.Array.Parallel.Unlifted.Sequential.Combinators    as Seq


import qualified Data.Vector.Unboxed                            as U
import qualified Data.Vector                                    as V
 

usegd   = lengthsToUSegd
                (U.fromList [80, 10, 20, 40, 50, 10])

upssegd  = UPSSegd.mkUPSSegd
                (U.fromList [100, 190, 200, 300, 450, 490])
                (U.fromList [0,   0,   1,   2,   2,   3])
                usegd

pdatas   :: V.Vector (U.Vector Int)                
pdatas   =  V.fromList
        [ U.enumFromTo 0    499
        , U.enumFromTo 1000 1499
        , U.enumFromTo 2000 2499
        , U.enumFromTo 3000 3499 ]

results = UPSSegd.foldSegsWith (+) 
                (Seq.foldSSU (+) 0)
                upssegd pdatas