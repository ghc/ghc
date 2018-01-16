
module Sums where
import qualified Data.Array.Parallel.PArray     as PA
import Data.Array.Parallel.PArray.PRepr.Instances
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.Types
import qualified Data.Vector                    as V
import qualified Data.Array.Parallel.Unlifted    as U

arr1 :: PArray (Either Int Int)
arr1 = PA.fromList [Left 10, Left 20, Right 30, Left 40, Right 50 ]

arr2 :: PArray (Either Int Int)
arr2 = PA.fromList [Right 60, Right 70, Left 80 ]


pdatas :: PDatas (Sum2 Int Int)
pdatas = fromVectordPR (V.fromList [toArrPRepr $ unpack arr1, toArrPRepr $ unpack arr2])

segd    = U.lengthsToSegd (U.fromList [3, 2, 2, 2, 1])
ssegd   = U.mkSSegd     
                (U.fromList [0, 0, 3, 3, 2])
                (U.fromList [0, 1, 0, 0, 1])
                segd