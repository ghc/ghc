
module Test where
import Data.Array.Parallel.PArray
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray.Stream


arrI0   = (fromListPA [])               :: PArray Int
arrN0   = (fromListPA [])               :: PArray (PArray Int)
arrN0'  = (fromListPA [fromListPA []])  :: PArray (PArray Int)

ex_1 = replicatePR 10 (5 :: Int)

tagsI5 = [0, 1, 1, 0, 0, 1 :: Int]

arrI1  = fromListPA [0 :: Int]
arrI2  = fromListPA [9, 3 :: Int]
arrI2' = fromListPA [4, 2 :: Int]
arrI3  = fromListPA [1, 2, 3 :: Int]
arrI3' = fromListPA [8, 6, 3 :: Int]
arrI4  = fromListPA [3, 2, 4, 0 :: Int]
arrI5  = fromListPA [5, 6, 7, 8, 9 :: Int]
arrI5' = fromListPA [1, 2, 3, 4, 5 :: Int]

arrI7 = fromListPA [7, 8, 9, 10, 11, 12, 13 :: Int]

arrN1  = fromListPA [arrI5]
arrN2  = fromListPA [arrI1, arrI3]
arrN3  = fromListPA [arrI1, arrI2', arrI5]
arrN3' = fromListPA [arrI3, arrI3', arrI2]
arrN4  = fromListPA [arrI7,  arrI1, arrI3, arrI1]
arrN4' = fromListPA [arrI5', arrI3, arrI7, arrI3]
arrN7  = fromListPA [arrI7, arrI1, arrI3, arrI1, arrI5, arrI1, arrI3]
arrN7' = fromListPA [arrI5, arrI3, arrI1, arrI1, arrI7, arrI3, arrI5]

tagsN7 = [1, 1, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 1:: Int]

arrM3  = fromListPA [arrN2, arrN1, arrN7]
arrM3' = fromListPA [arrN4, arrN2, arrN3]

-- TODO: this fails and not caught by quickcheck props
-- validPA $ combine2PA' [0, 1, 0] arrM3 arrM3'

arrM6   = fromListPA    [arrN4, arrN2, arrN3, arrN1, arrN4', arrN1]
arrM6'  = replicatesPA' [3, 1, 2] arrM3

