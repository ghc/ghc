import qualified Data.Vector.Unboxed as U
import Data.Bits
main = print . U.sum . U.map fst $ U.zip
                        (U.enumFromTo 1 (100000000 :: Int))
                        (U.enumFromTo 2 (100000001 :: Int))

