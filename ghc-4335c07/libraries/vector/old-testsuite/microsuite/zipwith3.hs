import qualified Data.Vector.Unboxed as U
import Data.Bits
main = print . U.sum $ U.zipWith3 (\x y z -> x * y * z)
                        (U.enumFromTo 1 (100000000 :: Int))
                        (U.enumFromTo 2 (100000001 :: Int))
                        (U.enumFromTo 7 (100000008 :: Int))

