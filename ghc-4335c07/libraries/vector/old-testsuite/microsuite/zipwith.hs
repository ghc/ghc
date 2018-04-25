import qualified Data.Vector as U
import Data.Bits
main = print . U.sum . U.map (`shiftL` 1) $ U.zipWith (*)
                        (U.enumFromTo 1 (100000000 :: Int))
                        (U.replicate (100000000 :: Int) 42)

