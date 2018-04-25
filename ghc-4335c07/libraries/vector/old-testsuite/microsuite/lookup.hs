import qualified Data.Vector as U
import Data.Bits
main = print . U.lookup 10000
             . U.zip (U.enumFromTo 1 (10000000 :: Int)) $
                     (U.replicate (10000000 :: Int) (42::Int))
