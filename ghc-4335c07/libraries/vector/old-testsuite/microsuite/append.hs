import qualified Data.Vector as U
import Data.Bits
main = print . U.sum . U.map (`shiftL` 2) $
           (U.++) (U.replicate 10000000 (1::Int))
                  (U.replicate 10000000 (7::Int))
