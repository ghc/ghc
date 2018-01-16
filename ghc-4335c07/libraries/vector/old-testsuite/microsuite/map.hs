import qualified Data.Vector as U
import Data.Bits
main = print . U.sum . U.map (`shiftL` 1) . U.map (*2) . U.map (+1) . U.replicate (100000000::Int) $ (8::Int)

