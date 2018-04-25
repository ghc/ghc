import qualified Data.Vector as U
import Data.Bits
main = print . U.sum . U.repeat 10 $ U.replicate (10000000 :: Int) (5::Int) 

