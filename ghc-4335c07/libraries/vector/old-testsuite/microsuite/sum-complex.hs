import qualified Data.Vector as U
import Data.Complex

main = print . U.sum $ U.replicate (1000000000 :: Int) (1 :+ 1 ::Complex Double)

