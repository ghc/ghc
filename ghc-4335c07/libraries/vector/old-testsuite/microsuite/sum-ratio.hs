import qualified Data.Vector as U
import Data.Ratio

main = print . U.sum $ U.replicate (100000000 :: Int) (1 % 2 :: Rational)

