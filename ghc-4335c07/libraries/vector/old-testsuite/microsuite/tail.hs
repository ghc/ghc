import qualified Data.Vector as U
import Data.Bits
main = print . U.length . U.tail . U.replicate 1000000 $ (7 :: Int)

