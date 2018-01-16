import qualified Data.Vector as U
import Data.Bits
main = print . U.sum . U.takeWhile (< (7::Int)).  U.enumFromTo 1 $ 10000000

    -- U.replicate 1000000 $ (7 :: Int)

    -- gets removed entirely!
