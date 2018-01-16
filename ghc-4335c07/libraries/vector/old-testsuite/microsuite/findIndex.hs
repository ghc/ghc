import qualified Data.Vector as U
import Data.Bits
main = print . U.findIndex (==100) . U.map (`shiftL` 1) . U.enumFromTo 1 $ (10000 :: Int)

