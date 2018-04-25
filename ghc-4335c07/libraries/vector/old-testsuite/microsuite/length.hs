import qualified Data.Vector as U
main = print . U.length . U.enumFromTo 1 $ (100000000 :: Int)

