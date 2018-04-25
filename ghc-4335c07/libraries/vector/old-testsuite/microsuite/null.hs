import qualified Data.Vector as U
main = print . U.null . U.filter (>10) . U.map (subtract 6) . U.enumFromTo 1 $ (100000000 :: Int)

