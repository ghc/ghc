import qualified Data.Vector as U -- Parallel.Unlifted
main = print . U.sum . U.map fstS . indexedU . U.enumFromTo 1 $ (100000000 :: Int)

