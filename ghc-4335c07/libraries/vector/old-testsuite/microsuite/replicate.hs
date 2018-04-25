import qualified Data.Vector as U
main = print . U.sum . U.map (subtract 7) . U.replicate 10000000 $ (7 :: Int)

