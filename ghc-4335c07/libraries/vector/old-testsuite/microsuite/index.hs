import qualified Data.Vector as U
main = print . (\arr -> arr U.! 42) . U.map (subtract 6) . U.replicate 10000000 $ (7 :: Int)

