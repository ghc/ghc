import qualified Data.Vector as U
main = print . head . U.toList . U.fromList $ replicate 1 (7::Int)
