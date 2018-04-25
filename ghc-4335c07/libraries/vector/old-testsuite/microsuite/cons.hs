import qualified Data.Vector as U
main = print . U.sum . U.cons 0xdeadbeef . U.replicate (100000000::Int) $ (8::Int)

