import qualified Data.Vector as U
main = print . U.sum $ U.cons (0xdeadbeef::Int) U.empty

