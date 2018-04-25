import qualified Data.Vector as U
main = print . U.sum $ U.cons (10::Int) (U.singleton 2)

