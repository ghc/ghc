
import qualified Data.Vector.Unboxed as U
main = print ((==) (U.replicate 100000000 True)
                   (U.replicate 100000000 True))


