import qualified Data.Array.Unboxed as UA
import Data.Word

main :: IO ()
main = print (UA.listArray (1, 2^(20::Int)) (repeat 0)
              :: UA.UArray Int Word64)
       -- this unboxed array should at least take:
       --   2^20 * 64 bits
       -- = 8 * (2^20 bytes)
       -- = 8 MiB (in heap)
