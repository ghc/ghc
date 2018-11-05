import Control.Exception (evaluate)
import Data.List

-- The following will fuse with minimal heap usage provided
-- `findIndices` is marked `INLINABLE` instead of `INLINE`.

unsafeFindIndex p = head . findIndices p

main = do evaluate $ elemIndex 999999 [(1::Int)..1000000]
          evaluate $ elemIndices 999999 [(1::Int)..1000000]
          evaluate $ findIndex (>=999999) [(1::Int)..1000000]
          evaluate $ findIndices (>=999999) [(1::Int)..1000000]
          evaluate $ unsafeFindIndex (>=999999) [(1::Int)..1000000]
