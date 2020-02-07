import Control.Exception (evaluate)
import qualified Data.List as L

-- The following will fuse with minimal heap usage provided
-- `findIndices` is marked `INLINABLE` instead of `INLINE`.

unsafeFindIndex p = head . L.findIndices p

main = do evaluate $ L.elemIndex 999999 [(1::Int)..1000000]
          evaluate $ L.elemIndices 999999 [(1::Int)..1000000]
          evaluate $ L.findIndex (>=999999) [(1::Int)..1000000]
          evaluate $ L.findIndices (>=999999) [(1::Int)..1000000]
          evaluate $ unsafeFindIndex (>=999999) [(1::Int)..1000000]
