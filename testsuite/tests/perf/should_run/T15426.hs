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

{- Note; see !7997.

You would think those [1..100000] sub-expressions would float to the
top level, be CSE'd, and shared.

But no: until May 22-ish, they are the argument of a strict function
findIndices; and in HEAD SetLevels goes to some trouble not to float
strict arguments. So in HEAD, no sharing happens.

I think the reasoning is bogus, so I changed in; see
"Arguments" in Note [Floating to the top] in SetLevels.

As a result these lists are now floated out and shared.

Just leaving breadcrumbs, in case we later see big perf changes on
this (slightly fragile) benchmark.
-}