module T24331 where

import GHC.Float
import GHC.Word

a :: Word64
a = castDoubleToWord64 1.0

b :: Word32
b = castFloatToWord32 2.0

c :: Double
c = castWord64ToDouble 4621819117588971520

d :: Float
d = castWord32ToFloat 1084227584
