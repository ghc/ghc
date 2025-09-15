-- | Verify that a default export doesn't export the class name as well.

import ExportBitsInt
import Data.Bits (zeroBits)

main = print z

z :: Bits a => a
z = zeroBits
