-- | Unsafe as we explicitly marked UnsafeInfered09_B as unsafe
module UnsafeInfered09_A where

import UnsafeInfered09_B

g :: Int
g = f

