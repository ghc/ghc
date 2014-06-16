-- | Trivial Unsafe Module
module UnsafeInfered01_A where

import System.IO.Unsafe

f :: IO a -> a
f = unsafePerformIO

