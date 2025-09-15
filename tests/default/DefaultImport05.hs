-- | Verify that a default can be imported and be in effect even if the class is out of scope.

{-# LANGUAGE Haskell2010 #-}

import ExportBitsInt
import Data.Bits (zeroBits)

main = print zeroBits
