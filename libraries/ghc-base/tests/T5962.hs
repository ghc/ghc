module Main where

import Data.Typeable

unitToUnit_a = typeOf (\() -> ())
unitToUnit_b = mkFunTy (typeOf ()) (typeOf ())

main = print (unitToUnit_a == unitToUnit_b)
