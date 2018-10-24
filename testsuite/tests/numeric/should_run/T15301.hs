import Numeric
import GHC.Natural

main = do
   -- test that GHC correctly compiles big Natural literals
   let x = 0xffffffffffffffffffffffff :: Natural
   print (showHex x "" == "ffffffffffffffffffffffff")
