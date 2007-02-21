import GHC.Base (breakpoint)
import Control.Exception

f :: IO Bool
f = handle (\_->return False) (breakpoint$ return True)

