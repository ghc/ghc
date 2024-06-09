{-# LANGUAGE CPP #-}
import Control.Exception (assert)

main = 
    if assertsEnabled 
    then putStrLn "Assertions Enabled" 
    else putStrLn "Assertions Ignored"

assertsEnabled :: Bool
#ifdef __GLASGOW_HASKELL_ASSERTS_IGNORED__
assertsEnabled = False
#else
assertsEnabled = True
#endif
