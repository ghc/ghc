{-# OPTIONS -fno-implicit-prelude  #-}

import PrelIOBase
import PrelIO
import PrelBase
import PrelAddr

foreign import "libHS_cbits" "getErrStr__"  unsafe ggetErrStr__  :: Int -> IO Addr 

main = putStr (uunsafePerformIO (ggetErrStr__ 4))

uunsafePerformIO	:: IO Addr -> [Char]
uunsafePerformIO (IO m) = case m realWorld# of (# _, (A# r) #)   -> (unpackCString#  r)
