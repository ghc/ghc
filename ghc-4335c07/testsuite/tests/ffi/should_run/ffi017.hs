
-- Test for trac #953

module Main where

import System.Posix.Types
import Foreign.C

foreign import ccall safe "unistd.h lseek" foo :: CInt -> COff -> CInt -> IO COff

main :: IO ()
main = return ()

