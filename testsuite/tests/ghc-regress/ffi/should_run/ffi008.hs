-- !!! Test exceptions in a foreign import "wrapper"

import Foreign
import Foreign.C

import System.Exit

type IOF = IO ()
foreign import ccall "wrapper" wrap_f_io :: IOF -> IO (FunPtr IOF)
foreign import ccall "dynamic" call_io :: FunPtr IOF -> IOF

mk_error = error "this is an error"

main = do f <- wrap_f_io mk_error; call_io f
