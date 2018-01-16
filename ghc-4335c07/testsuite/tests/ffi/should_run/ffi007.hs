-- !!! Test System.Exit.exitWith called from a foreign import "wrapper"

import Foreign
import Foreign.C

import System.Exit

type IOF = IO ()
foreign import ccall "wrapper" wrap_f_io :: IOF -> IO (FunPtr IOF)
foreign import ccall "dynamic" call_io :: FunPtr IOF -> IOF

exit = do putStrLn "exiting..."; exitWith ExitSuccess

main = do f <- wrap_f_io exit; call_io f
