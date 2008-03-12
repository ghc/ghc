-- exposed a bug in GHC 6.4 threaded RTS, fixed in Schedule.c rev. 1.232

module Main where

import Control.Concurrent
import Control.Monad
import Foreign.Ptr
import Data.IORef

main = replicateM 100 (putStrLn "." >> forkOS thread >> thread)

thread = do var <- newIORef 0
            let f = modifyIORef var (1+)
            callC =<< mkFunc f

type FUNC  =  IO ()

foreign import ccall unsafe "wrapper"
   mkFunc :: FUNC -> IO (FunPtr FUNC)

foreign import ccall threadsafe "ffi014_cbits.h callC"
   callC:: FunPtr FUNC -> IO ()

