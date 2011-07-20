-- exposed a bug in GHC 6.4 threaded RTS, fixed in Schedule.c rev. 1.232

module Main where

import Control.Concurrent
import Control.Monad
import Foreign.Ptr
import Data.IORef

main = do
  ms <- replicateM 100 $ do putStrLn "." 
       		      	    m <- newEmptyMVar 
			    forkOS (thread >> putMVar m ())
			    thread
			    return m
  mapM takeMVar ms

thread = do var <- newIORef 0
            let f = modifyIORef var (1+)
            callC =<< mkFunc f

type FUNC  =  IO ()

foreign import ccall unsafe "wrapper"
   mkFunc :: FUNC -> IO (FunPtr FUNC)

foreign import ccall safe "ffi014_cbits.h callC"
   callC:: FunPtr FUNC -> IO ()

