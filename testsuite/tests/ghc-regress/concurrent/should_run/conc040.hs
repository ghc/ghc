{-# OPTIONS -fffi #-}

import Foreign
import Data.IORef
import Control.Concurrent
import Control.Exception

foreign import ccall "wrapper"
  wrap :: IO () -> IO (FunPtr (IO ()))

foreign import ccall "dynamic" 
  invoke :: FunPtr (IO ()) -> IO ()

{-# NOINLINE m #-}
m :: IORef ThreadId
m = unsafePerformIO (newIORef (error "m"))

main = do
  id <- myThreadId
  writeIORef m id
  raise' <- wrap raise
  invoke raise'

raise = do
  id <- readIORef m
  throwTo id (ErrorCall "kapow!")
