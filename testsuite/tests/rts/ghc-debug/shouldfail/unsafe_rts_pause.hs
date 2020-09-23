{-# LANGUAGE ForeignFunctionInterface #-}

import Data.Word
import Data.IORef
import GHC.Clock
import Control.Concurrent
import Foreign.Ptr
import System.Mem
import Control.Monad

data Capability

foreign import ccall unsafe "RtsAPI.h rts_pause"
    unsafe_rts_pause_c :: IO (Ptr Capability)

main :: IO ()
main = do
  putStrLn "Making a unsafe call to rts_pause() should fail. We \
           \cannot allow this haskell thread to continue if the RTS is paused."
  _ <- unsafe_rts_pause_c
  putStrLn "Oops! Haskell thread has continued even though RTS was paused."
