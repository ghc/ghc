{-# LANGUAGE ForeignFunctionInterface #-}
-- Measure raw callback performance.

module Main where

import Control.Concurrent
import Control.Monad
import Foreign
import Foreign.C
import Data.IORef
import System.Environment
import System.IO

main = do
  [s] <- getArgs
  poke pcount (fromIntegral (read s))
  callC =<< mkFunc (return ())

type FUNC  =  IO ()

foreign import ccall "&count" pcount :: Ptr CInt

foreign import ccall unsafe "wrapper"
   mkFunc :: FUNC -> IO (FunPtr FUNC)

foreign import ccall safe "cbits.h callC"
   callC:: FunPtr FUNC -> IO ()

