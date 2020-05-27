{-# LANGUAGE CPP, GADTs #-}

-- |
-- The Remote GHCi server.
--
-- For details on Remote GHCi, see Note [Remote GHCi] in
-- compiler/GHC/Runtime/Interpreter.hs.
--
module Main (main) where

import Lib (serv)

import GHCi.Message
import GHCi.Signals
import GHCi.Utils

import Control.Exception
import Control.Monad
import Data.IORef
import System.Environment
import System.Exit
import Text.Printf

dieWithUsage :: IO a
dieWithUsage = do
    prog <- getProgName
    die $ prog ++ ": " ++ msg
  where
#if defined(WINDOWS)
    msg = "usage: iserv <write-handle> <read-handle> [-v]"
#else
    msg = "usage: iserv <write-fd> <read-fd> [-v]"
#endif

main :: IO ()
main = do
  args <- getArgs
  (wfd1, rfd2, rest) <-
      case args of
        arg0:arg1:rest -> do
            let wfd1 = read arg0
                rfd2 = read arg1
            return (wfd1, rfd2, rest)
        _ -> dieWithUsage

  verbose <- case rest of
    ["-v"] -> return True
    []     -> return False
    _      -> dieWithUsage
  when verbose $
    printf "GHC iserv starting (in: %d; out: %d)\n"
      (fromIntegral rfd2 :: Int) (fromIntegral wfd1 :: Int)
  inh  <- getGhcHandle rfd2
  outh <- getGhcHandle wfd1
  installSignalHandlers
  lo_ref <- newIORef Nothing
  let pipe = Pipe{pipeRead = inh, pipeWrite = outh, pipeLeftovers = lo_ref}
  uninterruptibleMask $ serv verbose hook pipe

  where hook = return -- empty hook
    -- we cannot allow any async exceptions while communicating, because
    -- we will lose sync in the protocol, hence uninterruptibleMask.

