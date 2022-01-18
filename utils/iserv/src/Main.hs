{-# LANGUAGE CPP, GADTs #-}

-- |
-- The Remote GHCi server.
--
-- For details on Remote GHCi, see Note [Remote GHCi] in
-- compiler/GHC/Runtime/Interpreter.hs.
--
module Main (main) where

import IServ (serv)

import GHCi.Message
import GHCi.Signals
import GHCi.Utils

import Control.Exception
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.IORef
import System.Environment
import System.Exit
import Text.Printf
#if defined(WINDOWS)
import Foreign.Ptr (wordPtrToPtr)
# if defined(__IO_MANAGER_WINIO__)
import GHC.IO.SubSystem ((<!>))
import GHC.IO.Handle.Windows (handleToHANDLE)
import GHC.Event.Windows (associateHandle')
# endif
#endif

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
  (outh, inh, rest) <-
      case args of
        arg0:arg1:rest -> do
#if defined(WINDOWS)
            let wfd1 = wordPtrToPtr (read arg0)
                rfd2 = wordPtrToPtr (read arg1)
# if defined(__IO_MANAGER_WINIO__)
            -- register the handles we received with
            -- our I/O manager otherwise we can't use
            -- them correctly.
            return () <!> (do
              associateHandle' wfd1
              associateHandle' rfd2)
# endif
#else
            let wfd1 = read arg0
                rfd2 = read arg1
#endif
            inh  <- getGhcHandle rfd2
            outh <- getGhcHandle wfd1
            return (outh, inh, rest)
        _ -> dieWithUsage

  (verbose, rest') <- case rest of
    "-v":rest' -> return (True, rest')
    _ -> return (False, rest)

  (wait, rest'') <- case rest' of
    "-wait":rest'' -> return (True, rest'')
    _ -> return (False, rest')

  unless (null rest'') $
    dieWithUsage

  when verbose $
    printf "GHC iserv starting (in: %s; out: %s)\n" (show inh) (show outh)
  installSignalHandlers
  lo_ref <- newIORef Nothing
  let pipe = Pipe{pipeRead = inh, pipeWrite = outh, pipeLeftovers = lo_ref}

  when wait $ do
    when verbose $
      putStrLn "Waiting 3s"
    threadDelay 3000000

  uninterruptibleMask $ serv verbose hook pipe

  where hook = return -- empty hook
    -- we cannot allow any async exceptions while communicating, because
    -- we will lose sync in the protocol, hence uninterruptibleMask.
