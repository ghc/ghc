module GHC.Runtime.Interpreter.Process
  (
  -- * Message API
    Message(..)
  -- * Top-level message API
  , sendMessage
  , sendMessageNoResponse
  )
where

import GHC.Prelude

import GHC.Runtime.Interpreter.Types
import GHCi.Message

import GHC.IO (catchException)
import GHC.Utils.Panic
import GHC.Utils.Exception as Ex

import Data.Binary
import System.Exit
import System.Process

-- -----------------------------------------------------------------------------
-- Top-level Message API

-- | Send a message to the interpreter process that doesn't expect a response
--   (locks the interpreter while sending)
sendMessageNoResponse :: ExtInterpInstance d -> Message () -> IO ()
sendMessageNoResponse i m = writeInterpProcess (instProcess i) (putMessage m)

-- | Send a message to the interpreter that expects a response
--   (locks the interpreter while until the response is received)
sendMessage :: Binary a => ExtInterpInstance d -> Message a -> IO a
sendMessage i m = callInterpProcess (instProcess i) m

-- | Send a 'Message' and receive the response from the interpreter process
callInterpProcess :: Binary a => InterpProcess -> Message a -> IO a
callInterpProcess i msg =
  remoteCall (interpPipe i) msg
    `catchException` \(e :: SomeException) -> handleInterpProcessFailure i e

-- | Send a value to the interpreter process
writeInterpProcess :: InterpProcess -> Put -> IO ()
writeInterpProcess i put =
  writePipe (interpPipe i) put
    `catchException` \(e :: SomeException) -> handleInterpProcessFailure i e

handleInterpProcessFailure :: InterpProcess -> SomeException -> IO a
handleInterpProcessFailure i e = do
  let hdl = interpHandle i
  ex <- getProcessExitCode hdl
  case ex of
    Just (ExitFailure n) ->
      throwIO (InstallationError ("External interpreter terminated (" ++ show n ++ ")"))
    _ -> do
      terminateProcess hdl
      _ <- waitForProcess hdl
      throw e
