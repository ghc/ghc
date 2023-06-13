module GHC.Runtime.Interpreter.Process
  (
  -- * Low-level API
    callInterpProcess
  , readInterpProcess
  , writeInterpProcess

  -- * Message API
  , Message(..)
  , DelayedResponse (..)
  , sendMessage
  , sendMessageNoResponse
  , sendMessageDelayedResponse
  , sendAnyValue
  , receiveAnyValue
  , receiveDelayedResponse
  , receiveTHMessage

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

data DelayedResponse a = DelayedResponse

-- | Send a message to the interpreter process that doesn't expect a response
sendMessageNoResponse :: ExtInterpInstance d -> Message () -> IO ()
sendMessageNoResponse i m = writeInterpProcess (instProcess i) (putMessage m)

-- | Send a message to the interpreter that excepts a response
sendMessage :: Binary a => ExtInterpInstance d -> Message a -> IO a
sendMessage i m = callInterpProcess (instProcess i) m

-- | Send a message to the interpreter process whose response is expected later
--
-- This is useful to avoid forgetting to receive the value and to ensure that
-- the type of the response isn't lost. Use receiveDelayedResponse to read it.
sendMessageDelayedResponse :: ExtInterpInstance d -> Message a -> IO (DelayedResponse a)
sendMessageDelayedResponse i m = do
  writeInterpProcess (instProcess i) (putMessage m)
  pure DelayedResponse

-- | Send any value
sendAnyValue :: Binary a => ExtInterpInstance d -> a -> IO ()
sendAnyValue i m = writeInterpProcess (instProcess i) (put m)

-- | Expect a value to be received
receiveAnyValue :: ExtInterpInstance d -> Get a -> IO a
receiveAnyValue i get = readInterpProcess (instProcess i) get

-- | Expect a delayed result to be received now
receiveDelayedResponse :: Binary a => ExtInterpInstance d -> DelayedResponse a -> IO a
receiveDelayedResponse i DelayedResponse = readInterpProcess (instProcess i) get

-- | Expect a value to be received
receiveTHMessage :: ExtInterpInstance d -> IO THMsg
receiveTHMessage i = receiveAnyValue i getTHMessage


-- -----------------------------------------------------------------------------
-- Low-level API

-- | Send a 'Message' and receive the response from the interpreter process
callInterpProcess :: Binary a => InterpProcess -> Message a -> IO a
callInterpProcess i msg =
  remoteCall (interpPipe i) msg
    `catchException` \(e :: SomeException) -> handleInterpProcessFailure i e

-- | Read a value from the interpreter process
readInterpProcess :: InterpProcess -> Get a -> IO a
readInterpProcess i get =
  readPipe (interpPipe i) get
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
