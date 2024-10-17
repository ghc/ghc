{-# LANGUAGE LambdaCase #-}
module GHC.Runtime.Interpreter.Process
  (
  -- * Message API
    Message(..)
  , DelayedResponse (..)
  -- * Top-level message API (these acquire/release a lock)
  , sendMessage
  , sendMessageNoResponse
  , sendMessageDelayedResponse
  , receiveDelayedResponse
  -- * Nested message API (these require the interpreter to already be locked)
  , sendAnyValue
  , receiveAnyValue
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
import Control.Concurrent.MVar (MVar, withMVar, takeMVar, putMVar, isEmptyMVar)

data DelayedResponse a = DelayedResponse

-- -----------------------------------------------------------------------------
-- Top-level Message API

-- | Send a message to the interpreter process that doesn't expect a response
--   (locks the interpreter while sending)
sendMessageNoResponse :: ExtInterpInstance d -> Message () -> IO ()
sendMessageNoResponse i m =
  withLock i $ writeInterpProcess (instProcess i) (putMessage m)

-- | Send a message to the interpreter that expects a response
--   (locks the interpreter while until the response is received)
sendMessage :: Binary a => ExtInterpInstance d -> Message a -> IO a
sendMessage i m = withLock i $ callInterpProcess (instProcess i) m

-- | Send a message to the interpreter process whose response is expected later
--
-- This is useful to avoid forgetting to receive the value and to ensure that
-- the type of the response isn't lost. Use receiveDelayedResponse to read it.
-- (locks the interpreter until the response is received using
-- `receiveDelayedResponse`)
sendMessageDelayedResponse :: ExtInterpInstance d -> Message a -> IO (DelayedResponse a)
sendMessageDelayedResponse i m = do
  lock i
  writeInterpProcess (instProcess i) (putMessage m)
  pure DelayedResponse

-- | Expect a delayed result to be received now
receiveDelayedResponse :: Binary a => ExtInterpInstance d -> DelayedResponse a -> IO a
receiveDelayedResponse i DelayedResponse = do
  ensureLocked i
  r <- readInterpProcess (instProcess i) get
  unlock i
  pure r

-- -----------------------------------------------------------------------------
-- Nested Message API

-- | Send any value (requires locked interpreter)
sendAnyValue :: Binary a => ExtInterpInstance d -> a -> IO ()
sendAnyValue i m = ensureLocked i >> writeInterpProcess (instProcess i) (put m)

-- | Expect a value to be received (requires locked interpreter)
receiveAnyValue :: ExtInterpInstance d -> Get a -> IO a
receiveAnyValue i get = ensureLocked i >> readInterpProcess (instProcess i) get

-- | Wait for a Template Haskell message (requires locked interpreter)
receiveTHMessage :: ExtInterpInstance d -> IO THMsg
receiveTHMessage i = ensureLocked i >> receiveAnyValue i getTHMessage

-- -----------------------------------------------------------------------------

getLock :: ExtInterpInstance d -> MVar ()
getLock = interpLock . instProcess

withLock :: ExtInterpInstance d -> IO a -> IO a
withLock i f = withMVar (getLock i) (const f)

lock :: ExtInterpInstance d -> IO ()
lock i = takeMVar (getLock i)

unlock :: ExtInterpInstance d -> IO ()
unlock i = putMVar (getLock i) ()

ensureLocked :: ExtInterpInstance d -> IO ()
ensureLocked i =
  isEmptyMVar (getLock i) >>= \case
    False -> panic "ensureLocked: external interpreter not locked"
    _     -> pure ()


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
