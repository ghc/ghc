module GHC.Runtime.Interpreter.Process
  (
  -- * Message API
    Message(..)
  , sendMessage
  , sendTHComputation
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
import Control.Concurrent
import Data.IORef
import Control.Concurrent.STM

-- -----------------------------------------------------------------------------
-- Top-level Message API

-- | Send a message to the interpreter and expect a response, blocking until it arrives.
--
-- This issues an asynchronous RPC on the external interpreter which does not
-- block the external interpreter thread. We block here on the host because we
-- wait for the asynchronous response to arrive.
sendMessage :: Binary a => ExtInterpInstance d -> Message a -> IO a
sendMessage i msg =
  remoteCall (interpPipe proc) msg
    `catchException` \(e :: SomeException) -> handleInterpProcessFailure proc e
  where
    proc = instProcess i

-- | Send a message with a TH computation to the ext. interpreter (like
-- 'RunTH', or 'RunModFinalizers') and handle 'THMessage' requests from the
-- ext. interpreter to us in the meantime.
--
-- This function blocks until the TH computation returns a value, but spawns in
-- the background a thread to handle the 'THMessage's until the TH computation
-- is fully done.
sendTHComputation
  :: Binary a
  => ExtInterpInstance d
  -> (InboxId -> Message a)
  -> (forall b. Binary b => THMessage b -> IO (Bool, b))
  -- ^ Handle one message. Return True to end the loop.
  -- This handler must never uninterrutibly mask an expression which may get
  -- stuck. If it does, then 'sendTHComputation' will also get uninterruptibly stuck.
  -> IO a
sendTHComputation i msg reqHandler = mask $ \restore -> do
  -- inline "withAsync/wait"
  var  <- newEmptyTMVarIO
  mbox <- newMailbox (pipeAsyncMngr (interpPipe proc))
  ht   <- forkFinally (restore (handlerLoop mbox)) (atomically . putTMVar var)
  restore (sendMessage i (msg mbox) <* atomically (readTMVar var))
    `onException` uninterruptibleMask_ (throwTo ht CancelTHHandler)
  where
    proc = instProcess i
    handleOne mbox doneRef = do
      handleRemoteTHCall (interpPipe proc) mbox $ \req -> do
        (stop, res) <- reqHandler req
        writeIORef doneRef stop
        return res
    handlerLoop mbox = do
      doneRef <- newIORef False
      let loop = do
            handleOne mbox doneRef
            isDone <- readIORef doneRef
            if isDone then return ()
                      else loop
       in loop

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

data CancelTHHandler = CancelTHHandler deriving Show
instance Exception CancelTHHandler where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException
