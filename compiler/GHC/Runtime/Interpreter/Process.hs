module GHC.Runtime.Interpreter.Process
  (
  -- * Message API
    Message(..)
  , sendMessage
  , sendTHComputation
  , sendMessageWithResponse
  )
where

import GHC.Prelude

import GHC.Runtime.Interpreter.Types
import GHCi.Message

import GHC.Utils.Panic
import GHC.Utils.Exception as Ex

import Data.Binary
import Data.IORef

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
  where
    proc = instProcess i

sendMessageWithResponse :: Binary a => ExtInterpInstance c -> Message a -> (IO a -> IO b) -> IO b
sendMessageWithResponse i msg k =
  remoteCallWithResponse (interpPipe proc) msg k
  where
    proc = instProcess i

-- | Send a message with a TH computation to the ext. interpreter (like
-- 'RunTH', or 'RunModFinalizers') and handle 'THMessage' requests from the
-- ext. interpreter to us until done, then wait for the response.
sendTHComputation
  :: Binary a
  => ExtInterpInstance d
  -> (InboxId -> Message a)
  -> (forall b. Binary b => THMessage b -> IO (Bool, b))
  -- ^ Handle one message. Return True to end the loop.
  -> IO a
sendTHComputation i msg reqHandler =
  withNewMailbox (pipeAsyncMngr (interpPipe proc)) $ \ mbox ->
    sendMessageWithResponse i (msg mbox) $ \ waitRes -> do
      handlerLoop mbox
      waitRes
  where
    proc = instProcess i
    handleOne mbox doneRef = do
      handleRemoteTHCall (interpPipe proc) mbox $ \_setCancel req -> do
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

data CancelTHHandler = CancelTHHandler deriving Show
instance Exception CancelTHHandler where
  fromException = asyncExceptionFromException
  toException = asyncExceptionToException
