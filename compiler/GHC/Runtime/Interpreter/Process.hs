module GHC.Runtime.Interpreter.Process
  (
  -- * Message API
    Message(..)
  , RemoteResource(..)
  , sendMessage
  , sendTHComputation
  )
where

import GHC.Prelude

import GHC.Runtime.Interpreter.Types
import GHCi.Message

import GHC.Utils.Exception as Ex

import Control.Concurrent
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
  remoteCall (interpPipe proc) msg (disposeMessageResults i msg)
  where
    proc = instProcess i

-- | Reclaim the resources in a reply that arrived after the request was
-- canceled, so nobody is going to take ownership of them.
disposeMessageResults :: ExtInterpInstance d -> Message a -> a -> IO ()
disposeMessageResults i msg result = do
  -- TODO: seqList this
  let ress = resultRemoteResources msg result
  modifyMVar_ (instPendingFrees i) (pure . (ress ++))

-- | Send a message with a TH computation to the ext. interpreter (like
-- 'RunTH', or 'RunModFinalizers') and handle 'THMessage' requests from the
-- ext. interpreter to us until done, then wait for the response.
sendTHComputation
  :: Binary a
  => ExtInterpInstance d
  -> (InboxId -> Message a)
  -> (forall b. Binary b => (forall a. IO a -> IO a) -> THMessage b -> IO (Bool, b))
  -- ^ Handle one message. Return True to end the loop.
  -> IO a
sendTHComputation i mkMsg reqHandler = mask $ \ restore -> do
  mbox <- newMailbox (pipeAsyncMngr (interpPipe proc))
  let msg = mkMsg mbox
  res <- remoteCallInterleaving (interpPipe proc) msg
     -- handle reverse requests until done
     (restore $ handlerLoop mbox)
     (dispose msg mbox)
  disposeMailbox mbox
  either throwIO pure res
  where
    disposeMailbox mbox = do
      -- non-blocking
      -- ext. interp. will cleanup on error response, so we don't need reqs.
      _reqs <- closeMailbox (interpPipe proc) mbox
      pure ()
    dispose msg mbox res = do
      disposeMailbox mbox
      case res of
        -- rethrown elsewhere where relevant.
        Left _   -> pure ()
        Right r -> disposeMessageResults i msg r
    proc = instProcess i
    handleOne mbox doneRef = mask $ \ restore -> do
      handleRemoteTHCall (interpPipe proc) mbox $ \_setCancel req -> do
        (stop, res) <- reqHandler restore req
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
