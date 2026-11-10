{-# LANGUAGE RecursiveDo, GADTs, DeriveGeneric, StandaloneDeriving, ScopedTypeVariables,
    GeneralizedNewtypeDeriving, ExistentialQuantification, RecordWildCards, CPP,
    NamedFieldPuns, LambdaCase, TypeAbstractions, TypeApplications,
    LinearTypes, RankNTypes, UnboxedTuples, BlockArguments, TupleSections, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
module GHCi.Message.Async
  (
    -- * Pipes
    Pipe(..), mkPipeFromHandles, mkPipeFromContinuations
  , closePipe, PipeClosedException(..)

    -- * Interface for async bidirectional RPC
  , remoteAsyncReq, handleAsyncReq
  , remoteAsyncReqWithResponse
  , AsyncReqResult(..), getAsyncReqReply
  , RemoteProcessException(..), DecodeAsyncReqException(..)

    -- ** Mailboxes
  , InboxId(..)
  , newMailbox
  , closeMailbox

    -- ** Async matching internals
  , AsyncManager(..), CorrelationId
  , IddMsg(..), newAsyncManager

    -- ** Consume `reply` exactly once
  , WriteReply(..), writeReply
  , runWriteReply, (|#>), (%>>=)
  , ReplyResult (..)

    -- * Serializable exceptions
  , SerializableException(..)
  , toSerializableException, fromSerializableException, closingPipe
  ) where

import Prelude -- See note [Why do we import Prelude here?]
import GHCi.TH.Binary () -- For Binary instances

import GHC.IO (IO(..))
import Control.Concurrent
import Control.Exception
#if MIN_VERSION_base(4,20,0)
import Control.Exception.Context
#endif
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LB
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import GHC.Generics
import System.Exit
import System.IO
import Control.Monad
import Control.Concurrent.STM
import Data.Bifunctor
import GHC.Base (State#)
import GHC.Exts (RealWorld)

--------------------------------------------------------------------------------
-- * Reading/writing and Pipes
--------------------------------------------------------------------------------

{- | Datatype describing the possible states of a pipe end.

'WillClose' is set when communication is meant to stop, so we exit cleanly even
 if exceptions happen during shutdown, e.g. peer closed its read end before our last reply.

-}
data PipeEndStatus
  = Open                          -- ^ Starting state, ready.
  | WillClose                     -- ^ About to close, expect peer to close its own pipe end soon.
  | Closed !(Maybe SomeException) -- ^ No new read/writes allowed,
                                  -- exception stored if transitioned here from
                                  -- 'Open'.

isOpen :: PipeEndStatus -> Bool
isOpen = \case
  Open -> True
  _    -> False

-- | Returns whether a read/write is still allowed.
--
-- 'True' for 'WillClose', so e.g. a final ack can get through.
isActive :: PipeEndStatus -> Bool
isActive = \case
  Open      -> True
  WillClose -> True
  _         -> False


-- | An opaque pipe for bidirectional binary data transmission.
data Pipe = Pipe
  { getSome         :: !(IO ByteString)
  , putAll          :: !(B.Builder -> IO ())
  , pipeLeftovers   :: !(IORef (Maybe ByteString))
  , pipeAsyncMngr   :: !AsyncManager
  , pipeCloseRead   :: !(IO ())
  -- ^ closes read end of the pipe
  , pipeCloseWrite  :: !(IO ())
  -- ^ closes write end of the pipe
  , pipeReadStatus  :: !(TVar PipeEndStatus)
    -- ^ After the pipe is closed, messages can no longer be received from the
    -- other end, but the 'AsyncManager' queues might not be empty yet.
  , pipeWriteStatus :: !(TVar PipeEndStatus)
    -- ^ After the write end of the pipe is closed we can no longer submit
    -- requests or respond to ones we receive, but we might still receive
    -- replies.
  }

-- | Make a 'Pipe' from a 'Handle' to read and a 'Handle' to write.
-- Must call 'closePipe' at the end to clean up.
mkPipeFromHandles :: Handle -> Handle -> IO Pipe
mkPipeFromHandles pipeRead pipeWrite = do
  let getSome = B.hGetSome pipeRead (32*1024)
      putAll b = do
        B.hPutBuilder pipeWrite b
        hFlush pipeWrite
  mkPipeFromContinuations getSome putAll (hClose pipeRead) (hClose pipeWrite)

-- | Make a 'Pipe' from a reader function and a writer function.
-- The pipe should be closed with 'closePipe', which will call 'pipeCloseRead' and 'pipeCloseWrite'.
--
-- Arguments 'getSome' and 'putAll' should throw an `IOException` when communication fails
-- or is no longer possible.
mkPipeFromContinuations :: IO ByteString
                        -- ^ getSome
                        -> (B.Builder -> IO ())
                        -- ^ putAll
                        -> IO ()
                        -- ^ pipeCloseRead
                        -> IO ()
                        -- ^ pipeCloseWrite
                        -> IO Pipe
mkPipeFromContinuations getSome putAll pipeCloseRead pipeCloseWrite = mdo
  pipeLeftovers   <- newIORef Nothing
  pipeReadStatus  <- newTVarIO Open
  pipeWriteStatus  <- newTVarIO Open
  mdo
    pipeAsyncMngr <- newAsyncManager p
    let p = Pipe { .. }
    pure p

-- | Close our ends of the pipe.
--
-- The ordering is subtle and important. We first stop the write thread, which
-- fails any pending writes, then closes the write end so the other side sees
-- EOF. Only then do we stop the drain thread which, once its read terminates,
-- fails any pending replies and then closes the read end. On Windows its read
-- can't be interrupted, so this waits for the other side to close its write end
-- too.
closePipe :: Pipe -> IO ()
closePipe Pipe{..} = do
  throwTo (asyncFillTid pipeAsyncMngr) PipeWriteClosedException
    `finally` throwTo (asyncDrainTid pipeAsyncMngr) PipeReadClosedException

closingPipeEnd :: TVar PipeEndStatus -> STM PipeEndStatus
closingPipeEnd v = readTVar v >>= \ s -> do
  case s of
    Open -> writeTVar v WillClose
    _    -> pure ()
  return s

deactivatePipeEnd :: TVar PipeEndStatus -> Maybe SomeException -> STM PipeEndStatus
deactivatePipeEnd v me = readTVar v >>= \ s -> do
  case s of
    Open      -> writeTVar v (Closed me)
    WillClose -> writeTVar v (Closed Nothing)
    _         -> pure ()
  pure s

closingPipe :: Pipe -> IO ()
closingPipe p = atomically $ do
  void $ closingPipeEnd (pipeReadStatus p)
  void $ closingPipeEnd (pipeWriteStatus p)

-- | Exception thrown/reported when attempting to read or write on a closed pipe.
--
-- Errors occurring while reading or writing will show up as the original
-- exception instead (e.g. IOException).
data PipeClosedException
  = PipeReadClosedException  -- ^ We've closed the pipe read end on this process

  | PipeWriteClosedException -- ^ We've closed the pipe write end on this process
  deriving Show
instance Exception PipeClosedException


--------------------------------------------------------------------------------
-- ** Pipe thread-safe read/write
--------------------------------------------------------------------------------
-- All messages on the wire are 'IddMsg', which contain a correlation header
-- and identify whether they are requests or replies.

_writePipe :: Pipe -> IddMsg -> IO ()
_writePipe p msg = do
  var <- maybe (throwIO PipeWriteClosedException) pure =<< atomically (writePipeAsync p msg)
  either throwIO pure =<< atomically (takeTMVar var)

-- | Enqueues the message for writing.
--
-- The returned TMVar can be checked for the outcome.
writePipeAsync :: Pipe -> IddMsg -> STM (Maybe (TMVar (Either SomeException ())))
writePipeAsync Pipe{..} msg = do
  var <- newEmptyTMVar
  isActive <$> readTVar pipeWriteStatus >>= \case
    False -> pure Nothing
    True  -> writeTQueue (writeQueue pipeAsyncMngr) (msg, var) >> pure (Just var)


-- | Only the 'drainPipe' thread should read from the pipe directly. Other
-- functions should wait for the matching reply to be delivered asynchronously.
-- (see 'withAsyncReq'). It doesn't lock to read bc it assumes single reader is 'drainPipe'.
--
-- Returns @Nothing@ on EOF.
readPipe :: Pipe -> IO (Maybe IddMsg)
readPipe Pipe{..} = do
  leftovers <- readIORef pipeLeftovers
  m <- getBin getSome get leftovers
  case m of
    Nothing -> return Nothing
    Just (result, new_leftovers) -> do
      writeIORef pipeLeftovers new_leftovers
      return (Just result)

getBin
  :: (IO ByteString) -> Get a -> Maybe ByteString
  -> IO (Maybe (a, Maybe ByteString))

getBin getsome get leftover = go leftover (runGetIncremental get)
 where
   go Nothing (Done leftover _ msg) =
     return (Just (msg, if B.null leftover then Nothing else Just leftover))
   go _ Done{} = throwIO (ErrorCall "getBin: Done with leftovers")
   go (Just leftover) (Partial fun) = do
     go Nothing (fun (Just leftover))
   go Nothing (Partial fun) = do
     -- putStrLn "before hGetSome"
     b <- getsome
     -- putStrLn $ "hGetSome: " ++ show (B.length b)
     if B.null b
        then return Nothing
        else go Nothing (fun (Just b))
   go _lft (Fail _rest _off str) =
     throwIO (ErrorCall ("getBin: " ++ str))

--------------------------------------------------------------------------------
-- * Matching requests with async replies
--------------------------------------------------------------------------------


-- | Send an asynchronous request and block waiting for a matching reply
--
-- See Note [Exceptions during serialization] about exceptions the 'Put' argument might raise.
remoteAsyncReq :: Pipe -> Put -> Maybe InboxId
               -> (Either SomeException LB.ByteString -> IO ())
               -- ^ What to do if a reply is received after the request is canceled.
               -> IO (Either SomeException LB.ByteString)
remoteAsyncReq pipe msg ii dispose = mask_ $
  getAsyncReqReply (pure ()) dispose =<< remoteAsyncReqWithResponse pipe msg ii


getAsyncReqReply :: IO ()
                 -> (Either SomeException LB.ByteString -> IO ())
                 -> AsyncReqResult
                 -> IO (Either SomeException LB.ByteString)
getAsyncReqReply middle dispose = \case
  NotWritten e -> throwIO e
  MaybeWritten written waitRes cancel ->
    (do atomically (written >>= either throwSTM pure)
        middle
        atomically waitRes)
      `onException` do
         -- Do not care about write outcome, since reply could be coming either way.
         void $ atomically cancel
         -- exception must be happening while stuck in waitRes at the latest,
         -- so waitRes can be called again.
         --
         -- We forkIO to wait, to keep cleanup bounded. If the pipe is
         -- irrecoverable it will close eventually and this thread will terminate.
         void $ forkIO (atomically waitRes >>= dispose)


data AsyncReqResult
     = NotWritten PipeClosedException
     | MaybeWritten
          (STM (Either SomeException ()))  -- ^ block for write outcome
          (STM (Either SomeException LB.ByteString))   -- ^ block for reply
          (STM (Maybe (STM (Either SomeException ())))) -- ^ send cancel


remoteAsyncReqWithResponse :: Pipe
                           -> Put
                           -> Maybe InboxId
                           -> IO AsyncReqResult
remoteAsyncReqWithResponse pipe msg ii = do
  -- Mark pending request
  uq <- freshId (pipeAsyncMngr pipe)
  wv <- newEmptyTMVarIO

  atomically $ readTVar (pipeReadStatus pipe) >>= \ readEnd -> case isActive readEnd of
    False  -> pure $ NotWritten PipeReadClosedException
    True -> writePipeAsync pipe (Request uq ii (runPut msg)) >>= \case
      Nothing -> pure $ NotWritten PipeWriteClosedException
      Just waitWritten -> do
        -- request queued, so we register a pending reply. After this tx we
        -- always expect a reply, and when it arrives 'drainPipe' will clean up 'pendingReplies'.
        modifyTVar pendingReplies (IntMap.insert uq wv)
        pure (MaybeWritten (takeTMVar waitWritten) (takeTMVar wv)
                           (fmap takeTMVar <$> writePipeAsync pipe (Cancel uq)))
  where
    pendingReplies = pendingReply (pipeAsyncMngr pipe)

-- | Receive a single asynchronous request and send a matching reply
-- asynchronously (the handler may fork a computation that eventually replies
-- and can return before actually replying, as long as it eventually does).
--
-- The handler receives as an argument the function to use to reply to the request.
-- It must be used exactly once (all requests expect a reply, and you can't
-- reply more than once). Linear types enforce the handler replies in every
-- branch, and if the handler otherwise throws, it is handled by 'handleAsyncReq'.
--
-- See Note [Exceptions during serialization] about exceptions the 'Put' argument might raise.
handleAsyncReq :: Pipe -> Get a -> (Maybe InboxId) -> (WriteReply Put %1 -> (IO () -> IO ()) -> a -> IO r) -> IO r
handleAsyncReq pipe getit mii k = mask $ \restore -> do
  let am = pipeAsyncMngr pipe
  ack <- newEmptyMVar

  -- Block waiting for request
  (uq,bs) <- atomically $ do
    readTVar (pipeWriteStatus pipe) >>= \ s -> case isActive s of
      -- We will not be able to write the reply, so no point in continuiting.
      False -> throwSTM PipeWriteClosedException
      True  -> pure ()
    taken <- case mii of
      Nothing -> tryReadTQueue  $  requestsQueue am
      Just ii -> maybe (throwSTM ClosedMailbox) tryReadTQueue =<< getMailbox am ii
    case taken of
      Just req            -> pure req
      Nothing  -> isActive <$> readTVar (pipeReadStatus pipe) >>= \case
        False -> throwSTM PipeReadClosedException -- can't expect reqs
        True  -> retry

  let
    reply p = do
      r <- atomically $ replyToReqAsync pipe uq p
      putMVar ack ()
      -- TODO: log r if Left{}
      case r of
        Nothing -> pure $ ReplyNotWritten
        Just mv -> atomically (takeTMVar mv) >>= \case
          Left e   -> pure $ ReplyWritingError e
          Right () -> pure ReplyWritingSuccess

  -- Run the continuation which must reply exactly once with 'writeReply'
  case runGetOrFail getit bs of
    Left (_, _, err) -> do
      let exc = DecodeAsyncReqException err
      void $ reply (Left (toException exc))
      throwIO exc
    Right (_, _, it) ->
      restore (k (WriteReply $ \r k' -> reply r >>= k') (setActive am uq) it)

        -- If there's an exception before the reply is sent, reply with an error instead.
        `catch` \(e :: SomeException) -> do

            tryTakeMVar ack >>= \case
              Just () ->
                -- Already replied
                throwIO e
              Nothing -> do
                -- Reply with an error
                void $ reply (Left e)
                throwIO e

replyToReqAsync :: Pipe -> CorrelationId -> Either SomeException Put
                -> STM (Maybe (TMVar (Either SomeException ())))
replyToReqAsync pipe uq p = do
  doneHandling am uq
  writePipeAsync pipe (Reply uq (bimap toSerializableException runPut p))
  where
    am = pipeAsyncMngr pipe

_replyToReq :: Pipe -> CorrelationId -> Either SomeException Put -> IO ()
_replyToReq pipe uq p = atomically (replyToReqAsync pipe uq p) >>= \case
  Nothing -> throwIO PipeWriteClosedException
  Just written -> atomically (readTMVar written) >>= either throwIO pure

-- | 'handleSyncReq' failed to decode an incoming request payload using the given @'Get' a@
newtype DecodeAsyncReqException = DecodeAsyncReqException String deriving Show
instance Exception DecodeAsyncReqException


-- ** Mailboxes ----------------------------------------------------------------

-- | A 'Request' may optionally declare an 'InboxId'. If it does, it can only
-- be read by a 'handleAsyncReq' for that specific 'InboxId'. That is,
-- @'handleAsyncReq' p get Nothing@ will not receive messages meant for a
-- specific inbox.
--
-- This is useful to prevent mixing up reverse requests, where we want to send
-- many reverse requests but have the caller handle them without interleaving
-- them.
newtype InboxId = InboxId Int deriving (Show, Binary)

-- | Create a new mailbox from where we can read all requests addressed to this 'InboxId'.
newMailbox :: AsyncManager -> IO InboxId
newMailbox am = do
  ii <- freshId am
  -- Created here, so it missing means the mailbox got closed.
  atomically $ do
    q <- newTQueue
    modifyTVar (mailboxQueues am) (IntMap.insert ii q)
  return $ InboxId ii

  -- `mailboxQueues` is written on demand, no need to do it here.

-- | Clean up a created mailbox by 'InboxId'.
--
--  Asynchronously replies with 'ClosedMailbox' exception to remaining requests.
closeMailbox :: Pipe -> InboxId -> IO [(LB.ByteString, Maybe (STM (Either SomeException ())))]
closeMailbox pipe@Pipe{pipeAsyncMngr = AsyncManager{..}} (InboxId ii) = atomically $ do
  boxes <- readTVar mailboxQueues
  case IntMap.lookup ii boxes of
    -- never opened? already closed?
    Nothing -> pure []
    Just q  -> do
      writeTVar mailboxQueues (IntMap.delete ii boxes)
      msgs <- flushTQueue q
      (`mapM` msgs) $ \(uq,msg) -> do
         (msg,) . fmap takeTMVar <$> replyToReqAsync pipe uq (Left $ toException $ ClosedMailbox)


data ClosedMailbox = ClosedMailbox deriving Show
instance Exception ClosedMailbox

-- | Get or create a new mailbox for this 'InboxId'
getMailbox :: AsyncManager -> InboxId -> STM (Maybe (TQueue (CorrelationId, LB.ByteString)))
getMailbox am (InboxId ii) = do
  mboxes <- readTVar (mailboxQueues am)
  pure $! IntMap.lookup ii mboxes

-- ** Async matching "internals" -----------------------------------------------

-- | Tracks handling status of a received 'Request'. The values are states of a
-- tiny state machine used to coordinate cancelations.
--
-- 'drainPipe' will push a request to the appropriate queue, and set the
-- status to `Received`. Then one of two things can happen first:
--  - we receive a 'Cancel', then status is set to `Canceling` so the cancel
--    action will run when set.
--  - handler sets a cancel action, then reception of a `Cancel` runs that
--    action and deletes the RequestStatus.
--
-- If RequestStatus is `Canceling` or absent, then a `Reply` won't be sent.
--
-- The two possible transition paths:
-- readPipe -> Received -> Active cancel -> gone
-- readPipe -> Received -> Canceling     -> gone

data RequestStatus
  = Received       -- ^ 'Received' from the 'Pipe'.
  | Active (IO ()) -- ^ Handler has set a cancel action, and has not sent a reply
                   -- yet. The cancel action must not block.
  | Canceling      -- ^ `Cancel` received while status is still `Received`.

data AsyncManager = AsyncManager
  { pendingReply :: !(TVar (IntMap (TMVar (Either SomeException LB.ByteString))))
    -- ^ When a request is sent, keep track that we're expecting a reply. The
    -- TMVar is signaled when the reply arrives. The exception can originate
    -- from the host process, or from the remote one (with 'RemoteProcessException').
  , requestsQueue :: !(TQueue (CorrelationId, LB.ByteString))
    -- ^ A queue of pending received requests. When a request is received
    -- /without/ a specific 'InboxId', queue it here. A request handler will
    -- pop the next request from it. (Nothing except the 'drainPipe' thread
    -- should read from the pipe). Read with @'handleAsyncReq' p get Nothing@.
  , cancelStatus :: !(TVar (IntMap RequestStatus))
    -- ^ When a `Cancel uq` is received, we lookup `uq` in this map and call the
    -- callback if present.
  , mailboxQueues :: !(TVar (IntMap (TQueue (CorrelationId, LB.ByteString))))
    -- ^ Requests for a specific 'InboxId' that can be read using
    -- @'handleAsyncReq' p get (Just <mailboxid>)@.
  , writeQueue    :: !(TQueue (IddMsg, TMVar (Either SomeException ())))
    -- ^ Messages to write on the pipe.
  , asyncDrainTid :: !ThreadId
  , asyncFillTid  :: !ThreadId
  , asyncFreshId  :: !(IORef Int)
  }

-- | Each 'Request' expects a 'Reply' with a matching 'CorrelationId'.
type CorrelationId = Int

-- | We have a single type for requests and replies because both the host and
-- interpreter may send requests *and* replies on the write end *and* on recv
-- both on the read end.
--
-- For instance:
--  1. The host sends a RunTH request on its Host Write End (HWE)
--  2. The interpreter sends a NewName TH request on its Interpreter Write End (IWE)
--  3. The host sends a reply to the TH request on its HWE
--  4. The interpreter sends the reply to the RunTH request on its IWE
--
-- Additionally, we want to keep concurrent TH requests for independent RunTH
-- calls separated. Thus the interpreter requests additionally specify a
-- mailbox announced by RunTH:
--  1. To interpreter:   @Request _ Nothing (RunTH <mailid>)@
--  2. From interpreter: @Request _ (Just <mailid>) NewName@
-- And the host will relate the <mailid> reverse requests to that RunTH run
-- exclusively.
data IddMsg  = Request !CorrelationId !(Maybe InboxId) LB.ByteString
             | Reply   !CorrelationId (Either SerializableException LB.ByteString)
             | Cancel  !CorrelationId -- ^ Technically a Request, but no Reply
                                      -- is expected, the CorrelationId is the
                                      -- one of the Request being canceled,
                                      -- which does still expect a Reply.

instance Binary IddMsg where
  put (Request i ii bs) = putWord8 0 >> put i >> put ii >> put bs
  put (Reply i bs)      = putWord8 1 >> put i >> put bs
  put (Cancel i)        = putWord8 2 >> put i

  get = do
    b <- getWord8
    case b of
      0 -> Request <$> get <*> get <*> get
      1 -> Reply   <$> get <*> get
      2 -> Cancel  <$> get
      _ -> fail $ "impossible: Binary IddMsg - " ++ show b

newAsyncManager :: Pipe -> IO AsyncManager
newAsyncManager pipe = do
  asyncFreshId  <- newIORef 0
  pendingReply  <- newTVarIO IntMap.empty
  requestsQueue <- newTQueueIO
  cancelStatus  <- newTVarIO IntMap.empty
  mailboxQueues <- newTVarIO IntMap.empty
  writeQueue <- newTQueueIO

  -- Fork thread to drain read end.
  -- (cleaned up on 'closePipe')
  asyncDrainTid <- mask_ $ forkIOWithUnmask (drainPipe pipe)

  -- Fork thread to fill write end.
  -- (cleaned up on 'closePipe')
  asyncFillTid <- mask_ $ forkIOWithUnmask (fillPipe pipe)

  let am = AsyncManager{..}
  pure am

-- | Drains the read end of the Pipe and match 'Reply'es against pending 'Request's.
--
-- In pratice, we'll have both a thread draining the read end of the host's
-- 'Pipe' and a thread draining the read end of the interpreter's 'Pipe', since
-- both processes construct a 'Pipe' and send and receive messages
--
-- Closes the Pipe read end when done reading
drainPipe :: Pipe -> (forall x. IO x -> IO x) -> IO ()
drainPipe p unmask = catch loop (void . replyToPendings) where
  loop = do
    unmask (readPipe p) >>= \case
      Just msg -> do
        handleMsg msg
        loop
      -- EOF on read handle
      Nothing -> do
        -- exit loop
        throwIO PipeReadClosedException
  handleMsg = \case
    -- Receive a reply to a pending notification
    Reply uq bs -> do
      atomically $ do
        m <- readTVar (pendingReply am)
        case IntMap.lookup uq m of
          Nothing ->
            -- pendingReply should only be cleared here, so something has gone wrong.
            -- TODO: logging.
            pure ()
          Just wv -> do
            writeTVar (pendingReply am) (IntMap.delete uq m)
            -- signal matching request
            putTMVar wv (bimap (toException . RemoteProcessException) id bs)
    -- Receive a request and queue it
    Request uq mbi bs -> do
      atomically $ receivedRequest am uq >> case mbi of
        Nothing ->
          -- "Global" request
          writeTQueue (requestsQueue am) (uq,bs)
        Just ii -> do
          -- "Specific mailbox" request
          mq <- getMailbox am ii
          case mq of
            Nothing -> void $ replyToReqAsync p uq (Left $ toException ClosedMailbox)
            Just q  -> writeTQueue q (uq,bs)
    Cancel uq -> cancelRequest am uq

  replyToPendings e = do
    atomically $ failPendingReplies p e
    pipeCloseRead p `catch` \(_ :: IOException) -> pure ()

  am = pipeAsyncMngr p

cancelRequest :: AsyncManager -> CorrelationId -> IO ()
cancelRequest am uq = join . atomically $ do
  cs <- readTVar (cancelStatus am)
  case IntMap.lookup uq cs of
    Nothing              -> pure (pure ())
    Just Received        -> writeTVar (cancelStatus am) (IntMap.insert uq Canceling cs) >> pure (pure ())
    Just (Active cancel) -> writeTVar (cancelStatus am) (IntMap.delete uq cs) >> pure cancel
    Just Canceling       -> pure (pure ())

doneHandling :: AsyncManager -> CorrelationId -> STM ()
doneHandling am uq = modifyTVar (cancelStatus am) (IntMap.delete uq)

setActive :: AsyncManager -> CorrelationId -> IO () -> IO ()
setActive am uq cancel = join . atomically $ do
  cs <- readTVar (cancelStatus am)
  case IntMap.lookup uq cs of
    Nothing         -> pure (pure ()) -- reply already sent.
    Just Received   -> writeTVar (cancelStatus am) (IntMap.insert uq (Active cancel) cs) >> pure (pure ())
    Just (Active _) -> writeTVar (cancelStatus am) (IntMap.insert uq (Active cancel) cs) >> pure (pure ())
    Just Canceling  -> pure cancel

receivedRequest :: AsyncManager -> CorrelationId -> STM ()
receivedRequest am uq = do
  cs <- readTVar (cancelStatus am)
  case IntMap.lookup uq cs of
    Nothing         -> writeTVar (cancelStatus am) (IntMap.insert uq Received cs)
    Just Received   -> error "impossible"
    Just (Active _) -> error "impossible"
    Just Canceling  -> error "impossible"



{-
Note [Exceptions during serialization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For efficiency reasons, any `IddMsg` to be sent is forced only when already
writing onto the Pipe, so we cannot discern whether a partial write already
occurred. The only option then is to tear down the write end of the pipe.

Thus, it is recommended to fully force messages at the source, to avoid
needlessly interrupting communication.
-}


-- | "Fill"s write end of the Pipe with queued messages.
--
-- Because Pipe is written to from this thread rather than 'writePipe', calls to
-- the latter can be interrupted without compromising the protocol.
--
-- Closes the Pipe write end when stopping, notifying any messages still queued to write.
--
fillPipe :: Pipe -> (forall x. IO x -> IO x) -> IO ()
fillPipe p unmask = loop `catch` cleanup Nothing where
  loop = do
    (msg,var) <- atomically $ readTQueue (writeQueue am)
    -- See Note [Exceptions during serialization] about how we always have to close the pipe.
    stop <- handle (\e -> cleanup (Just var) e >> pure True) $ do
      unmask $ (putAll p . execPut . put $ msg)
      atomically $ putTMVar var (Right ())
      pure False
    unless stop loop

  am = pipeAsyncMngr p

  cleanup last_write e0 = do
    atomically $ do
      -- deactivate here too so we keep track of the exception if end still 'Open'.
      s <- deactivatePipeEnd (pipeWriteStatus p) (Just e0)
      case last_write of
        Nothing -> pure ()
        Just var -> do
          -- report exception to the originating write
          putTMVar var . Left $ if isOpen s then e0 else toException PipeWriteClosedException
      -- real exception already reported once
      failPendingWrites p $ toException PipeWriteClosedException
    pipeCloseWrite p `catch` \(_ :: IOException) -> pure ()

failPendingWrites :: Pipe -> SomeException -> STM ()
failPendingWrites p e = do
  void $ deactivatePipeEnd (pipeWriteStatus p) (Just e)
  writes <- flushTQueue (writeQueue (pipeAsyncMngr p))
  mapM_ (`writeTMVar` Left e) (map snd writes)

failPendingReplies :: Pipe -> SomeException -> STM ()
failPendingReplies p e = do
  void $ deactivatePipeEnd (pipeReadStatus p) (Just e)
  pendings <- swapTVar (pendingReply (pipeAsyncMngr p)) IntMap.empty
  mapM_ (\wv -> putTMVar wv (Left e)) pendings


-- | An exception that can be thrown on the host process representing an
-- expection that happened on the remote one.
newtype RemoteProcessException = RemoteProcessException SerializableException deriving Show
instance Exception RemoteProcessException

-- | Come up with a fresh id for 'CorrelationId' or 'InboxId'
freshId :: AsyncManager -> IO Int
freshId AsyncManager{asyncFreshId} =
  atomicModifyIORef' asyncFreshId $ \i -> (i+1, i)

--------------------------------------------------------------------------------
-- * Consume `reply` exactly once
--------------------------------------------------------------------------------

data ReplyResult
  = ReplyNotWritten                 -- ^ Pipe already closed when trying to queue the write.
  | ReplyWritingError SomeException -- ^ Encountered exception while writing the reply message.
  | ReplyWritingSuccess             -- ^ Write completed (i.e. putAll returned without error).

replyResultMaybeThrow :: ReplyResult -> IO ()
replyResultMaybeThrow = \case
  ReplyNotWritten -> throwIO PipeWriteClosedException
  ReplyWritingError e -> throwIO e
  ReplyWritingSuccess -> pure ()

-- | Every request should get exactly one reply.
data WriteReply a where
  WriteReply :: (Either SomeException a -> forall b. (ReplyResult -> IO b) -> IO b) %1 -> WriteReply a

-- | Write a reply exactly once (consuming the ability to write more replies)
writeReply :: WriteReply a %1 -> a -> IO ()
writeReply (WriteReply r) = \x -> r (Right x) replyResultMaybeThrow

-- | Write the reply with a value or an exception
runWriteReply :: WriteReply a %1 -> Either SomeException a -> (ReplyResult -> IO b) -> IO b
runWriteReply (WriteReply r) x = r x

-- | Infix, linear, 'unIO'
(|#>) :: IO a %1 -> State# RealWorld -> (# State# RealWorld, a #)
(|#>) (IO fs) s = fs s

(%>>=) :: (IO a) -> (a -> IO b) %1 -> IO b
(%>>=) = \ m -> \ f -> IO (\s0 ->
        case m |#> s0 of -- unIO for linearity checking
          (# s1, x #) -> f x |#> s1)
infixl 1 %>>=

--------------------------------------------------------------------------------
-- * Serializable Exceptions
--------------------------------------------------------------------------------

-- SomeException can't be serialized because it contains dynamic
-- types.  However, we do very limited things with the exceptions that
-- are thrown by interpreted computations:
--
-- * We print them, e.g. "*** Exception: <something>"
-- * UserInterrupt has a special meaning
-- * In ghc -e, exitWith should exit with the appropriate exit code
--
-- So all we need to do is distinguish UserInterrupt and ExitCode, and
-- all other exceptions can be represented by their 'show' string.
--
data SerializableException
  = EUserInterrupt
  | EExitCode ExitCode
  | EOtherException String
  deriving (Generic, Show)

toSerializableException :: SomeException -> SerializableException
toSerializableException ex
  | Just UserInterrupt <- fromException ex  = EUserInterrupt
  | Just (ec::ExitCode) <- fromException ex = (EExitCode ec)
  | otherwise = EOtherException $
#if MIN_VERSION_base(4,20,0)
      -- Exception plus backtrace as seen in `displayExceptionWithInfo`
      case displayExceptionContext (someExceptionContext ex) of
        "" -> displayException (ex :: SomeException)
        cx -> displayException (ex :: SomeException) ++ "\n\n" ++ cx
#else
      show (ex :: SomeException)
#endif

fromSerializableException :: SerializableException -> SomeException
fromSerializableException EUserInterrupt = toException UserInterrupt
fromSerializableException (EExitCode c) = toException c
fromSerializableException (EOtherException str) = toException (ErrorCall str)

instance Binary ExitCode
instance Binary SerializableException
