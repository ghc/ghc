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
  , RemoteProcessException(..), DecodeAsyncReqException(..)

    -- ** Mailboxes
  , InboxId(..)
  , newMailbox
  , closeMailbox
  , withNewMailbox

    -- ** Async matching internals
  , AsyncManager(..), CorrelationId
  , IddMsg(..), newAsyncManager

    -- ** Consume `reply` exactly once
  , WriteReply(..), writeReply
  , runWriteReply, (|#>), (%>>=)

    -- * Serializable exceptions
  , SerializableException(..)
  , toSerializableException, fromSerializableException
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

-- | An opaque pipe for bidirectional binary data transmission.
data Pipe = Pipe
  { getSome         :: !(IO ByteString)
  , putAll          :: !(B.Builder -> IO ())
  , pipeLeftovers   :: !(IORef (Maybe ByteString))
  , pipeLock        :: !(MVar ()) -- ^ Lock to prevent concurrent writes to the stream
  , pipeAsyncMngr   :: !AsyncManager
  , pipeCloseRead   :: !(IO ())
  -- ^ closes read end of the pipe
  , pipeCloseWrite  :: !(IO ())
  -- ^ closes write end of the pipe
  , pipeReadClosed  :: !(TVar Bool)
    -- ^ After the pipe is closed, messages can no longer be received from the
    -- other end, but the 'AsyncManager' queues might not be empty yet.
  , pipeWriteClosed :: !(TVar Bool)
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
  pipeLock        <- newMVar ()
  pipeReadClosed  <- newTVarIO False
  pipeWriteClosed  <- newTVarIO False
  mdo
    pipeAsyncMngr <- newAsyncManager p
    let p = Pipe { .. }
    pure p

-- | Close our ends of the pipe.
--
-- The ordering is subtle and important. We first fail any requests still
-- waiting for a reply, then close the write end so the other side sees EOF.
-- We take the write lock to close it, so we never close under a concurrent
-- write. Only then do we stop the drain thread, which closes the read end
-- itself once its read has finished. On Windows its read can't be
-- interrupted, so this waits for the other side to close its write end too.
-- Finally we stop the write thread, which could be blocked waiting for more to write.
closePipe :: Pipe -> IO ()
closePipe p@Pipe{..} = do
  readAlreadyClosed <- atomically $ do
    closed <- readTVar pipeReadClosed
    failPendingReplies p (toException PipeReadClosedException)
    pure closed

  writeAlreadyClosed <- atomically $ do
    closed <- readTVar pipeWriteClosed
    failPendingWrites p (toException PipeWriteClosedException)
    pure closed

  unless writeAlreadyClosed (withLock p pipeCloseWrite)
    `finally` do
      unless readAlreadyClosed
        (throwTo (asyncDrainTid pipeAsyncMngr) PipeReadClosedException)
      unless writeAlreadyClosed
        (throwTo (asyncFillTid pipeAsyncMngr) PipeWriteClosedException)

data PipeClosedException
  = PipeReadClosedException -- ^ We've closed the pipe read end on this process

  | PipeWriteClosedException -- ^ We've closed the pipe write end on this process
  deriving Show
instance Exception PipeClosedException


--------------------------------------------------------------------------------
-- ** Pipe thread-safe read/write
--------------------------------------------------------------------------------
-- All messages on the wire are 'IddMsg', which contain a correlation header
-- and identify whether they are requests or replies.

writePipe :: Pipe -> IddMsg -> IO ()
writePipe p msg = do
  var <- atomically $ writePipeAsync p msg
  either throwIO pure =<< atomically (takeTMVar var)

-- | Enqueues the message for writing.
--
-- The returned TMVar can be checked for the outcome.
writePipeAsync :: Pipe -> IddMsg -> STM (TMVar (Either SomeException ()))
writePipeAsync Pipe{..} msg = do
    var <- newEmptyTMVar
    readTVar pipeWriteClosed >>= \case
      True -> putTMVar var (Left $ toException PipeWriteClosedException)
      False -> writeTQueue (writeQueue pipeAsyncMngr) (msg, var)
    pure var


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

withLock :: Pipe -> IO c -> IO c
withLock Pipe{..} = bracket (takeMVar pipeLock) (putMVar pipeLock) . const

--------------------------------------------------------------------------------
-- * Matching requests with async replies
--------------------------------------------------------------------------------

-- | Allocates a new 'pendingReply' and takes care of deallocation if the
--   continuation throws an exception.
--
--  Will throw 'PipeReadClosedException' if that is the case.
newPendingReply :: Pipe -> (Int -> TMVar (Either SomeException LB.ByteString) -> IO b) -> (IO b)
newPendingReply pipe k = do

  let pendingReplies = pendingReply (pipeAsyncMngr pipe)

  -- Mark pending request
  uq <- freshId (pipeAsyncMngr pipe)
  wv <- newEmptyTMVarIO

  bracketOnError
    (atomically $ do
      readTVar (pipeReadClosed pipe) >>= \case
        -- don't set a reply as pending if we won't receive it.
        True  -> throwSTM PipeReadClosedException
        False -> modifyTVar pendingReplies (IntMap.insert uq wv))

    (const $ atomically $ modifyTVar pendingReplies (IntMap.delete uq))

    (const $ k uq wv)

-- | Send an asynchronous request and block waiting for a matching reply
--
-- See Note [Exceptions during serialization] about exceptions the 'Put' argument might raise.
remoteAsyncReq :: Pipe -> Put -> (Maybe InboxId) -> IO (Either SomeException LB.ByteString)
remoteAsyncReq pipe msg ii = remoteAsyncReqWithResponse pipe msg ii id


remoteAsyncReqWithResponse :: Pipe
                           -> Put
                           -> Maybe InboxId
                           -> (IO (Either SomeException LB.ByteString) -> IO b)
                           -> IO b
remoteAsyncReqWithResponse pipe msg ii k = newPendingReply pipe $ \ uq wv -> do
  -- We use an MVar to only Cancel the request if the response did not get read by `k`.
  readRes <- newEmptyMVar
  -- We might send a Cancel even if the request never got sent, but it will just
  -- get ignored in that case.
  --
  -- Waiting for confirmation that the request got sent before "arming" the
  -- cancelation requires more collaboration from `fillPipe`, because
  -- writePipe's takeMVar is interruptible. Could be done if we generalize the
  -- type of `writeQueue` to `TQueue (IddMsg, (Either SomeException ()) -> STM ())`.
  cancelOnErrorMaybe uq readRes $ do
    -- Write request
    writePipe pipe (Request uq ii (runPut msg))

    k $ do
      -- Block waiting for reply
      atomically (takeTMVar wv) <* putMVar readRes ()
  where
    cancelOnErrorMaybe :: CorrelationId -> MVar () -> IO a -> IO a
    cancelOnErrorMaybe uq readRes = handle (\ e -> sendCancelMaybe e >> throwIO e) where
      sendCancelMaybe = \case
        e | Just PipeWriteClosedException <- fromException e ->
          pure ()
          | otherwise ->
          tryTakeMVar readRes >>= \case
            Just () -> pure ()
            Nothing -> void . atomically $ do
              -- we don't wait for Cancel to be written on the pipe,
              -- and won't get a reply: nowhere to report it anyway.
              writePipeAsync pipe (Cancel uq)

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
  mbreq <- atomically $ do
    readTVar (pipeWriteClosed pipe) >>= \case
      -- We will not be able to write the reply, so no point in continuiting.
      True -> throwSTM PipeWriteClosedException
      False -> pure ()
    isClosed <- readTVar (pipeReadClosed pipe)
    taken <- case mii of
      Nothing -> tryReadTQueue  $  requestsQueue am
      Just ii -> tryReadTQueue =<< getMailbox am ii
    case taken of
      Just req@(uq,_)      -> pure (Just req)
      Nothing  | isClosed  -> pure Nothing    -- empty and closed
               | otherwise -> retry           -- wait for msg

  case mbreq of
    Nothing -> throwIO PipeReadClosedException -- can't expect reqs
    Just (uq, bs) -> do
      let reply p = do
            b <- atomically $ doneHandling am uq
            when b $ writePipe pipe (Reply uq (bimap toSerializableException runPut p))
            putMVar ack ()
      -- Run the continuation which must reply exactly once with 'writeReply'
      case runGetOrFail getit bs of
        Left (_, _, err) -> do
          let exc = DecodeAsyncReqException err
          reply (Left (toException exc))
          throwIO exc
        Right (_, _, it) ->
          restore (k (WriteReply reply) (setActive am uq) it)

            -- If there's an exception before the reply is sent, reply with an error instead.
            `catch` \(e :: SomeException) -> do

                tryTakeMVar ack >>= \case
                  Just () ->
                    -- Already replied
                    throwIO e
                  Nothing -> do
                    -- Reply with an error
                    reply (Left e)
                    throwIO e


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
newMailbox am = InboxId <$> freshId am
  -- `mailboxQueues` is written on demand, no need to do it here.

-- | Clean up a created mailbox by 'InboxId'.
closeMailbox :: AsyncManager -> InboxId -> IO ()
closeMailbox AsyncManager{..} (InboxId ii) =
  atomically (modifyTVar mailboxQueues (IntMap.delete ii))

withNewMailbox :: AsyncManager -> (InboxId -> IO a) -> IO a
withNewMailbox am = bracket (newMailbox am) (closeMailbox am)

-- | Get or create a new mailbox for this 'InboxId'
getMailbox :: AsyncManager -> InboxId -> STM (TQueue (CorrelationId, LB.ByteString))
getMailbox am (InboxId ii) = do
  mboxes <- readTVar (mailboxQueues am)
  case IntMap.lookup ii mboxes of
    Nothing -> do
      q <- newTQueue
      writeTVar (mailboxQueues am) (IntMap.insert ii q mboxes)
      pure q
    Just q ->
      pure q

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
                                      -- one of the Request being canceled.

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
drainPipe p unmask = catch (unmask $ loop `finally` pipeCloseRead p) replyToPendings  where
  loop = do
    readPipe p >>= \case
      Just msg -> do
        handleMsg msg
        loop
      -- EOF on read handle
      Nothing -> do
        -- clean up pipe then exit loop
        replyToPendings (toException PipeReadClosedException)
        pure ()
  handleMsg = \case
    -- Receive a reply to a pending notification
    Reply uq bs -> do
      atomically $ do
        m <- readTVar (pendingReply am)
        case IntMap.lookup uq m of
          Nothing ->
            -- the req was dropped from pending bc there was an exception, and no
            -- one's listening any more. the reply is ignored
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
          q <- getMailbox am ii
          writeTQueue q (uq,bs)
    Cancel uq -> cancelRequest am uq

  replyToPendings e = atomically $ failPendingReplies p e

  am = pipeAsyncMngr p

cancelRequest :: AsyncManager -> CorrelationId -> IO ()
cancelRequest am uq = join . atomically $ do
  cs <- readTVar (cancelStatus am)
  case IntMap.lookup uq cs of
    Nothing              -> pure (pure ())
    Just Received        -> writeTVar (cancelStatus am) (IntMap.insert uq Canceling cs) >> pure (pure ())
    Just (Active cancel) -> writeTVar (cancelStatus am) (IntMap.delete uq cs) >> pure cancel
    Just Canceling       -> pure (pure ())

-- | Returns @True@ when the handler should write its 'Reply'.
doneHandling :: AsyncManager -> CorrelationId -> STM Bool
doneHandling am uq = do
  cs <- readTVar (cancelStatus am)
  case IntMap.lookup uq cs of
    Nothing -> pure False
    Just s -> do
      writeTVar (cancelStatus am) $ IntMap.delete uq cs
      case s of
        Canceling -> pure False
        _         -> pure True

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
writing onto the Pipe, so we cannot discern whether any raised exception
originated due to forcing during serialization or from interacting with the
Pipe.

If the exceptions happen to be `SomeAsyncException` or `IOException` the write
end of the `Pipe` will be closed while rethrowing them, as those are likely to
indicate something is wrong with the `Pipe` itself. Same for
PipeWriteClosedException, used by `closePipe`.

It is recommended to fully force messages at the source, to avoid needlessly
interrupting communication.
-}


-- | "Fill"s write end of the Pipe with queued messages.
--
-- Because Pipe is written to from this thread rather than 'writePipe', calls to
-- the latter can be interrupted without compromising the protocol.
--
-- Closes the Pipe write end when stopping, notifying any messages still queued to write.
fillPipe :: Pipe -> (forall x. IO x -> IO x) -> IO ()
fillPipe p unmask = catch (loop `finally` pipeCloseWrite p) (atomically . failPendingWrites p) where
  loop = do
    (msg,var) <- atomically $ readTQueue (writeQueue am)
    success <- handle (\ e -> atomically (putTMVar var (Left e)) >> rethrowIfPipeFatal e >> pure False) $ do
      unmask $ withLock p (putAll p . execPut . put $ msg)
      pure True
    when success $ atomically $ putTMVar var (Right ())
    loop

  -- See Note [Exceptions during serialization]
  rethrowIfPipeFatal e
    | Just SomeAsyncException{}     <- fromException e = throwIO e
    | Just (_ :: IOException)       <- fromException e = throwIO e
    | Just PipeWriteClosedException <- fromException e = throwIO e
    | otherwise = pure ()
  am = pipeAsyncMngr p

failPendingWrites :: Pipe -> SomeException -> STM ()
failPendingWrites p e = do
  writeTVar (pipeWriteClosed p) True
  writes <- flushTQueue (writeQueue (pipeAsyncMngr p))
  mapM_ (`writeTMVar` Left e) (map snd writes)

failPendingReplies :: Pipe -> SomeException -> STM ()
failPendingReplies p e = do
  -- reqs are only added if pipe not closed;
  -- thus we close and take all pending atomically.
  writeTVar (pipeReadClosed p) True
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

-- | Every request should get exactly one reply.
data WriteReply a where
  WriteReply :: (Either SomeException a -> IO ()) %1 -> WriteReply a

-- | Write a reply exactly once (consuming the ability to write more replies)
writeReply :: WriteReply a %1 -> a -> IO ()
writeReply (WriteReply r) = \x -> r (Right x)

-- | Write the reply with a value or an exception
runWriteReply :: WriteReply a %1 -> Either SomeException a -> IO ()
runWriteReply (WriteReply r) = r

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
