{-# LANGUAGE RecursiveDo, GADTs, DeriveGeneric, StandaloneDeriving, ScopedTypeVariables,
    GeneralizedNewtypeDeriving, ExistentialQuantification, RecordWildCards, CPP,
    NamedFieldPuns, LambdaCase, TypeAbstractions, TypeApplications,
    LinearTypes, RankNTypes, UnboxedTuples, BlockArguments, TupleSections, MagicHash #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
module GHCi.Message.Async
  (
    -- * Pipes
    Pipe(..), mkPipeFromHandles, mkPipeFromContinuations
  , closePipe, PipeReadClosedException(..)

    -- * Interface for async bidirectional RPC
  , remoteAsyncReq, handleAsyncReq
  , RemoteProcessException(..), DecodeAsyncReqException(..)

    -- ** Mailboxes
  , InboxId(..)
  , newMailbox
  , closeMailbox

    -- ** Async matching internals
  , AsyncManager(..), CorrelationId
  , IddMsg(..), newAsyncManager

    -- ** Consume `reply` exactly once
  , WriteReply(..)
  , writeReply, (|#>), bindIO

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
mkPipeFromContinuations :: IO ByteString -> (B.Builder -> IO ())
                        -> IO ()
                        -- ^ pipeCloseRead
                        -> IO ()
                        -- ^ pipeCloseWrite
                        -> IO Pipe
mkPipeFromContinuations getSome putAll pipeCloseRead pipeCloseWrite = mdo
  pipeLeftovers   <- newIORef Nothing
  pipeLock        <- newMVar ()
  pipeReadClosed  <- newTVarIO False
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
closePipe :: Pipe -> IO ()
closePipe p@Pipe{..} = do
  readAlreadyClosed <- atomically $ do
    closed <- readTVar pipeReadClosed
    failPendingReplies p (toException PipeReadClosedException)
    pure closed
  withLock p pipeCloseWrite `finally`
    unless readAlreadyClosed
      (throwTo (asyncDrainTid pipeAsyncMngr) PipeReadClosedException)

-- | We've closed the pipe read end on this process
data PipeReadClosedException = PipeReadClosedException deriving Show
instance Exception PipeReadClosedException

--------------------------------------------------------------------------------
-- ** Pipe thread-safe read/write
--------------------------------------------------------------------------------
-- All messages on the wire are 'IddMsg', which contain a correlation header
-- and identify whether they are requests or replies.

writePipe :: Pipe -> IddMsg -> IO ()
writePipe p@Pipe{..} msg = withLock p $ do
  putAll (execPut (put msg)) `catch` \(e :: IOError) -> do
    pipeCloseWrite
    -- TODO: do we need a pipeCloseWrite TVar ?
    throwIO e

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

-- | Send an asynchronous request and block waiting for a matching reply
remoteAsyncReq :: Pipe -> Put -> (Maybe InboxId) -> IO (Either SomeException LB.ByteString)
remoteAsyncReq pipe msg ii = do

  let pendingReplies = pendingReply (pipeAsyncMngr pipe)

  -- Mark pending request
  uq <- freshId (pipeAsyncMngr pipe)
  wv <- newEmptyTMVarIO

  isClosed <- atomically $ do
    isClosed <- readTVar (pipeReadClosed pipe)
                -- don't send the req if we won't get a reply.
    if isClosed
      then pure True
      else do
        modifyTVar pendingReplies (IntMap.insert uq wv)
        pure False

  when isClosed $ do
    throwIO PipeReadClosedException

  -- Write request
  writePipe pipe (Request uq ii (runPut msg))
    `onException`
      -- the req wasn't done, or no one will be reading the reply:
      -- in either case, drop it from the pending map.
      atomically (modifyTVar pendingReplies (IntMap.delete uq))

  -- Block waiting for reply
  atomically (takeTMVar wv)
    `onException`
      atomically (modifyTVar pendingReplies (IntMap.delete uq))

-- | Receive a single asynchronous request and send a matching reply
-- asynchronously (the handler may fork a computation that eventually replies
-- and can return before actually replying, as long as it eventually does).
--
-- The handler receives as an argument the function to use to reply to the request.
-- It must be used exactly once (all requests expect a reply, and you can't
-- reply more than once). Linear types enforce the handler replies in every
-- branch, and if the handler otherwise throws, it is handled by 'handleAsyncReq'.
handleAsyncReq :: Pipe -> Get a -> (Maybe InboxId) -> (a -> IO Put) -> (CorrelationId -> a -> (a -> IO ()) %1 -> IO ()) -> IO ()
handleAsyncReq pipe getit mii hndlr k = mask $ \restore -> do
  let am = pipeAsyncMngr pipe

  -- Block waiting for request
  mbreq <- atomically $ do
    isClosed <- readTVar (pipeReadClosed pipe)
    taken <- case mii of
      Nothing -> tryReadTQueue  $  requestsQueue am
      Just ii -> tryReadTQueue =<< getMailbox am ii
    case taken of
      Just req -> pure (Just req)
      Nothing  | isClosed  -> pure Nothing -- empty and closed
               | otherwise -> retry -- wait for msg

  case mbreq of
    Nothing -> throwIO PipeReadClosedException -- can't expect reqs
    Just (uq, bs) -> do
      let reply p = do
            writePipe pipe (Reply uq (bimap toSerializableException runPut p))

      -- Run the continuation which must reply exactly once with 'writeReply'
      case runGetOrFail getit bs of
        Left (_, _, err) -> do
          let exc = DecodeAsyncReqException err
          reply (Left (toException exc))
          throwIO exc
        Right (_, _, it) -> k uq it (\ msg -> do
          try @SomeException (restore (hndlr msg)) >>= \x -> do
            reply x
            either throwIO (const $ pure ()) x
          )

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
  , mailboxQueues :: !(TVar (IntMap (TQueue (CorrelationId, LB.ByteString))))
    -- ^ Requests for a specific 'InboxId' that can be read using
    -- @'handleAsyncReq' p get (Just <mailboxid>)@.
  , asyncDrainTid :: !ThreadId
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

instance Binary IddMsg where
  put (Request i ii bs) = putWord8 0 >> put i >> put ii >> put bs
  put (Reply i bs)      = putWord8 1 >> put i >> put bs
  get = do
    b <- getWord8
    case b of
      0 -> Request <$> get <*> get <*> get
      1 -> Reply   <$> get <*> get
      _ -> fail $ "impossible: Binary IddMsg - " ++ show b

newAsyncManager :: Pipe -> IO AsyncManager
newAsyncManager pipe = do
  asyncFreshId  <- newIORef 0
  pendingReply  <- newTVarIO IntMap.empty
  requestsQueue <- newTQueueIO
  mailboxQueues <- newTVarIO IntMap.empty

  -- Fork thread to drain read end.
  -- (cleaned up on 'closePipe')
  asyncDrainTid <- forkIOWithUnmask (drainPipe pipe)

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
      -- Receive a reply to a pending notification
      Just (Reply uq bs) -> do
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
        loop
      -- Receive a request and queue it
      Just (Request uq mbi bs) -> do
        atomically $ case mbi of
          Nothing ->
            -- "Global" request
            writeTQueue (requestsQueue am) (uq,bs)
          Just ii -> do
            -- "Specific mailbox" request
            q <- getMailbox am ii
            writeTQueue q (uq,bs)
        loop
      -- EOF on read handle
      Nothing -> do
        -- clean up pipe then exit loop
        replyToPendings (toException PipeReadClosedException)
        pure ()

  replyToPendings e = atomically $ failPendingReplies p e

  am = pipeAsyncMngr p

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

-- | Infix, linear, 'unIO'
(|#>) :: IO a %1 -> State# RealWorld -> (# State# RealWorld, a #)
(|#>) (IO fs) s = fs s

bindIO :: (IO a) -> (a -> IO b) %1 -> IO b
bindIO = \ m f -> IO (\s0 ->
        case m |#> s0 of -- unIO for linearity checking
          (# s1, x #) -> f x |#> s1)

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
