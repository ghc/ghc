{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module GHC.Driver.JobServer ( makeJobserverAcquireRelease ) where

import Prelude
import Foreign hiding (void)
import Foreign.C.Types
import System.Posix
import Foreign.C.Error
import Control.Exception
import Control.Concurrent.STM
import Control.Concurrent
import Data.Foldable
import Data.Time
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Monoid
import GHC.Utils.Panic
import GHC.Data.OrdList
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.IO.Class
import Data.Functor
import Control.Monad
import Data.Monoid
import Data.Maybe (isJust)
import Control.Monad.Trans.Maybe


-- data RlsAction a where
--   RlsAcquire :: RlsAction a
--   RlsRelease :: UTCTime -> a -> RlsAction ()


-- An adpater for an abstract sempahore, as two actions (IO a, a -> IO ())
-- we will not throttle our acquires
-- we will throttle our releases. We will not release unless:
--   It has been `limit` time since our last release or acquire


-- data RlsState a where
--   RlsAcquiring :: ThreadId -> RlsState ()
--   RlsReleasing :: ThreadId
--     -> TMVar (Maybe a) -> RlsState ()
--   RlsIdle      :: RlsState ()


-- data RlsState a
--   = RlsAcquiring
--     { raThreadId :: !ThreadId
--     , raWaits :: !(OrdList (TMVar a))
--     }
--   | RlsReleasing
--     { rrThreadId :: !ThreadId
--     , rrReleases :: !(Map UTCTime (OrdList a)) -- ^ tokens we have but don't need anymore
--     }
--   | RlsIdle

-- data RateLimitedSemaphore a
--   = RateLimitedSemaphore
--   { rlsState :: !(TMVar (RlsState a))
--   , rlsLastContact :: !(TVar UTCTime)
--   , rlsTimeout :: !Integer -- microseconds
--   , rlsAcquireIO :: !(IO a)
--   , rlsReleaseIO :: !(a -> IO ())
--   }

-- rlsAcquireThread :: RateLimitedSemaphore a -> IO ()
-- rlsAcquireThread rls@(RateLimitedSemaphore st_tmv lc_tmv timeout) = do
--   now <- getCurrentTime
--   last_contact <- atomically $ readTVar lc_tmv
--   let
--     contact_delay = diffTimeToPicoseconds (now `diffUTCTime` last_contact) `div` 1000
--   can_contact_tv <- if contact_delay < timeout
--     then registerDelay (fromIntegral $ timeout - contact_delay)
--     else newTVarIO True
--   lc <- atomically $ do
--     readTVar can_contact_tv >>= guard
--     lc <- readTVar lc_tmv
--   if lc /= last_contact
--     then rlsAcquireThread rls
--     else do
--     a <- rlsAcquireIO




--   lc <- atomically $ rlsLastContact <$> readTVar tv
--   tid <- forkIO $ undefined
--   atomically $ do
--     RateLimitedSemaphore st lc <- readTVar tv
--     case st of
--       RlsIdle -> do
--         tmv <- newEmptyTMVar
--         writeTVar tv $ RlsAcquiring tid (unitOL tmv)
--         pure tmv


-- rlsAcquire :: RateLimitedSemaphore a -> IO a
-- rlsAcquire rls@(RateLimitedSemaphore st_tmv _) = mask $ \unmask -> do
--   -- atomically is interruptible if it blocks
--   (r_tmv, st1_io) <- atomically $ do
--     r_tmv <- newEmptyTMVar
--     st0 <- takeTMVar st_tmv
--     let
--       mk_new_acquire_state = do
--         tid <- forkIO $ rlsAcquireThread rls
--         pure (RlsAcquiring tid $ unitOL tmv)
--       view_first_release m = case Map.minViewWithKey m of
--           Nothing -> Nothing
--           Just ((_,NilOL), m') -> view_first_release m'
--           Just ((k, a `ConsOL` as), m') -> Just $ (a, Map.insert k as m')
--       st1_io = case st0 of
--         RlsAcquiring tid waits -> pure $ RlsAcquiring tid $ waits `SnocOL` tmv
--         RlsIdle -> mk_new_acquire_state
--         RlsReleasing tid releases -> case view_first_release releases of
--           Nothing -> mk_new_acquire_state
--           Just (r, releases') -> atomically $ writeTMVar r_tmv r
--   st1 <- st1_io
--   atomically $ putTMVar tmv
--   unmask $ atomically $ takeTMVar r_tmv

-- rlsRelease ::

-- rlsAct :: (IO a, a -> IO ()) -> NominalDiffTime -> TVar (RateLimitedSemaphore a) -> IO ()
-- rlsAct (acq, rel) limit tv = mask $ \unmask -> do
--   let try_fork_releaser last_release_t a = do
--         r_tmv <- newTMVarIO a
--         tid <- forkIO $ mask_ $ do
--           mb_x <- atomically $ swapTMVar tmv Nothing `orElse` (putTMVar r_tmv Nothing $> Nothing)
--           for_ mb_x $ rel
--         pure $ RlsReleasing tid r_tmv
--       fork_acquirer = forkIO $ mask_ $ do
--         x <- acq
--         now <- getCurrentTime
--         atomically $ do
--           rls <- readTVar tv
--           new_rls <- case rlsWaits rls of
--             NilOL -> pure rls { rlsReleases = Map.insert now x $ rlsReleases rls }
--             ConsOL w w_rest -> putTMVar w x $> rls { rlsWaits = w_rest }
--           putTVar tv new_rls


--   action <- atomically $ do
--     rls <- readTVar tv
--     (action, new_rls) <- case rlsState rls of
--       RlsAcquiring tid
--         | null . rlsWaits $ rls -> pure killThread tid
--             -- we are acquiring, but we don't need a token anymore
--             now <- getCurrentTime

--           -- kill the thread that is mid-acquire
--           --
--           killThread tid
--           case Map.minViewWithKey (rlsReleases rls) of
--             Just ((t,a),new_map)
--               | limit `addUTCTime` t > now -> do
--                   fork_releaser a
--             _ -> pure rls {rlsState = RlsIdle}
--       | otherwise -> pure rls
--     RlsReleasing tid tmv
--       | ConsOL x rest <- rlsWaits rls -> mask_ $ do
--           mb_x <- atomically $ swapTMVar tmv Nothing `orElse` putTMVar tmv Nothing
--           case mb_x of
--             Nothing -> pure rls { rlsState = RlsIdle  }

--     RlsIdle
--       | ConsOL {} <- rlsWaits rls -> fork_acquirer
--       |


--                 -- we have a token, even though we don't need it any more





-- rlsTalk :: NominalDiffTime -> TVar (RateLimitedSemaphore a) -> IO (IO ())
-- rlsTalk lim tv = do
--   now <- getCurrentTime
--   let init_last_release = -lim `addUTCTime` now
--   x_tv <- newTVar (0, init_last_release)

--   let
--     loop last_release_t = do
--       atomically $ do
--         rls <- readTMVar tv


--     handler = undefined
--   tid <- forkFinally (loop $ ) handler
--   pure $ killThread tid


-- -- | A general adapter for an abstract ...
-- --




-- emptyRateLimitedSemaphore :: RateLimitedSemaphore a
-- emptyRateLimitedSemaphore = RateLimitedSemaphore mempty mempty

-- rlsAddWait :: TMVar a -> RateLimitedSemaphore a -> RateLimitedSemaphore a
-- rlsAddWait x rls = rls { rlsWaits = rlsWaits rls `SnocOL` x }

-- rlsAddRelease :: UTCTime -> a -> RateLimitedSemaphore a -> RateLimitedSemaphore a
-- rlsAddRelease t x rls = rls
--   { rlsReleases = Map.insertWith mappend t (unitOL x) $ rlsReleases rls
--   }

-- rlsAcquire :: TVar (RateLimitedSemaphore a)-> IO a
-- rlsAcquire tv = do
--   tmv <- newEmptyTMVarIO
--   mb_v <- atomically $ do
--     rls <- rlsAddWait tmv <$> readTVar tv
--     rlsNormaliseM rls >>= writeTVar tv
--     tryTakeTMVar tmv
--   -- It's important that we complete the above transaction before we wait
--   -- properly on the tmvar, otherwise other actors won't see our updates to tv
--   maybe (atomically $ takeTMVar tmv) pure mb_v

-- rlsRelease :: UTCTime -> a -> TVar (RateLimitedSemaphore a) -> STM ()
-- rlsRelease t x tv = do
--   rls <- rlsAddRelease t x <$> readTVar tv
--   rlsNormaliseM rls >>= writeTVar tv

-- rlsNormaliseM :: RateLimitedSemaphore a -> STM (RateLimitedSemaphore a)
-- rlsNormaliseM rls0 = let
--   (rls, stm) = rlsNormalise rls0
--   in stm $> rls

-- rlsNormalise :: RateLimitedSemaphore a -> (RateLimitedSemaphore a, STM ())
-- rlsNormalise rls0@RateLimitedSemaphore{..} = let
--   go NilOL r_map = pure rls0 { rlsReleases = r_map, rlsWaits = NilOL }
--   go waits@(ConsOL next_tmv rest_tmv) r_map = case Map.minViewWithKey r_map of
--     Nothing -> pure rls0 { rlsReleases = r_map, rlsWaits = waits }
--     Just ((k, releases_ol), new_map) -> case releases_ol of
--       NilOL -> panic "rlsNormalise"
--       ConsOL x rest -> let
--         new_releases
--           | NilOL <- rest = new_map
--           | otherwise = Map.insert k rest new_map
--         in do
--         CPS.tell $ unitOL (next_tmv, x)
--         go rest_tmv new_releases
--   (new_rls, ol_a_tmvs) = CPS.runWriter $ go rlsWaits rlsReleases
--   in (new_rls, for_ ol_a_tmvs $ \(tmv, x) -> putTMVar tmv x)


-- rlsAcquireThread :: IO a -> TVar (RateLimitedSemaphore a) -> IO ThreadId
-- rlsAcquireThread acquire0 tv = forkIO $ undefined






-- rateLimitedSemaphore :: DiffTime -> (IO (), IO ()) -> IO (ThreadId, IO (), IO ())
-- rateLimitedSemaphore limit (acquire0, release0) = do
--   tv <- newTVarIO emptyRateLimitedSemaphore
--   limited_tv <- newTVarIO False
--         threadDelay $ fromIntegral $ diffTimeToPicoseconds limit * 1000
--         atomically $ writeTVar limited_tv False
--       getCurrentTime
--     State.put now
--   undefined

-- | 'makeJobserverAcquireRelease r w' takes two fds and returns an 'acquire'
-- and 'release' action which correspond to taking and replacing job slots as
-- specified by the GNU Make Jobserver protocol.
--
-- This includes handling the implicit job given to any process; this implicit
-- job is the first to be taken and the last to be replaced.
makeJobserverAcquireRelease :: Fd -> Fd -> IO (IO (), IO ())
makeJobserverAcquireRelease r w = do
  -- Thi
  -- doesn't require taking a token from the r file (or returning any)
  implicitJob  <- newTMVarIO ()

  -- This is a stack (the jobserver doesn't care about the order) of tokens we
  -- have taken from the jobserver. Our preference is to return these over
  -- making the implicit job free.
  occupiedJobs <- newTMVarIO []

  let -- 'acquire' waits for either the implicit job to become available or the
      -- jobserver read to succeed. It opportunistically tries to take the
      -- implicit job first.
    acquire = mask $ \_ -> atomically (tryTakeTMVar implicitJob) >>= \case
      -- We took the implicit job
      Just () -> pure ()

      -- We didn't get the implicit job immediately, race a read on r and
      -- taking the implicit job
      Nothing -> do
        -- Issue the read, this TMVar will be populated when the read thread
        -- has finished. 'Nothing' if we didn't get a char (for instance we
        -- were interrupted)
        readChar   <- newEmptyTMVarIO
        -- Make sure we always populate the "thread-finished" var
        readThread <-
          forkIO
            (atomically . putTMVar readChar =<< try @SomeException (readByte r))

        -- Wait for either of the results, we have to take 'readChar' later
        -- anyway to wait for the thread, so don't leave it empty here
        -- Hang onto any exception we may be thrown
        r <- try @SomeException $ atomically
          (        (Implicit <$ takeTMVar implicitJob)
          `orElse` (JobServer <$ readTMVar readChar)
          )

        -- Kill the reading thread,
        -- The taking of readChar in the next atomically block does the waiting
        throwTo readThread StopReading

        -- Wait for the thread to die, and return the implicit job if we
        -- ended up taking both the implicit job and a jobserver job
        -- It's important that we don't get interrupted before balancing the
        -- books and pushing the taken token onto the stack. This also collates
        -- the exceptions we may have caught along the way.
        (tokenToReturn, readException, waitException) <-
          uninterruptibleMask_ $ atomically $ takeTMVar readChar >>= \case
          -- We interrupted the read before it got a token, because we
          -- grabbed the implicit job.
          Right Nothing -> case r of
            Right Implicit -> pure (Nothing, Nothing, Nothing)
            -- impossible
            Right JobServer ->
              error
                "readByte returned with Nothing but it wasn't interrupted"
            Left e -> pure (Nothing, Nothing, Just e)

          -- We got a token from the read, maybe we got the implicit job
          -- too.
          Right (Just b) -> case r of
            -- We got both the token and the implicit job, return the
            -- token
            Right Implicit  -> pure (Just b, Nothing, Nothing)
            -- We got just the token from the jobserver, push it onto the
            -- stack
            Right JobServer -> do
              modifyTMVar occupiedJobs (b :)
              pure (Nothing, Nothing, Nothing)
            -- We got a token, but have been interrupted, hang onto it so
            -- we can return the token before rethrowing
            Left e -> pure (Just b, Nothing, Just e)

          -- The token read failed in some unexpected way, nothing to do but
          -- rethrow the error after returning the implicit job (if we have
          -- it)
          Left e -> do
            case r of
              Right Implicit -> do
                putTMVar implicitJob ()
                pure (Nothing, Just e, Nothing)
              Right JobServer -> pure (Nothing, Just e, Nothing)
              -- Gee Bill! How Come Your Mom Lets You Eat *Two* Exceptions?
              Left  e2        -> pure (Nothing, Just e, Just e2)

        -- If we were interrupted before we finished waiting or if we got the
        -- implicit job and a token, return the token
        for_ tokenToReturn (writeByte w)
        -- Give preference to the exception we got while waiting
        for_ waitException throwIO
        for_ readException throwIO

    -- release puts back a jobserver token if we have taken any, otherwise it
    -- releases the implicit job
    release = mask $ \_ -> do
      b <- atomically $ takeTMVar occupiedJobs >>= \case
        -- We have already returned all our jobserver tokens, so we must be
        -- finishing the implicit job.
        [] -> do
          putTMVar implicitJob  ()
          putTMVar occupiedJobs []
          pure Nothing
        x : xs -> do
          putTMVar occupiedJobs xs
          pure (Just x)

      -- If we are freeing a jobserver job, then return it to the server
      for_ b (writeByte w)

  pure (acquire, release)

data StopReading = StopReading
  deriving (Show, Exception)

data WaitResult
  = Implicit
  | JobServer

-- | Try to read one byte, returns Nothing if the read was interrupted
--
-- If the fd is non-blocking then this falls back to 'poll' to wait until the
-- file is ready to be read from.
readByte :: Fd -> IO (Maybe CChar)
readByte fd = alloca $ \p ->
  let loop = do
        n <- try (c_interruptible_read fd p 1)
        e <- getErrno
        case n of
          Left  StopReading -> pure Nothing
          Right 1           -> Just <$> peek p
          Right _
            | e == eOK -> error
              "\"read\" returned eOK but n /= 1 bytes were read"
            | -- The fd isn't in blocking mode, use poll to wait
              e == eAGAIN || e == eWOULDBLOCK -> waitFd fd >>= \case
              -- We were interrupted
              Nothing -> pure Nothing
              -- Ready for reading
              Just _  -> loop
            | otherwise -> throwErrno "readByte"
  in  loop

-- | Use 'poll' to wait for a fd to become available for reading
--
-- Returns 'Nothing' if interrupted, 'Just ()' if the file is ready for
-- reading. Note that even if the file is available for reading any contents
-- may be gobbled up by another thread (or even another process) before they
-- can read by the waiting thread.
waitFd :: Fd -> IO (Maybe ())
waitFd fd = do
  let pollFd = PollFd fd pollIn (Event 0)
  with pollFd $ \pPollFd ->
    let loop = do
          r <- try (c_interruptible_poll pPollFd 1 (-1))
          e <- getErrno
          case r of
            -- We were interrupted, poll can finish with eINTR for any signal
            -- interrupting or not
            Left StopReading -> pure Nothing
            _ | e == eINTR   -> pure Nothing
            -- Timed out
            Right 0          -> loop
            -- We got an update
            Right 1          -> do
              pollFd <- peek pPollFd
              if pfdRevents pollFd == Event 0 then loop else pure $ Just ()
            -- Some kind of error
            Right _ -> throwErrno "readByte->poll"
    in  loop

-- | Write one byte
writeByte :: Fd -> CChar -> IO ()
writeByte fd b = alloca $ \p -> do
  poke p b
  n <- c_write fd p 1
  e <- getErrno
  if
    | n == 1 -> pure ()
    | e == eOK -> error "\"write\" returned eOK but n /= 1 bytes were written"
    | otherwise -> throwErrno "writeByte"

foreign import ccall interruptible "read"
   c_interruptible_read :: Fd -> Ptr CChar -> CSize -> IO CSize

foreign import ccall "write"
   c_write :: Fd -> Ptr CChar -> CSize -> IO CSize

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar var f = do
  x <- takeTMVar var
  putTMVar var (f x)

----------------------------------------------------------------
-- Poll
----------------------------------------------------------------

foreign import ccall interruptible "poll.h poll"
  c_interruptible_poll :: Ptr PollFd -> CNfds -> CInt -> IO CInt

data PollFd = PollFd
  { pfdFd      :: {-# UNPACK #-} !Fd
  , pfdEvents  :: {-# UNPACK #-} !Event
  , pfdRevents :: {-# UNPACK #-} !Event
  }
  deriving Show

newtype Event = Event CShort
    deriving (Eq, Show)
    deriving newtype (Storable)

pollIn :: Event
pollIn = Event 1

instance Storable PollFd where
  sizeOf _ = 8
  alignment _ = alignment (undefined :: CInt)

  peek ptr = do
    fd      <- (`peekByteOff` 0) ptr
    events  <- (`peekByteOff` 4) ptr
    revents <- (`peekByteOff` 6) ptr
    let !pollFd' = PollFd fd events revents
    return pollFd'

  poke ptr p = do
    (`pokeByteOff` 0) ptr (pfdFd p)
    (`pokeByteOff` 4) ptr (pfdEvents p)
    (`pokeByteOff` 6) ptr (pfdRevents p)
