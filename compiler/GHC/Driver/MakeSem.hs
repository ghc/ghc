{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
-- |

module GHC.Driver.MakeSem(AbstractSem(..), withAbstractSem, makeSemaphoreJobserver) where

import GHC.Prelude

import System.Posix.Semaphore
import System.Posix.Files (stdFileMode)
import qualified Control.Monad.Catch as MC

import Data.Foldable
import Control.Monad
import Control.Concurrent.QSem
import Control.Concurrent.STM
import Data.Time
import GHC.Data.OrdList
import Data.Functor
import GHC.Utils.Panic
import GHC.Conc
import GHC.IO.Exception
-- | Abstraction over the operations of a semaphore which allows usage with the
--  -j1 case or a jobserver
data AbstractSem = AbstractSem { acquireSem :: IO ()
                               , releaseSem :: IO ()
                               , cleanupSem :: IO ()
                               }

withAbstractSem :: AbstractSem -> IO b -> IO b
withAbstractSem sem = MC.bracket_ (acquireSem sem) (releaseSem sem)

data SemaphoreJobserver
  = SemaphoreJobserver
  { sjSem :: !Semaphore
  , sjTokensOwned :: !Int -- ^ How many tokens have been claimed from semaphore
  , sjTokensFree :: !Int -- ^ How many tokens are not currently being used
  , sjWaiting :: !(OrdList (TMVar ()))
  -- , sjLastAcquireTime :: !(Maybe UTCTime)
  }

data SemaphoreJobserverLoopState
  = SJLSIdle | SJLSAcquiring ThreadId (TVar Bool)| SJLSReleasing ThreadId (TVar Bool)


-- TODO pull out each operation that twiddles the SemaphoreJobserver into a named function

-- | update state because we are releasing a token.
-- We decrement sjTokensFree
-- precondition :: sjTokensFree > 1
sjsReleasingReal :: SemaphoreJobserver -> SemaphoreJobserver
sjsReleasingReal sjs = sjs { sjTokensFree = sjTokensFree sjs - 1 }

acquireSemaphoreJobserver :: TVar SemaphoreJobserver -> IO ()
acquireSemaphoreJobserver tv = do
  (tmv, _) <- atomically $ modifySjs tv $ \sjs -> do
    tmv <- newEmptyTMVar
    pure (tmv, sjs { sjWaiting = sjWaiting sjs `SnocOL` tmv })
  atomically $ takeTMVar tmv

releaseSemaphoreJobserver :: TVar SemaphoreJobserver -> IO ()
releaseSemaphoreJobserver tv = atomically $ void $ modifySjs_ tv $ \sjs ->
  pure ((), sjs { sjTokensFree = sjTokensFree sjs + 1 })

normaliseSemaphoreJobserver :: SemaphoreJobserver -> STM SemaphoreJobserver
normaliseSemaphoreJobserver sjs = case (sjTokensOwned sjs, sjWaiting sjs) of
  (x, next `ConsOL` rest)
    | x > 0 -> do
        putTMVar next ()
        normaliseSemaphoreJobserver sjs { sjTokensFree = x - 1, sjWaiting = rest }
  _ -> pure sjs

-- All modification goes through modifySjs to ensure the contents of the tvar is always in normal form
modifySjs :: TVar SemaphoreJobserver
  -> (SemaphoreJobserver -> STM (a, SemaphoreJobserver))
  -> STM (a, SemaphoreJobserver)
modifySjs tv act = do
  (a, sjs0) <- readTVar tv >>= act
  sjs <- normaliseSemaphoreJobserver sjs0 >>= \x -> writeTVar tv x $> x
  pure (a, sjs)

modifySjs_ :: TVar SemaphoreJobserver
  -> (SemaphoreJobserver -> STM (a, SemaphoreJobserver))
  -> STM a
modifySjs_ tv = fmap fst . modifySjs tv

sjAcquireThread :: TVar SemaphoreJobserver -> IO SemaphoreJobserverLoopState
sjAcquireThread tv = do
  sem <- atomically $ sjSem <$> readTVar tv
  tv_b <- newTVarIO False
  tid <- forkIOWithUnmask $ \unmask -> do
    flip MC.finally (atomically $ writeTVar tv_b True) $ do
      r <- MC.try $ unmask $ semWait sem
      case r of
        -- TODO if this is not ThreadKilled then we need to report this back
        Left e | Just ThreadKilled <- MC.fromException e
            -> pure ()
        Right () -> atomically $ modifySjs_ tv $ \sjs -> pure ((), sjs
          { sjTokensOwned = sjTokensOwned sjs + 1
          , sjTokensFree = sjTokensFree sjs + 1})
  pure $ SJLSAcquiring tid tv_b

sjReleaseThread :: TVar SemaphoreJobserver -> IO SemaphoreJobserverLoopState
sjReleaseThread tv = do
  sem <- atomically $ sjSem <$> readTVar tv
  tv_b <- newTVarIO False
  tid <- forkIOWithUnmask $ \unmask -> do
    flip MC.finally (atomically $ writeTVar tv_b True) $ do
      r <- MC.try $ unmask $ semPost sem
      case r of
        -- TODO if this is not ThreadKilled then we need to report this back
        Left (e :: MC.SomeException)
          -- | Just ThreadKilled <- MC.fromException e
            -> atomically $ modifySjs_ tv $ \sjs -> pure
              ((), sjs { sjTokensFree = sjTokensFree sjs + 1 })
        Right () -> atomically $ modifySjs_ tv $ \sjs -> pure ((), sjs { sjTokensOwned = sjTokensOwned sjs - 1 })
  pure $ SJLSReleasing tid tv_b

sjTryAcquire :: TVar SemaphoreJobserver -> SemaphoreJobserverLoopState -> STM (IO SemaphoreJobserverLoopState)
sjTryAcquire tv SJLSIdle = do
  SemaphoreJobserver
    { sjTokensFree } <- readTVar tv
  guard $ sjTokensFree == 0
  pure $ sjAcquireThread tv
sjTryAcquire _ _ = retry

sjTryRelease :: TVar SemaphoreJobserver -> SemaphoreJobserverLoopState -> STM (IO SemaphoreJobserverLoopState)
sjTryRelease tv SJLSIdle = do
  SemaphoreJobserver
    { sjTokensFree, sjTokensOwned, sjWaiting 
    } <- readTVar tv
  guard $ length sjWaiting == 0 && sjTokensFree > 0 && sjTokensOwned > 1
  pure $ sjReleaseThread tv
sjTryRelease _ _ = retry


sjTryNoticeIdle :: TVar SemaphoreJobserver -> SemaphoreJobserverLoopState -> STM (IO SemaphoreJobserverLoopState)
sjTryNoticeIdle tv ls = case ls of
  SJLSAcquiring _ tv_b -> sync_num_caps tv_b
  SJLSReleasing _ tv_b -> sync_num_caps tv_b
  _ -> retry
  where
    sync_num_caps tv_b = do
      readTVar tv_b >>= guard
      SemaphoreJobserver
        { sjTokensOwned
        } <- readTVar tv
      pure $ do
        x <- getNumCapabilities
        when (x /= sjTokensOwned) $ setNumCapabilities sjTokensOwned
        pure SJLSIdle


sjTryStopThread ::  TVar SemaphoreJobserver -> SemaphoreJobserverLoopState -> STM (IO SemaphoreJobserverLoopState)
sjTryStopThread tv ls = case ls of
  SJLSAcquiring tid _ -> do
    readTVar tv >>= guard . (== 0) . length . sjWaiting
    pure $ kill_thread_and_idle tid
  SJLSReleasing tid _ -> do
    readTVar tv >>= guard . (> 0) . length . sjWaiting
    pure $ kill_thread_and_idle tid
  _ -> retry
  where
    kill_thread_and_idle tid = killThread tid $> SJLSIdle


semaphoreJobserverLoop :: TVar SemaphoreJobserver -> IO ()
semaphoreJobserverLoop tv = loop SJLSIdle where
  loop s = do
    action <- atomically $ asum $ (\x -> x tv s) <$> [sjTryRelease, sjTryAcquire, sjTryNoticeIdle, sjTryStopThread ]
      -- sjs <- readTVar tv >>= normaliseSemaphoreJobserver
      -- let
      --   kill_thread_and_return_idle tid = killThread tid $> SJLSIdle
      --   -- TODO we can make this much nicer by 'asum'ing several STM ops together
      --   -- TODO the returned action also needs to call setNumCapabilities
      --   -- TODO rate limiting via registerDelay
      -- case (sjTokensOwned sjs, sjTokensFree sjs, sjWaiting sjs, s) of
      --   (_, 0, NilOL, SJLSIdle) -> retry
      --   (_, 0, NilOL, SJLSAcquiring tid _) ->
      --     pure $ kill_thread_and_return_idle tid
      --   (_, 0, NilOL, SJLSReleasing tid _) ->
      --     pure $ kill_thread_and_return_idle tid
      --   (num_owned, x, NilOL, SJLSIdle)
      --     | x > 0, num_owned > 1 -> do
      --         modifySjs_ tv $ \sjs0 -> let
      --           sjs = sjs0 { sjTokensFree = sjTokensFree sjs0 - 1 }
      --           in pure (sjReleaseThread tv, sjs)
      --   (_, x, NilOL, SJLSAcquiring tid _)
      --     | x > 0 -> pure $ kill_thread_and_return_idle tid
      --   (_, x, NilOL, SJLSReleasing _ tv_b)
      --     | x > 0 -> do
      --         readTVar tv_b >>= guard
      --         pure $ pure SJLSIdle
      --   (_, 0, _ `ConsOL` _, SJLSIdle) -> pure (sjAcquireThread tv)
      --   (_, 0, _ `ConsOL` _, SJLSAcquiring _ tv_b) -> do
      --     readTVar tv_b >>= guard
      --     pure $ pure SJLSIdle
      --   (_, 0, _ `ConsOL` _, SJLSReleasing tid _) ->
      --     pure $ kill_thread_and_return_idle tid
      --   _ -> panic "semaphoreJobserverLoop"
    action >>= loop

makeSemaphoreJobserver :: FilePath -> IO AbstractSem
makeSemaphoreJobserver sem_path = do
  sjSem <- semOpen sem_path (OpenSemFlags { semCreate = False, semExclusive = False }) stdFileMode 0
  let
    init_sjs = SemaphoreJobserver { sjSem, sjTokensOwned = 1, sjTokensFree = 1, sjWaiting = NilOL }
  sjs_tv <- newTVarIO init_sjs
  loop_tid <- forkIO $ semaphoreJobserverLoop sjs_tv
  let
    acquireSem = acquireSemaphoreJobserver sjs_tv
    releaseSem = releaseSemaphoreJobserver sjs_tv
    cleanupSem = killThread loop_tid
  pure AbstractSem{..}
  





