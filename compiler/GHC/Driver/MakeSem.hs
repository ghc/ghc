{-# language RecordWildCards #-}
{-# language NamedFieldPuns #-}
-- |

module GHC.Driver.MakeSem(AbstractSem(..), withAbstractSem, runPosixSemaphoreAbstractSem) where

import GHC.Prelude

import System.Posix.Semaphore
import System.Posix.Files (stdFileMode)
import qualified Control.Monad.Catch as MC

import Data.Foldable
import Control.Monad
import Control.Concurrent.MVar
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
  = SJLSIdle
  | SJLSAcquiring ThreadId (TMVar (Maybe MC.SomeException))
  | SJLSReleasing ThreadId (TMVar (Maybe MC.SomeException))


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
  r_tmv <- newEmptyTMVarIO
  tid <- forkIOWithUnmask $ \unmask -> do
    MC.try (unmask $ semPost sem) >>= \x -> atomically $ do
      r <- case x of
        Left (e :: MC.SomeException) -> pure $ case MC.fromException e of
          Just ThreadKilled -> Nothing
          _ -> Just e
        Right () -> do
          modifySjs_ tv $ \sjs -> pure ((), sjs
            { sjTokensOwned = sjTokensOwned sjs + 1
            , sjTokensFree = sjTokensFree sjs + 1})
          pure Nothing
      putTMVar r_tmv r

  pure $ SJLSAcquiring tid r_tmv

sjReleaseThread :: TVar SemaphoreJobserver -> IO SemaphoreJobserverLoopState
sjReleaseThread tv = do
  sem <- atomically $ sjSem <$> readTVar tv
  r_tmv <- newEmptyTMVarIO
  MC.mask_ $ do
    still_good <- atomically $ modifySjs_ tv $ \sjs -> if sjGuardRelease sjs
      then pure (True, sjs { sjTokensFree = sjTokensFree sjs - 1 })
      else pure (False, sjs)
    if not still_good
      then pure SJLSIdle
      else do
      tid <- forkIOWithUnmask $ \unmask -> do
        x <- MC.try (unmask $ semPost sem)
        atomically $ do
          r <- case x of
            Left (e :: MC.SomeException) -> do
              modifySjs_ tv $ \sjs -> pure
                ((), sjs { sjTokensFree = sjTokensFree sjs + 1 })
              pure $ case MC.fromException e of
                Just ThreadKilled -> Nothing
                _ -> Just e
            Right () -> do
              modifySjs_ tv $ \sjs -> pure ((), sjs { sjTokensOwned = sjTokensOwned sjs - 1 })
              pure Nothing
          putTMVar r_tmv r
      pure $ SJLSReleasing tid r_tmv

sjTryAcquire :: TVar SemaphoreJobserver -> SemaphoreJobserverLoopState -> STM (IO SemaphoreJobserverLoopState)
sjTryAcquire tv SJLSIdle = do
  SemaphoreJobserver
    { sjTokensFree, sjWaiting } <- readTVar tv
  guard $ sjTokensFree == 0 && length sjWaiting > 0
  pure $ sjAcquireThread tv
sjTryAcquire _ _ = retry

sjGuardRelease :: SemaphoreJobserver -> Bool
sjGuardRelease SemaphoreJobserver { sjTokensFree, sjTokensOwned, sjWaiting} =
  length sjWaiting == 0 && sjTokensFree > 0 && sjTokensOwned > 1

sjTryRelease :: TVar SemaphoreJobserver -> SemaphoreJobserverLoopState -> STM (IO SemaphoreJobserverLoopState)
sjTryRelease tv SJLSIdle = do
  readTVar tv >>= guard . sjGuardRelease
  pure $ sjReleaseThread tv
sjTryRelease _ _ = retry


sjTryNoticeIdle :: TVar SemaphoreJobserver -> SemaphoreJobserverLoopState -> STM (IO SemaphoreJobserverLoopState)
sjTryNoticeIdle tv ls = case ls of
  SJLSAcquiring _ tmv -> sync_num_caps tmv
  SJLSReleasing _ tmv -> sync_num_caps tmv
  _ -> retry
  where
    sync_num_caps tmv = do
      takeTMVar tmv >>= maybe (pure ()) MC.throwM
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
    action <- atomically $ asum $ (\x -> x tv s) <$>
      [ sjTryRelease
      , sjTryAcquire
      , sjTryNoticeIdle
      , sjTryStopThread
      ]
    action >>= loop

makeSemaphoreJobserver :: FilePath -> IO (AbstractSem, IO ())
makeSemaphoreJobserver sem_path = do
  sjSem <- semOpen sem_path (OpenSemFlags { semCreate = False, semExclusive = False }) stdFileMode 0
  let
    init_sjs = SemaphoreJobserver { sjSem, sjTokensOwned = 1, sjTokensFree = 1, sjWaiting = NilOL }
  sjs_tv <- newTVarIO init_sjs
  loop_res_mv <- newEmptyMVar
  loop_tid <- forkIOWithUnmask $ \unmask -> do
    r <- try $ unmask $ semaphoreJobserverLoop sjs_tv
    putMVar loop_res_mv $ case r of
      Left e
        | Just ThreadKilled <- fromException e -> Nothing
        | otherwise -> Just e
      Right () -> Nothing
  let
    acquireSem = acquireSemaphoreJobserver sjs_tv
    releaseSem = releaseSemaphoreJobserver sjs_tv
    cleanupSem = do
      -- this is interruptible
      killThread loop_tid
      takeMVar loop_res_mv >>= maybe (pure ()) MC.throwM

  pure (AbstractSem{..}, cleanupSem)
  

runPosixSemaphoreAbstractSem :: FilePath -> (AbstractSem -> IO a) -> IO a
runPosixSemaphoreAbstractSem s action = MC.mask $ \unmask -> do
  (abs, cleanup) <- makeSemaphoreJobserver s
  r <- try $ unmask $ action abs
  case r of
    Left (e1 :: MC.SomeException) -> do
      (_ :: Either MC.SomeException ()) <-  MC.try cleanup
      MC.throwM e1
    Right x -> cleanup $> x
