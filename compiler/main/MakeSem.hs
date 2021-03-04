-- |

module MakeSem where

import GhcPrelude

import System.Posix.Semaphore
import System.Posix.Files (stdFileMode)
--import qualified Control.Monad.Catch as MC
import Exception

import Data.Foldable
import Control.Monad.IO.Class
import Control.Monad (unless)
import Control.Concurrent.MVar
import Control.Concurrent.QSem

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )

-- TODO tidy + add windows constructor
data MakeSem = LocalSem QSem | GlobalSem (Maybe (MVar Int)) Semaphore

waitMakeSem :: MonadIO m => MakeSem -> m ()
waitMakeSem s = liftIO $ case s of
  LocalSem qsem -> waitQSem qsem
  GlobalSem mb_num_running_mv sem -> do
    semWait sem
    for_ mb_num_running_mv $ \mv -> modifyMVar_ mv $ \i -> do
      n_cpus <- getNumProcessors
      setNumCapabilities $ (i + 1) `min` n_cpus
      pure (i + 1)


signalMakeSem :: MonadIO m => MakeSem -> m ()
signalMakeSem s = liftIO $ case s of
  LocalSem qsem -> signalQSem qsem
  GlobalSem mb_num_running_mv sem -> do
    semPost sem
    for_ mb_num_running_mv $ \mv -> modifyMVar_ mv $ \i -> do
      setNumCapabilities $ (i - 1) `max` 1
      pure (i - 1)


withMakeSem :: (ExceptionMonad m) => Int -> Maybe FilePath -> (MakeSem -> m a) -> m a
withMakeSem n_jobs mb_global_sem action = do
  n_capabilities <- liftIO getNumCapabilities
  case mb_global_sem of
    Just sem_path -> do
      -- TODO this can fail. We need a reasonable error message
      (mv, sem) <- liftIO $ do
        sem <- semOpen sem_path (OpenSemFlags { semCreate = True, semExclusive = False }) stdFileMode n_jobs
        mv <- if n_capabilities /= 1 then pure Nothing else fmap Just $ newMVar 0
        pure (mv, sem)
      action (GlobalSem mv sem)
    Nothing -> do
      let resetNumCapabilities orig_n = liftIO $ setNumCapabilities orig_n
          updNumCapabilities = liftIO $ do
            n_cpus <- getNumProcessors
              -- Setting number of capabilities more than
              -- CPU count usually leads to high userspace
              -- lock contention. #9221
            let n_caps = min n_jobs n_cpus
            unless (n_capabilities /= 1) $ setNumCapabilities n_caps
            return n_capabilities
      sem <- liftIO $ newQSem n_jobs
      gbracket updNumCapabilities resetNumCapabilities  $ \_ -> action (LocalSem sem)
