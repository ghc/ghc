-- |

module GHC.Driver.MakeSem where

import GHC.Prelude

import System.Posix.Semaphore
import System.Posix.Files (stdFileMode)
import qualified Control.Monad.Catch as MC

import Control.Monad.IO.Class
import Control.Monad (unless)
import Control.Concurrent.MVar
import Control.Concurrent.QSem

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )

-- TODO tidy + add windows constructor
data MakeSem = LocalSem QSem | GlobalSem Int (MVar Int) Semaphore

waitMakeSem :: MonadIO m => MakeSem -> m ()
waitMakeSem s = liftIO $ case s of
  LocalSem qsem -> waitQSem qsem
  GlobalSem _ mv sem -> do
    semWait sem
    modifyMVar_ mv $ \i -> do
      let i' = i + 1
      setNumCapabilities i'
      pure i'


signalMakeSem :: MonadIO m => MakeSem -> m ()
signalMakeSem s = liftIO $ case s of
  LocalSem qsem -> signalQSem qsem
  GlobalSem _ mv sem -> do
    semPost sem
    modifyMVar_ mv $ \i -> do
      let i' = (i - 1) `max` 1
      c <- getNumCapabilities
      setNumCapabilities i'
      pure i'


withMakeSem :: (MC.MonadMask m, MonadIO m) => Int -> Maybe FilePath -> (MakeSem -> m a) -> m a
withMakeSem n_jobs mb_global_sem action
  | Just sem_path <- mb_global_sem = do
      -- TODO this can fail. We need a reasonable error message
      sem <- liftIO $ semOpen sem_path (OpenSemFlags { semCreate = True, semExclusive = False }) stdFileMode n_jobs
      mv <- liftIO $ newMVar ()
      action (GlobalSem n_jobs mv sem)
  | otherwise = do
      let resetNumCapabilities orig_n = liftIO $ setNumCapabilities orig_n
          updNumCapabilities = liftIO $ do
            n_capabilities <- getNumCapabilities
            n_cpus <- getNumProcessors
              -- Setting number of capabilities more than
              -- CPU count usually leads to high userspace
              -- lock contention. #9221
            let n_caps = min n_jobs n_cpus
            unless (n_capabilities /= 1) $ setNumCapabilities n_caps
            return n_capabilities
      sem <- liftIO $ newQSem n_jobs
      MC.bracket updNumCapabilities resetNumCapabilities  $ \_ -> action (LocalSem sem)
