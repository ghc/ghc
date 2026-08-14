module GHC.Driver.MakeAction
  ( MakeAction(..)
  , RunMakeM
  , MakeEnv(..)
  , withMakeEnv
  -- * Running the pipelines
  , runAllPipelines
  , runPipelines
  -- * Utility
  , withMakeEnvConcurrency
  , withWorkerLimitHsc
  , viewHscWorkerEnv
  , setHscWorkerEnv
  ) where

import GHC.Prelude

import GHC.Driver.Concurrency
import GHC.Driver.Config.Concurrency
import GHC.Driver.Env
import GHC.Driver.Errors.Types
import GHC.Driver.Messager
import GHC.Driver.Monad

import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Monad
import qualified Control.Monad.Catch as MC
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

--------------------------------------------------------------------------------
-- * MakeEnv and MakeAction
--------------------------------------------------------------------------------

-- | Environment used when compiling a module
data MakeEnv =
  MakeEnv
    { me_hsc_env      :: !HscEnv -- The basic HscEnv which will be augmented for each module
    , me_concurrency  :: !Concurrency
    , me_messager     :: !(Maybe Messager)
    , me_diag_wrapper :: GhcMessage -> AnyGhcDiagnostic
    }

-- | Come up with a 'MakeEnv' based on the given 'WorkerLimit'.
-- For -j1, it will be a trivial 'MakeEnv' not prepared for parallelism.
-- For -jn, it can be used from multiple threads (e.g. in runAllPipelines when -jN)
withMakeEnv
  :: WorkerLimit    -- ^ How to limit work parallelism
  -> HscEnv         -- ^ The basic HscEnv which is augmented with specific info for each module
  -> (GhcMessage -> AnyGhcDiagnostic)
  -> Maybe Messager -- ^ Optional custom messager to use to report progress
  -> (MakeEnv -> IO r) -> IO r
withMakeEnv worker_limit hsc_env diag_wrapper mHscMessager act =
  withWorkerLimitHsc hsc_env worker_limit $ \ conc hsc_env' ->
    act $
      MakeEnv
        { me_hsc_env       = hsc_env'
        , me_concurrency   = conc
        , me_messager      = mHscMessager
        , me_diag_wrapper  = diag_wrapper
        }

-- ** MakeAction ---------------------------------------------------------------

data MakeAction = forall a . MakeAction !(RunMakeM a) !(MVar (Maybe a))

type RunMakeM a = ReaderT MakeEnv (MaybeT IO) a

--------------------------------------------------------------------------------
-- * Running the pipelines
--------------------------------------------------------------------------------

-- | Build and run a pipeline using the given worker limit for parallelism
runPipelines
  :: WorkerLimit -> HscEnv
  -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager
  -> [MakeAction] -- ^ The build plan for all the module nodes
  -> IO ()
runPipelines n_job hsc_env diag_wrapper mHscMessager all_pipelines = do
  liftIO $ label_self "main --make thread"
  withMakeEnv n_job hsc_env diag_wrapper mHscMessager $ \make_env -> do
    runAllPipelines make_env all_pipelines
  where
    label_self :: String -> IO ()
    label_self thread_name = do
        self_tid <- CC.myThreadId
        CC.labelThread self_tid thread_name

-- | Run the given actions (assumed to be in dependency order) and wait for
-- them all to finish, rethrowing the first unhandled exception (in action order)
-- afterwards.
runAllPipelines :: MakeEnv -> [MakeAction] -> IO ()
runAllPipelines env acts =
  void $
    mapConcurrentWorkers "make_worker" (me_concurrency env) (viewHscWorkerEnv (me_hsc_env env))
      ( \ work_env (MakeAction act res_var) -> do
          let lcl_env = env { me_hsc_env = setHscWorkerEnv work_env (me_hsc_env env) }
          mres <- runMaybeT (runReaderT act lcl_env)
                    `MC.onException` putMVar res_var Nothing
          putMVar res_var mres )
      acts

--------------------------------------------------------------------------------
-- * Utility
--------------------------------------------------------------------------------

-- | A version of 'withWorkerLimit' taking an 'HscEnv'.
withWorkerLimitHsc :: HscEnv -> WorkerLimit -> (Concurrency -> HscEnv -> IO a) -> IO a
withWorkerLimitHsc hsc_env limit k =
  withWorkerLimit (hsc_logger hsc_env) (hsc_tmpfs hsc_env)
    (semaphoreOpenFailureHandler (hsc_logger hsc_env) (hsc_dflags hsc_env))
    limit
    (\conc work_env -> k conc (setHscWorkerEnv work_env hsc_env))

-- | Like 'withConcurrency', but retrieving the 'Concurrency' and 'HscEnv' from
-- the 'MakeEnv'.
withMakeEnvConcurrency :: MakeEnv -> (HscEnv -> IO a) -> IO a
withMakeEnvConcurrency env cont =
  withConcurrency (me_concurrency env) (cont (me_hsc_env env))

-- | The local environment for a concurrent worker derived from an 'HscEnv'.
viewHscWorkerEnv :: HscEnv -> ConcurrentWorkerEnv
viewHscWorkerEnv hsc_env =
  ConcurrentWorkerEnv { cwe_logger = hsc_logger hsc_env, cwe_tmpfs = hsc_tmpfs hsc_env }

-- | Set the local concurrent worker environment within an 'HscEnv'.
setHscWorkerEnv :: ConcurrentWorkerEnv -> HscEnv -> HscEnv
setHscWorkerEnv (ConcurrentWorkerEnv { cwe_logger = logger, cwe_tmpfs = tmpfs }) hsc_env =
  hsc_env { hsc_logger = logger, hsc_tmpfs = tmpfs }
