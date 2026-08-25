{-# LANGUAGE BlockArguments #-}

module GHC.Driver.MakeAction
  ( -- * Make action
    MakeAction(..)
  , RunMakeM
  , MakeEnv(..)
   -- ** Result cells
  , ResultCell
  , newResultCell
  , awaitResultCell
  , resultCellFilled
  , resultCellsFilled
  , allSucceeded
  -- * Running the pipelines
  , runPipelines
  -- * Utility
  , hscWorkerPool
  , setHscWorkerEnv
  ) where

import GHC.Prelude

import GHC.Driver.Concurrency
import GHC.Driver.Config.Concurrency
import GHC.Driver.Env
import GHC.Driver.Errors ( reportError )
import GHC.Driver.Errors.Types
import GHC.Driver.Messager
import GHC.Driver.Monad

import GHC.Data.Dependent ( Some(..) )

import GHC.Types.SrcLoc ( noSrcSpan )
import GHC.Utils.Error ( emptyDiagOpts )
import GHC.Utils.Misc
import GHC.Utils.Outputable ( neverQualify, text )
import GHC.Utils.Panic ( panic )

import Control.Concurrent.STM
  ( STM, TMVar, atomically, newEmptyTMVarIO, readTMVar, tryPutTMVar )
import Control.Exception ( SomeAsyncException, SomeException, fromException )
import Control.Monad ( unless, void )
import qualified Control.Monad.Catch as MC
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Maybe ( isJust )
import qualified GHC.Conc as CC

--------------------------------------------------------------------------------
-- * MakeEnv and MakeAction
--------------------------------------------------------------------------------

-- | Environment used when compiling a module
data MakeEnv =
  MakeEnv
    { me_hsc_env      :: !HscEnv
      -- ^ the basic 'HscEnv', which will be augmented for each module
    , me_messager     :: !(Maybe Messager)
    , me_diag_wrapper :: GhcMessage -> AnyGhcDiagnostic
    }

-- ** MakeAction ---------------------------------------------------------------

data MakeAction where
  MakeAction :: forall a.
    { actionDeps :: ![Some ResultCell]
      -- ^ The results this action depends on.
      --
      -- The action runs only once every result cell is filled. If any result
      -- cell contains a failure, skip the action and fill its own result cell
      -- with failure.
    , makeAction :: !(RunMakeM a)
      -- ^ The action to run
    , makeResult :: !(ResultCell a)
      -- ^ Where the action's result is put
    } -> MakeAction

type RunMakeM a = ReaderT MakeEnv (MaybeT IO) a

-- | A write-once (monotonic) cell holding the result of a 'MakeAction'
-- ('Nothing' when the action failed or was skipped).
newtype ResultCell a = ResultCell ( TMVar ( Maybe a ) )

newResultCell :: IO ( ResultCell a )
newResultCell = ResultCell <$> newEmptyTMVarIO

-- | Fill a result cell.
--
-- Panics on a cell that is already filled: each cell is written exactly once.
fillResultCell :: HasDebugCallStack => ResultCell a -> Maybe a -> IO ()
fillResultCell ( ResultCell var ) res = atomically do
  filled <- tryPutTMVar var res
  unless filled $ panic "fillResultCell: cell already filled"

-- | Record a failure in a result cell, doing nothing if the cell is already
-- filled.
--
-- Used to ensure a result cell is always filled, regardless of when an
-- async exception may strike.
ensureResultCellFilled :: ResultCell a -> IO ()
ensureResultCellFilled ( ResultCell var ) =
  void $ atomically $ tryPutTMVar var Nothing

-- | The result in the cell; retries until it has been filled.
awaitResultCell :: ResultCell a -> STM ( Maybe a )
awaitResultCell ( ResultCell var ) = readTMVar var

-- | Retries until the cell has been filled.
resultCellFilled :: ResultCell a -> STM ()
resultCellFilled = void . awaitResultCell

-- | Retries until all of the given 'ResultCell's have been filled.
resultCellsFilled :: [Some ResultCell] -> STM ()
resultCellsFilled = mapM_ (\ (Some cell) -> resultCellFilled cell)

-- | Retries until all of the given 'ResultCell's have been filled,
-- returning whether they __all__ contain a success value.
allSucceeded :: [Some ResultCell] -> STM Bool
allSucceeded = fmap and . traverse (\ (Some cell) -> isJust <$> awaitResultCell cell)

--------------------------------------------------------------------------------
-- * Running the pipelines
--------------------------------------------------------------------------------

-- | Run the given 'MakeAction's (assumed to be in dependency order), waiting
-- for them all to finish.
--
-- A synchronous exception escaping an action is reported and recorded as a
-- failure of that action; the remaining actions still run.
--
-- Asynchronous exceptions abort the build and are rethrown.
runPipelines
  :: HscEnv
  -> (GhcMessage -> AnyGhcDiagnostic) -> Maybe Messager
  -> [MakeAction] -- ^ The build plan for all the module nodes
  -> IO ()
runPipelines hsc_env diag_wrapper mHscMessager all_pipelines = do
  liftIO $ label_self "main --make thread"
  pool <- hscWorkerPool "make_worker" hsc_env
  runCoordinatingWorkers @MakeAction pool
    ( \ (MakeAction { actionDeps = deps } ) -> resultCellsFilled deps )
    ( \ work_env (MakeAction { actionDeps = deps, makeAction = act, makeResult = res_cell }) -> do
        let lcl_hsc_env = setHscWorkerEnv work_env hsc_env
            lcl_env =
              MakeEnv
                { me_hsc_env      = lcl_hsc_env
                , me_messager     = mHscMessager
                , me_diag_wrapper = diag_wrapper
                }
        -- The dependencies have settled (that was the readiness condition).
        -- Only run the action if they all succeeded; otherwise propagate failure.
        deps_ok <- atomically ( allSucceeded deps )
        mres <-
          if not deps_ok
          then pure Nothing
          else
            runMaybeT (runReaderT act lcl_env) `MC.catch` \ (e :: SomeException) ->
            case fromException @SomeAsyncException e of
              Just {} -> MC.throwM e
              Nothing -> do
                -- A synchronous exception should never escape here; report it here
                -- (just in case) instead of failing the whole build.
                reportError (hsc_logger lcl_hsc_env) neverQualify emptyDiagOpts noSrcSpan
                  (text (show e))
                pure Nothing
        fillResultCell res_cell mres )
    ( \ (MakeAction _ _ res_cell) ->
      -- Make sure that the result cell is always filled, regardless of exceptions.
      ensureResultCellFilled res_cell
    )
    all_pipelines
  where
    label_self :: String -> IO ()
    label_self thread_name = do
      self_tid <- CC.myThreadId
      CC.labelThread self_tid thread_name

--------------------------------------------------------------------------------
-- * Utility
--------------------------------------------------------------------------------

-- | A pool of workers limited by the session's @-j@\/@-jsem@ flags, under the
-- given thread label.
hscWorkerPool :: String -> HscEnv -> IO WorkerPoolConfig
hscWorkerPool label hsc_env = do
  limit <- mkWorkerLimit (hsc_dflags hsc_env)
  pure $
    WorkerPoolConfig
      { wp_label  = label
      , wp_limit  = limit
      , wp_report_semaphore_failure =
          semaphoreOpenFailureHandler (hsc_logger hsc_env) (hsc_dflags hsc_env)
      , wp_logger = hsc_logger hsc_env
      , wp_tmpfs  = hsc_tmpfs hsc_env
      }

-- | Set the local concurrent worker environment within an 'HscEnv'.
setHscWorkerEnv :: ConcurrentWorkerEnv -> HscEnv -> HscEnv
setHscWorkerEnv (ConcurrentWorkerEnv { cwe_logger = logger, cwe_tmpfs = tmpfs }) hsc_env =
  hsc_env { hsc_logger = logger, hsc_tmpfs = tmpfs }
