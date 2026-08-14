-- | Subsystem configuration for 'GHC.Driver.Concurrency'.
module GHC.Driver.Config.Concurrency
  ( mkWorkerLimit
  , semaphoreOpenFailureHandler
  ) where

import GHC.Prelude

import GHC.Driver.Concurrency
import GHC.Driver.Config.Diagnostic ( initDiagOpts, initPrintConfig )
import GHC.Driver.DynFlags
import GHC.Driver.Errors ( printOrThrowDiagnostics )
import GHC.Driver.Errors.Types

import GHC.Types.Error ( singleMessage )
import GHC.Types.SrcLoc ( noSrcSpan )
import GHC.Utils.Error ( mkPlainMsgEnvelope )
import GHC.Utils.Logger ( Logger )

import GHC.Conc ( getNumProcessors )
import System.Semaphore ( SemaphoreError )

--------------------------------------------------------------------------------

-- | Compute the 'WorkerLimit' from the @-j@\/@-jsem@ flags.
mkWorkerLimit :: DynFlags -> IO WorkerLimit
mkWorkerLimit dflags =
  case parMakeCount dflags of
    Nothing -> pure $ num_procs 1
    Just (ParMakeSemaphore h) -> pure (JSemLimit h)
    Just ParMakeNumProcessors -> num_procs <$> getNumProcessors
    Just (ParMakeThisMany n) -> pure $ num_procs n
  where
    num_procs x = NumProcessorsLimit (max 1 x)

-- | Report that the semaphore specified using the @-jsem@ flag could not be opened.
semaphoreOpenFailureHandler :: Logger -> DynFlags -> SemaphoreError -> IO ()
semaphoreOpenFailureHandler logger dflags err = do
  let diag = DriverSemaphoreOpenFailure (checkBuildingCabalPackage dflags) err
      msg  = singleMessage $ mkPlainMsgEnvelope (initDiagOpts dflags) noSrcSpan diag
  printOrThrowDiagnostics logger (initPrintConfig dflags) (initDiagOpts dflags) (GhcDriverMessage <$> msg)
