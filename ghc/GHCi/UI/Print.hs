module GHCi.UI.Print
  ( printForUserNeverQualify
  , printForUserGlobalRdrEnv
  , printForUser
  , printForUserPartWay
  , printError
  , printGhciException
  , printGhciCommandException
  ) where

import qualified GHC
import GHC.Types.Name.Reader
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Types.Error
import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Errors
import GHC.Driver.Config.Diagnostic

import GHC.Utils.Logger
import GHC.Utils.Error
import GHC.Utils.Outputable
import qualified GHC.Types.Name.Ppr as Ppr (mkNamePprCtx)
import qualified GHC.Driver.Ppr as Ppr

import Prelude hiding ((<>))
import System.IO

import GHCi.UI.Exception


printForUserNeverQualify :: GhcMonad m => SDoc -> m ()
printForUserNeverQualify doc = do
  dflags <- GHC.getInteractiveDynFlags
  liftIO $ Ppr.printForUser dflags stdout neverQualify AllTheWay doc

printForUserGlobalRdrEnv :: (GhcMonad m, Outputable info)
                         => Maybe (GlobalRdrEnvX info) -> SDoc -> m ()
printForUserGlobalRdrEnv mb_rdr_env doc = do
  dflags <- GHC.getInteractiveDynFlags
  name_ppr_ctx <- mkNamePprCtxFromGlobalRdrEnv dflags mb_rdr_env
  liftIO $ Ppr.printForUser dflags stdout name_ppr_ctx AllTheWay doc
    where
      mkNamePprCtxFromGlobalRdrEnv _ Nothing = GHC.getNamePprCtx
      mkNamePprCtxFromGlobalRdrEnv dflags (Just rdr_env) =
        withSession $ \ hsc_env ->
        let unit_env = hsc_unit_env hsc_env
            ptc = initPromotionTickContext dflags
        in  return $ Ppr.mkNamePprCtx ptc unit_env rdr_env

printForUser :: GhcMonad m => SDoc -> m ()
printForUser doc = do
  name_ppr_ctx <- GHC.getNamePprCtx
  dflags <- GHC.getInteractiveDynFlags
  liftIO $ Ppr.printForUserColoured dflags stdout name_ppr_ctx AllTheWay doc

printForUserPartWay :: GhcMonad m => SDoc -> m ()
printForUserPartWay doc = do
  name_ppr_ctx <- GHC.getNamePprCtx
  dflags <- GHC.getInteractiveDynFlags
  liftIO $ Ppr.printForUser dflags stdout name_ppr_ctx DefaultDepth doc

-- | pretty-print a 'GhciCommandMessage'
printError :: GhcMonad m => GhciCommandMessage -> m ()
printError err =
  let errEnvelope = mkPlainErrorMsgEnvelope interactiveSrcSpan err
  in  printError' (const NoDiagnosticOpts) (singleMessage errEnvelope)

-- | Print the all diagnostics in a 'SourceError'.  Specialised for GHCi error reporting
-- for some error messages.
printGhciException :: GhcMonad m => SourceError -> m ()
printGhciException err = printError' initGhciPrintConfig (GhciGhcMessage <$> (srcErrorMessages err))

printGhciCommandException :: GhcMonad m => GhciCommandError -> m ()
printGhciCommandException (GhciCommandError errs) = printError' initGhciPrintConfig errs

printError' :: (GhcMonad m, Diagnostic a) => (DynFlags -> DiagnosticOpts a) -> Messages a -> m ()
printError' get_config err = do
  dflags <- getDynFlags
  logger <- getLogger
  let !diag_opts = initDiagOpts dflags
      !print_config = get_config dflags
  liftIO $ printMessages logger print_config diag_opts err

-- | Initialize the 'GhciMessageOpts' with the 'GhcOpts' from the 'DynFlags'
initGhciPrintConfig :: DynFlags -> GhciMessageOpts
initGhciPrintConfig dflags = GhciMessageOpts
  { ghcMessageOpts         = initPrintConfig dflags
  , ghciCommandMessageOpts = NoDiagnosticOpts
  }
