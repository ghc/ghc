{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module GHCi.UI.Exception(printGhciException) where

import GHC.Prelude
import GHC.Utils.Logger
import Control.Monad.IO.Class
import GHC.Driver.Session
import GHC.Types.SourceError
import GHC.Driver.Errors.Types
import GHC.Types.Error
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors

-- | Print the all diagnostics in a 'SourceError'.  Specialised for GHCi error reporting
-- for some error messages.
printGhciException :: (HasLogger m, MonadIO m, HasDynFlags m) => SourceError -> m ()
printGhciException err = do
  dflags <- getDynFlags
  logger <- getLogger
  let !diag_opts = initDiagOpts dflags
      !print_config = initPrintConfig dflags
  liftIO $ printMessages logger print_config diag_opts (GHCiMessage <$> (srcErrorMessages err))


newtype GHCiMessage = GHCiMessage { getGhciMessage :: GhcMessage }

instance Diagnostic GHCiMessage where
  type DiagnosticOpts GHCiMessage = DiagnosticOpts GhcMessage

  diagnosticMessage opts (GHCiMessage msg) = diagnosticMessage opts msg

  diagnosticReason (GHCiMessage msg) = diagnosticReason msg

  diagnosticHints (GHCiMessage msg) = diagnosticHints msg

  diagnosticCode (GHCiMessage msg)  = diagnosticCode msg
