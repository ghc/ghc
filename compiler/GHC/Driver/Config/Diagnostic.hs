
-- | Functions for initialising error message printing configuration from the
-- GHC session flags.
module GHC.Driver.Config.Diagnostic
  ( initDiagOpts
  , initPrintConfig
  , initPsMessageOpts
  , initDsMessageOpts
  , initTcMessageOpts
  , initDriverMessageOpts
  , initIfaceMessageOpts
  )
where

import GHC.Driver.Flags
import GHC.Driver.DynFlags
import GHC.Prelude

import GHC.Utils.Outputable
import GHC.Utils.Error (DiagOpts (..))
import GHC.Driver.Errors.Types (GhcMessage, GhcMessageOpts (..), PsMessage, DriverMessage, DriverMessageOpts (..), checkBuildingCabalPackage)
import GHC.Driver.Errors.Ppr () -- Diagnostic instances
import GHC.Tc.Errors.Types
import GHC.HsToCore.Errors.Types
import GHC.Types.Error
import GHC.Iface.Errors.Types

-- | Initialise the general configuration for printing diagnostic messages
-- For example, this configuration controls things like whether warnings are
-- treated like errors.
initDiagOpts :: DynFlags -> DiagOpts
initDiagOpts dflags = DiagOpts
  { diag_warning_flags       = warningFlags dflags
  , diag_fatal_warning_flags = fatalWarningFlags dflags
  , diag_custom_warning_categories = customWarningCategories dflags
  , diag_fatal_custom_warning_categories = fatalCustomWarningCategories dflags
  , diag_warn_is_error       = gopt Opt_WarnIsError dflags
  , diag_reverse_errors      = reverseErrors dflags
  , diag_max_errors          = maxErrors dflags
  , diag_ppr_ctx             = initSDocContext dflags defaultErrStyle
  }

-- | Initialise the configuration for printing specific diagnostic messages
initPrintConfig :: DynFlags -> DiagnosticOpts GhcMessage
initPrintConfig dflags =
  GhcMessageOpts { psMessageOpts = initPsMessageOpts dflags
                 , tcMessageOpts = initTcMessageOpts dflags
                 , dsMessageOpts = initDsMessageOpts dflags
                 , driverMessageOpts= initDriverMessageOpts dflags }

initPsMessageOpts :: DynFlags -> DiagnosticOpts PsMessage
initPsMessageOpts _ = NoDiagnosticOpts

initTcMessageOpts :: DynFlags -> DiagnosticOpts TcRnMessage
initTcMessageOpts dflags =
  TcRnMessageOpts { tcOptsShowContext    = gopt Opt_ShowErrorContext dflags
                  , tcOptsIfaceOpts      = initIfaceMessageOpts dflags }

initDsMessageOpts :: DynFlags -> DiagnosticOpts DsMessage
initDsMessageOpts _ = NoDiagnosticOpts

initIfaceMessageOpts :: DynFlags -> DiagnosticOpts IfaceMessage
initIfaceMessageOpts dflags =
                  IfaceMessageOpts { ifaceShowTriedFiles = verbosity dflags >= 3
                                   , ifaceBuildingCabalPackage = checkBuildingCabalPackage dflags }

initDriverMessageOpts :: DynFlags -> DiagnosticOpts DriverMessage
initDriverMessageOpts dflags = DriverMessageOpts (initPsMessageOpts dflags) (initIfaceMessageOpts dflags)

