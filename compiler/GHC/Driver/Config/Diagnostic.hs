module GHC.Driver.Config.Diagnostic
  ( initDiagOpts
  )
where

import GHC.Driver.Flags
import GHC.Driver.Session

import GHC.Utils.Outputable
import GHC.Utils.Error (DiagOpts (..))

initDiagOpts :: DynFlags -> DiagOpts
initDiagOpts dflags = DiagOpts
  { diag_warning_flags       = warningFlags dflags
  , diag_fatal_warning_flags = fatalWarningFlags dflags
  , diag_warn_is_error       = gopt Opt_WarnIsError dflags
  , diag_reverse_errors      = reverseErrors dflags
  , diag_max_errors          = maxErrors dflags
  , diag_ppr_ctx             = initSDocContext dflags defaultErrStyle
  }

