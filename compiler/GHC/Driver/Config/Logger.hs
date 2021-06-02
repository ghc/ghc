module GHC.Driver.Config.Logger
  ( initLogFlags
  )
where

import GHC.Prelude

import GHC.Driver.Session

import GHC.Utils.Logger (LogFlags (..))
import GHC.Utils.Outputable

-- | Initialize LogFlags from DynFlags
initLogFlags :: DynFlags -> LogFlags
initLogFlags dflags = LogFlags
  { log_default_user_context = initSDocContext dflags defaultUserStyle
  , log_default_dump_context = initSDocContext dflags defaultDumpStyle
  , log_dump_flags           = dumpFlags dflags
  , log_show_caret           = gopt Opt_DiagnosticsShowCaret dflags
  , log_show_warn_groups     = gopt Opt_ShowWarnGroups dflags
  , log_enable_timestamps    = not (gopt Opt_SuppressTimestamps dflags)
  , log_dump_to_file         = gopt Opt_DumpToFile dflags
  , log_dump_dir             = dumpDir dflags
  , log_dump_prefix          = dumpPrefix dflags
  , log_dump_prefix_override = dumpPrefixForce dflags
  , log_enable_debug         = not (hasNoDebugOutput dflags)
  , log_verbosity            = verbosity dflags
  }

