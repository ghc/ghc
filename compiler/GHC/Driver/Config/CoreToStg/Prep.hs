module GHC.Driver.Config.CoreToStg.Prep
  ( initCorePrepConfig
  , initCorePrepPgmConfig
  ) where

import GHC.Prelude

import GHC.Core.Opt.Pipeline.Types ( CoreToDo(..) )
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Config.Core.Lint
import GHC.Runtime.Context ( InteractiveContext )
import GHC.Tc.Utils.Env
import GHC.Utils.Outputable ( alwaysQualify )

import GHC.CoreToStg.Prep

initCorePrepConfig :: HscEnv -> IO CorePrepConfig
initCorePrepConfig hsc_env = do
   convertNumLit <- do
     let platform = targetPlatform $ hsc_dflags hsc_env
         home_unit = hsc_home_unit hsc_env
         lookup_global = lookupGlobal hsc_env
     mkConvertNumLiteral platform home_unit lookup_global
   return $ CorePrepConfig
      { cp_catchNonexhaustiveCases = gopt Opt_CatchNonexhaustiveCases $ hsc_dflags hsc_env
      , cp_convertNumLit = convertNumLit
      }

initCorePrepPgmConfig :: InteractiveContext -> DynFlags -> CorePrepPgmConfig
initCorePrepPgmConfig ic dflags = CorePrepPgmConfig
  { cpPgm_endPassConfig     = initEndPassConfig ic dflags alwaysQualify CorePrep
  , cpPgm_generateDebugInfo = needSourceNotes dflags
  }
