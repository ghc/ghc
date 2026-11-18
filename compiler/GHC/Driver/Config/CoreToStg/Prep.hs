module GHC.Driver.Config.CoreToStg.Prep
  ( initCorePrepConfig
  , initCorePrepConfigFor
  , initCorePrepPgmConfig
  ) where

import GHC.Prelude

import GHC.Core.Opt.Pipeline.Types ( CoreToDo(..) )
import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Config.Core.Lint
import GHC.Driver.Config.Core.Opt.Arity
import GHC.Types.Var
import GHC.Utils.Outputable ( alwaysQualify )

import GHC.CoreToStg.Prep

initCorePrepConfig :: HscEnv -> IO CorePrepConfig
initCorePrepConfig = initCorePrepConfigFor False

-- | Configure CorePrep after the driver has classified the input program or expression.
-- See Note [CorePrep for retained dependent programs] in GHC.CoreToStg.Prep.
initCorePrepConfigFor :: Bool -> HscEnv -> IO CorePrepConfig
initCorePrepConfigFor native hsc_env = do
   let dflags = hsc_dflags hsc_env
   return $ CorePrepConfig
      { cp_catchNonexhaustiveCases = gopt Opt_CatchNonexhaustiveCases dflags
      , cp_allowNativeDependent = native
      , cp_platform  = targetPlatform dflags
      , cp_arityOpts = if not native && gopt Opt_DoCleverArgEtaExpansion dflags
                       then Just (initArityOpts dflags)
                       else Nothing
      , cp_specEval  = not native && gopt Opt_SpecEval dflags
      , cp_specEvalDFun = not native && gopt Opt_SpecEvalDictFun dflags
      }

initCorePrepPgmConfig :: DynFlags -> [Var] -> CorePrepPgmConfig
initCorePrepPgmConfig dflags extra_vars = CorePrepPgmConfig
  { cpPgm_endPassConfig     = initEndPassConfig dflags extra_vars alwaysQualify CorePrep
  , cpPgm_generateDebugInfo = needSourceNotes dflags
  }
