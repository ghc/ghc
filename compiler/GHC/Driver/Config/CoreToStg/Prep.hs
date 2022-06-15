module GHC.Driver.Config.CoreToStg.Prep
  ( initCorePrepConfig
  , initCorePrepPgmConfig
  ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session
--import GHC.Driver.Config.Core.EndPass ()
import GHC.Driver.Config.Core.Lint ( defaultLintFlags, maybeInitLintPassResultConfig )
import GHC.Runtime.Context ( InteractiveContext )
import GHC.Tc.Utils.Env
import GHC.Types.Var
import GHC.Utils.Outputable

import GHC.Core.EndPass ( EndPassConfig(..) )
import GHC.Core.Lint ( LintFlags(..), StaticPtrCheck(..) )

import GHC.CoreToStg.Prep

import qualified GHC.LanguageExtensions as LangExt

initCorePrepConfig :: HscEnv -> Maybe InteractiveContext -> IO CorePrepConfig
initCorePrepConfig hsc_env m_ic = do
   convertNumLit <- do
     let platform = targetPlatform $ hsc_dflags hsc_env
         home_unit = hsc_home_unit hsc_env
         lookup_global = lookupGlobal hsc_env m_ic
     mkConvertNumLiteral platform home_unit lookup_global
   return $ CorePrepConfig
      { cp_catchNonexhaustiveCases = gopt Opt_CatchNonexhaustiveCases $ hsc_dflags hsc_env
      , cp_convertNumLit = convertNumLit
      }

initCorePrepPgmConfig :: DynFlags -> [Var] -> CorePrepPgmConfig
initCorePrepPgmConfig dflags extra_vars = CorePrepPgmConfig
  { cpPgm_endPassConfig = EndPassConfig
      { ep_dumpCoreSizes = not (gopt Opt_SuppressCoreSizes dflags)
      , ep_lintPassResult = maybeInitLintPassResultConfig dflags extra_vars
          lint_flags
          pass_ppr
          True
      , ep_printUnqual = alwaysQualify
      , ep_dumpFlag = Just Opt_D_dump_prep
      , ep_prettyPass = pass_ppr
      , ep_passDetails = empty
      }
  , cpPgm_generateDebugInfo = needSourceNotes dflags
  }
  where
    pass_ppr = text "CorePrep"
    lint_flags = (defaultLintFlags dflags)
      { -- See Note [Checking for global Ids]
        lf_check_global_ids = False
        -- See Note [Checking StaticPtrs]
      , lf_check_static_ptrs = if xopt LangExt.StaticPointers dflags
          then AllowAtTopLevel
          else AllowAnywhere
      }
