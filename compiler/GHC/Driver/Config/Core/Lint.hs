module GHC.Driver.Config.Core.Lint
  ( defaultLintFlags
  , initLintAnnotationsConfig
  , initLintPassResultConfig
  , maybeInitLintPassResultConfig
  , initLintConfig
  , showLintWarnings
  , perPassFlags
  ) where

import GHC.Prelude

import qualified GHC.LanguageExtensions as LangExt

import GHC.Driver.Session
import GHC.Driver.Config.Diagnostic

import GHC.Core.Lint
import GHC.Core.Opt.Config
import GHC.Core.Opt.Simplify ( SimplifyOpts(..) )
import GHC.Core.Opt.Simplify.Env ( SimplMode(..) )
import GHC.Core.Coercion

import GHC.Types.Basic ( CompilerPhase(..) )
import GHC.Types.SrcLoc ( SrcSpan )

import GHC.Utils.Outputable as Outputable

initLintAnnotationsConfig :: DynFlags -> SrcSpan -> PrintUnqualified -> CoreToDo -> LintAnnotationsConfig
initLintAnnotationsConfig dflags loc print_unqual pass = LintAnnotationsConfig
  { la_doAnnotationLinting = gopt Opt_DoAnnotationLinting dflags
  , la_passName = ppr pass
  , la_sourceLoc = loc
  , la_printUnqual = print_unqual
  }

initLintPassResultConfig :: DynFlags -> [Var] -> LintFlags -> SDoc -> Bool -> LintPassResultConfig
initLintPassResultConfig dflags extra_vars lint_flags pass_ppr show_lint_warnings = LintPassResultConfig
  { lpr_diagOpts      = initDiagOpts dflags
  , lpr_platform      = targetPlatform dflags
  , lpr_makeLintFlags = lint_flags
  , lpr_showLintWarnings = show_lint_warnings
  , lpr_passPpr = pass_ppr
  , lpr_localsInScope = extra_vars
  }

maybeInitLintPassResultConfig :: DynFlags -> [Var] -> LintFlags -> SDoc -> Bool -> Maybe LintPassResultConfig
maybeInitLintPassResultConfig dflags extra_vars lint_flags pass_ppr show_lint_warnings
  | gopt Opt_DoCoreLinting dflags = Just $ initLintPassResultConfig dflags extra_vars lint_flags pass_ppr show_lint_warnings
  | otherwise = Nothing

showLintWarnings :: CoreToDo -> Bool
-- Disable Lint warnings on the first simplifier pass, because
-- there may be some INLINE knots still tied, which is tiresomely noisy
showLintWarnings (CoreDoSimplify cfg) = case sm_phase (so_mode cfg) of
  InitialPhase -> False
  _ -> True
showLintWarnings _ = True

perPassFlags :: DynFlags -> CoreToDo -> LintFlags
perPassFlags dflags pass = (defaultLintFlags dflags)
  { lf_check_static_ptrs = check_static_ptrs
  }
  where
    -- See Note [Checking StaticPtrs]
    check_static_ptrs | not (xopt LangExt.StaticPointers dflags) = AllowAnywhere
                      | otherwise = case pass of
                          CoreDoFloatOutwards _ -> AllowAtTopLevel
                          _                     -> AllowAnywhere

initLintConfig :: DynFlags -> [Var] -> LintConfig
initLintConfig dflags vars = LintConfig
  { l_diagOpts = initDiagOpts dflags
  , l_platform = targetPlatform dflags
  , l_flags    = defaultLintFlags dflags
  , l_vars     = vars
  }

defaultLintFlags :: DynFlags -> LintFlags
defaultLintFlags dflags = LF
  { lf_check_global_ids = True
  , lf_check_inline_loop_breakers = True
  , lf_check_static_ptrs = AllowAnywhere
  , lf_check_linearity = gopt Opt_DoLinearCoreLinting dflags
  , lf_report_unsat_syns = True
  , lf_check_fixed_rep = True
  }
