{-# LANGUAGE LambdaCase #-}
module GHC.Driver.Config.Core.Lint
  ( defaultLintFlags
  , corePrepFlags
  , desugarBeforeFlags
  , desugarAfterFlags
  , tidyFlags
  , initLintAnnotationsConfig
  , initLintPassResultConfig
  , initLintConfig
  , showLintWarnings
  , perPassFlags
  ) where

import GHC.Prelude

import qualified GHC.LanguageExtensions as LangExt

import GHC.Driver.Session
import GHC.Driver.Config.Diagnostic

import GHC.Core.Lint
import GHC.Core.Opt.Pipeline.Types
import GHC.Core.Opt.Utils ( SimplMode(..) )
import GHC.Core.Coercion

import GHC.Types.Basic ( CompilerPhase(..) )
import GHC.Types.SrcLoc ( SrcSpan )

import GHC.Utils.Outputable as Outputable

initLintAnnotationsConfig :: DynFlags -> SrcSpan -> PrintUnqualified -> CoreToDo -> LintAnnotationsConfig
initLintAnnotationsConfig dflags loc print_unqual pass = LintAnnotationsConfig
  { la_doAnnotationLinting = gopt Opt_DoAnnotationLinting dflags
  , la_passName = ppr pass
  , la_sourceLoc = loc
  , la_debugLevel = debugLevel dflags
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

showLintWarnings :: CoreToDo -> Bool
-- Disable Lint warnings on the first simplifier pass, because
-- there may be some INLINE knots still tied, which is tiresomely noisy
showLintWarnings = \case
  (CoreDoSimplify
    (CoreDoSimplifyOpts
      _
      (SimplMode { sm_phase = InitialPhase })))
    -> False
  _ -> True

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

corePrepFlags :: DynFlags -> LintFlags
corePrepFlags dflags = (defaultLintFlags dflags)
  { lf_check_static_ptrs = check_static_ptrs
  }
  where
    -- See Note [Checking StaticPtrs]
    check_static_ptrs | not (xopt LangExt.StaticPointers dflags) = AllowAnywhere
                      | otherwise = AllowAtTopLevel

desugarBeforeFlags :: DynFlags -> LintFlags
desugarBeforeFlags dflags = (defaultLintFlags dflags)
  { -- See Note [Checking for INLINE loop breakers]
    lf_check_inline_loop_breakers = False
    -- See Note [Linting linearity]
  , lf_check_linearity = True
    -- In the output of the desugarer, before optimisation,
    -- we have eta-expanded data constructors with representation-polymorphic
    -- bindings; so we switch off the representation-polymorphism checks.
    -- The very simple optimiser will beta-reduce them away.
    -- See Note [Checking for representation-polymorphic built-ins]
    -- in GHC.HsToCore.Expr.
  , lf_check_fixed_rep = False
  }

desugarAfterFlags :: DynFlags -> LintFlags
desugarAfterFlags dflags = (defaultLintFlags dflags)
  { -- See Note [Checking for INLINE loop breakers]
    lf_check_inline_loop_breakers = False
  }

tidyFlags :: DynFlags -> LintFlags
tidyFlags dflags = (defaultLintFlags dflags)
  { -- See Note [Checking for global Ids]
    lf_check_global_ids = False
  , lf_check_static_ptrs = check_static_ptrs
  }
  where
    -- See Note [Checking StaticPtrs]
    check_static_ptrs | not (xopt LangExt.StaticPointers dflags) = AllowAnywhere
                      | otherwise = RejectEverywhere

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
