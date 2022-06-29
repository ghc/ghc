{-# LANGUAGE LambdaCase #-}
module GHC.Driver.Config.Core.Lint
  ( defaultLintFlags
  , desugarBeforeFlags
  , desugarAfterFlags
  , lintPassResult
  , lintCoreBindings
  , initLintAnnotationsConfig
  , initLintPassResultConfig
  , initLintConfig
  , showLintWarnings
  , perPassFlags
  ) where

import GHC.Prelude

import qualified GHC.LanguageExtensions as LangExt

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Config.Diagnostic

import GHC.Core
import GHC.Core.Lint
import GHC.Core.Lint.Interactive
import GHC.Core.Opt.Pipeline.Types
import GHC.Core.Opt.Utils ( SimplMode(..) )
import GHC.Core.Coercion

import GHC.Types.Basic ( CompilerPhase(..) )
import GHC.Types.SrcLoc ( SrcSpan )

import GHC.Utils.Outputable as Outputable

{-
These functions are not CoreM monad stuff, but they probably ought to
be, and it makes a convenient place for them.  They print out stuff
before and after core passes, and do Core Lint when necessary.
-}

lintPassResult :: HscEnv -> CoreToDo -> CoreProgram -> IO ()
lintPassResult hsc_env pass binds
  | not (gopt Opt_DoCoreLinting dflags)
  = return ()
  | otherwise
  = lintPassResult'
    (hsc_logger hsc_env)
    (initLintPassResultConfig dflags (interactiveInScope $ hsc_IC hsc_env)
      (perPassFlags dflags pass)
      (ppr pass)
      (showLintWarnings pass))
    binds
  where
    dflags = hsc_dflags hsc_env

-- | Type-check a 'CoreProgram'. See Note [Core Lint guarantee].
lintCoreBindings :: DynFlags -> CoreToDo -> [Var] -> CoreProgram -> WarnsAndErrs
lintCoreBindings dflags coreToDo vars -- binds
  = lintCoreBindings' $ LintConfig
      { l_diagOpts = initDiagOpts dflags
      , l_platform = targetPlatform dflags
      , l_flags    = perPassFlags dflags coreToDo
      , l_vars     = vars
      }

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
  , lpr_makeLintFlags = lint_flags --perPassFlags dflags pass
  , lpr_showLintWarnings = show_lint_warnings -- showLintWarnings pass
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
perPassFlags dflags pass
  = (defaultLintFlags dflags)
               { lf_check_global_ids = check_globals
               , lf_check_inline_loop_breakers = True
               , lf_check_static_ptrs = check_static_ptrs
               , lf_check_linearity = check_linearity
               , lf_check_fixed_rep = True
               }
  where
    -- See Note [Checking for global Ids]
    check_globals = case pass of
                      CoreTidy -> False
                      CorePrep -> False
                      _        -> True

    -- See Note [Checking StaticPtrs]
    check_static_ptrs | not (xopt LangExt.StaticPointers dflags) = AllowAnywhere
                      | otherwise = case pass of
                          CoreDoFloatOutwards _ -> AllowAtTopLevel
                          CoreTidy              -> RejectEverywhere
                          CorePrep              -> AllowAtTopLevel
                          _                     -> AllowAnywhere

    -- See Note [Linting linearity]
    check_linearity = gopt Opt_DoLinearCoreLinting dflags

desugarBeforeFlags :: DynFlags -> LintFlags
desugarBeforeFlags dflags
  = (defaultLintFlags dflags)
               {
               -- See Note [Checking for global Ids]
                 lf_check_global_ids = True
               -- See Note [Checking for INLINE loop breakers]
               , lf_check_inline_loop_breakers = False
               -- See Note [Checking StaticPtrs]
               , lf_check_static_ptrs = AllowAnywhere
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
desugarAfterFlags dflags
  = (defaultLintFlags dflags)
               {
               -- See Note [Checking for global Ids]
                 lf_check_global_ids = True
               -- See Note [Checking for INLINE loop breakers]
               , lf_check_inline_loop_breakers = False
               -- See Note [Checking StaticPtrs]
               , lf_check_static_ptrs = AllowAnywhere
               -- See Note [Linting linearity]
               , lf_check_linearity = gopt Opt_DoLinearCoreLinting dflags
               , lf_check_fixed_rep = True
               }

initLintConfig :: DynFlags -> [Var] -> LintConfig
initLintConfig dflags vars = LintConfig
  { l_diagOpts = initDiagOpts dflags
  , l_platform = targetPlatform dflags
  , l_flags    = defaultLintFlags dflags
  , l_vars     = vars
  }

defaultLintFlags :: DynFlags -> LintFlags
defaultLintFlags dflags = LF { lf_check_global_ids = True
                             , lf_check_inline_loop_breakers = True
                             , lf_check_static_ptrs = AllowAnywhere
                             , lf_check_linearity = gopt Opt_DoLinearCoreLinting dflags
                             , lf_report_unsat_syns = True
                             , lf_check_fixed_rep = True
                             }
