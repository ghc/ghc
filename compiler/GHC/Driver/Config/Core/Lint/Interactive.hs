module GHC.Driver.Config.Core.Lint.Interactive
  ( lintInteractiveExpr, lintInteractiveExprAfterPrep
  ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.DynFlags
import GHC.Driver.Config.Core.Lint

import GHC.Core
import GHC.Core.Ppr
import GHC.Core.Lint
import GHC.Core.Lint.Interactive

import GHC.Data.Bag

import GHC.Utils.Outputable as Outputable

lintInteractiveExpr :: SDoc -- ^ The source of the linted expression
                    -> HscEnv
                    -> CoreExpr -> IO ()
lintInteractiveExpr = lintInteractiveExprWith False

-- | Check an interactive expression after CorePrep lowered its literals.
lintInteractiveExprAfterPrep :: SDoc -> HscEnv -> CoreExpr -> IO ()
lintInteractiveExprAfterPrep = lintInteractiveExprWith True

lintInteractiveExprWith :: Bool -> SDoc -> HscEnv -> CoreExpr -> IO ()
lintInteractiveExprWith after_prep what hsc_env expr
  | not (gopt Opt_DoCoreLinting dflags)
  = return ()
  | Just err <- lintExpr config expr
  = displayLintResults logger what (pprCoreExpr expr) (emptyBag, err)
  | otherwise
  = return ()
  where
    dflags = hsc_dflags hsc_env
    logger = hsc_logger hsc_env
    initial = initLintConfig dflags (interactiveInScope (hsc_IC hsc_env))
    config = initial { l_flags = (l_flags initial) { lf_after_core_prep = after_prep } }
