module GHC.Driver.Config.Core.Lint.Interactive
  ( lintInteractiveExpr
  ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Config.Core.Lint

import GHC.Core
import GHC.Core.Ppr

import GHC.Core.Lint
import GHC.Core.Lint.Interactive

import GHC.Runtime.Context

import GHC.Data.Bag

import GHC.Utils.Outputable as Outputable

lintInteractiveExpr :: SDoc -- ^ The source of the linted expression
                    -> HscEnv
                    -> InteractiveContext
                    -> CoreExpr -> IO ()
lintInteractiveExpr what hsc_env ic expr
  | not (gopt Opt_DoCoreLinting dflags)
  = return ()
  | Just err <- lintExpr (initLintConfig dflags $ interactiveInScope ic) expr
  = displayLintResults logger False what (pprCoreExpr expr) (emptyBag, err)
  | otherwise
  = return ()
  where
    dflags = hsc_dflags hsc_env
    logger = hsc_logger hsc_env
