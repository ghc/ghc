{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE MultiWayIf #-}

module GHC.Driver.Main.Passes
    (
     hscCompileCoreExpr

    , hscDesugar'
    , hscSimplify

    ) where

import GHC.Prelude

import GHC.Driver.Env

import GHCi.RemoteTypes

import GHC.Linker.Types

import GHC.Core

import GHC.Tc.Utils.Monad

import GHC.Unit
import GHC.Unit.Module.ModGuts

import GHC.Types.SrcLoc

hscDesugar' :: ModLocation -> TcGblEnv -> Hsc ModGuts
hscSimplify :: HscEnv -> [String] -> ModGuts -> IO ModGuts
hscCompileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO (ForeignHValue, [LinkableUsage], PkgsLoaded)
