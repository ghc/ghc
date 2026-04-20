{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -fprof-auto-top #-}

-------------------------------------------------------------------------------
--
-- | Main API for compiling plain Haskell source code.
--
-- This module implements compilation of a Haskell source. It is
-- /not/ concerned with preprocessing of source files; this is handled
-- in "GHC.Driver.Pipeline"
--
-- There are various entry points depending on what mode we're in:
-- "batch" mode (@--make@), "one-shot" mode (@-c@, @-S@ etc.), and
-- "interactive" mode (GHCi). There are also entry points for
-- individual passes: parsing, typechecking/renaming, desugaring, and
-- simplification.
--
-- All the functions here take an 'HscEnv' as a parameter, but none of
-- them return a new one: 'HscEnv' is treated as an immutable value
-- from here on in (although it has mutable components, for the
-- caches).
--
-- We use the Hsc monad to deal with warning messages consistently:
-- specifically, while executing within an Hsc monad, warnings are
-- collected. When a Hsc monad returns to an IO monad, the
-- warnings are printed, or compilation aborts if the @-Werror@
-- flag is enabled.
--
-- For now we split this API roughly across the modules:
-- * GHC.Driver.Main.Compile
-- * GHC.Driver.Main.Hsc
-- * GHC.Driver.Main.Interactive
-- * GHC.Driver.Main.Passes
--
-- (c) The GRASP/AQUA Project, Glasgow University, 1993-2000
--
-------------------------------------------------------------------------------

module GHC.Driver.Main
    (
    -- * Making an HscEnv
      newHscEnv
    , newHscEnvWithHUG
    , initHscEnv

    -- * Compiling complete source files
    , Messager, batchMsg, batchMultiMsg
    , HscBackendAction (..), HscRecompStatus (..)
    , initModDetails
    , initWholeCoreBindings
    , loadIfaceByteCode
    , loadIfaceByteCodeLazy
    , hscMaybeWriteIface
    , hscCompileCmmFile
    , hscGenHardCode

    , hscInteractive
    , mkCgInteractiveGuts
    , CgInteractiveGuts
    , generateAndWriteByteCodeLinkable
    , generateFreshByteCodeLinkable

    -- * Running passes separately
    , hscRecompStatus
    , hscParse
    , hscTypecheckRename
    , hscTypecheckRenameWithDiagnostics
    , hscTypecheckAndGetWarnings
    , hscDesugar
    , makeSimpleDetails
    , hscSimplify -- ToDo, shouldn't really export this
    , hscDesugarAndSimplify

    -- * Safe Haskell
    , hscCheckSafe
    , hscGetSafe

    -- * Support for interactive evaluation
    , hscParseIdentifier
    , hscTcRcLookupName
    , hscTcRnGetInfo
    , hscIsGHCiMonad
    , hscGetModuleInterface
    , hscRnImportDecls
    , hscTcRnLookupRdrName
    , hscStmt, hscParseStmtWithLocation, hscStmtWithLocation, hscParsedStmt
    , hscParseDeclsWithLocation, hscParsedDecls
    , hscParseModuleWithLocation
    , hscTcExpr, TcRnExprMode(..), hscImport, hscKcType
    , hscParseExpr
    , hscParseType
    , hscCompileCoreExpr
    , hscTidy


    -- * Low-level exports for hooks
    , hscCompileCoreExpr'
      -- We want to make sure that we export enough to be able to redefine
      -- hsc_typecheck in client code
    , hscParse', hscSimplify', hscDesugar', tcRnModule', doCodeGen
    , getHscEnv
    , hscSimpleIface'
    , oneShotMsg
    , dumpIfaceStats
    , ioMsgMaybe
    , showModuleIndex
    , writeInterfaceOnlyMode
    , genModDetails
    ) where


import GHC.Driver.Main.Hsc
import GHC.Driver.Main.Compile
import GHC.Driver.Main.Passes
import GHC.Driver.Main.Interactive

import GHC.Driver.Env
import GHC.Driver.Messager ( Messager, oneShotMsg, batchMsg, batchMultiMsg, showModuleIndex )
