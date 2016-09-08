-- \section[Hooks]{Low level API hooks}

-- NB: this module is SOURCE-imported by DynFlags, and should primarily
--     refer to *types*, rather than *code*
-- If you import too muchhere , then the revolting compiler_stage2_dll0_MODULES
-- stuff in compiler/ghc.mk makes DynFlags link to too much stuff

{-# LANGUAGE CPP #-}
module Hooks ( Hooks
             , emptyHooks
             , lookupHook
             , getHooked
               -- the hooks:
             , dsForeignsHook
             , tcForeignImportsHook
             , tcForeignExportsHook
             , hscFrontendHook
#ifdef GHCI
             , hscCompileCoreExprHook
#endif
             , ghcPrimIfaceHook
             , runPhaseHook
             , runMetaHook
             , linkHook
             , runRnSpliceHook
#ifdef GHCI
             , getValueSafelyHook
             , createIservProcessHook
#endif
             ) where

import DynFlags
import Name
import PipelineMonad
import HscTypes
import HsDecls
import HsBinds
import HsExpr
import OrdList
import Id
import TcRnTypes
import Bag
import RdrName
import CoreSyn
#ifdef GHCI
import GHCi.RemoteTypes
import SrcLoc
import Type
import System.Process
#endif
import BasicTypes

import Data.Maybe

{-
************************************************************************
*                                                                      *
\subsection{Hooks}
*                                                                      *
************************************************************************
-}

-- | Hooks can be used by GHC API clients to replace parts of
--   the compiler pipeline. If a hook is not installed, GHC
--   uses the default built-in behaviour

emptyHooks :: Hooks
emptyHooks = Hooks
  { dsForeignsHook         = Nothing
  , tcForeignImportsHook   = Nothing
  , tcForeignExportsHook   = Nothing
  , hscFrontendHook        = Nothing
#ifdef GHCI
  , hscCompileCoreExprHook = Nothing
#endif
  , ghcPrimIfaceHook       = Nothing
  , runPhaseHook           = Nothing
  , runMetaHook            = Nothing
  , linkHook               = Nothing
  , runRnSpliceHook        = Nothing
#ifdef GHCI
  , getValueSafelyHook     = Nothing
  , createIservProcessHook = Nothing
#endif
  }

data Hooks = Hooks
  { dsForeignsHook         :: Maybe ([LForeignDecl Id] -> DsM (ForeignStubs, OrdList (Id, CoreExpr)))
  , tcForeignImportsHook   :: Maybe ([LForeignDecl Name] -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt))
  , tcForeignExportsHook   :: Maybe ([LForeignDecl Name] -> TcM (LHsBinds TcId, [LForeignDecl TcId], Bag GlobalRdrElt))
  , hscFrontendHook        :: Maybe (ModSummary -> Hsc FrontendResult)
#ifdef GHCI
  , hscCompileCoreExprHook :: Maybe (HscEnv -> SrcSpan -> CoreExpr -> IO ForeignHValue)
#endif
  , ghcPrimIfaceHook       :: Maybe ModIface
  , runPhaseHook           :: Maybe (PhasePlus -> FilePath -> DynFlags -> CompPipeline (PhasePlus, FilePath))
  , runMetaHook            :: Maybe (MetaHook TcM)
  , linkHook               :: Maybe (GhcLink -> DynFlags -> Bool -> HomePackageTable -> IO SuccessFlag)
  , runRnSpliceHook        :: Maybe (HsSplice Name -> RnM (HsSplice Name))
#ifdef GHCI
  , getValueSafelyHook     :: Maybe (HscEnv -> Name -> Type -> IO (Maybe HValue))
  , createIservProcessHook :: Maybe (CreateProcess -> IO ProcessHandle)
#endif
  }

getHooked :: (Functor f, HasDynFlags f) => (Hooks -> Maybe a) -> a -> f a
getHooked hook def = fmap (lookupHook hook def) getDynFlags

lookupHook :: (Hooks -> Maybe a) -> a -> DynFlags -> a
lookupHook hook def = fromMaybe def . hook . hooks
