-- \section[Hooks]{Low level API hooks}

-- NB: this module is SOURCE-imported by DynFlags, and should primarily
--     refer to *types*, rather than *code*

{-# LANGUAGE CPP, RankNTypes #-}

module GHC.Driver.Hooks
   ( Hooks
   , emptyHooks
   , lookupHook
   , getHooked
     -- the hooks:
   , dsForeignsHook
   , tcForeignImportsHook
   , tcForeignExportsHook
   , hscFrontendHook
   , hscCompileCoreExprHook
   , ghcPrimIfaceHook
   , runPhaseHook
   , runMetaHook
   , linkHook
   , runRnSpliceHook
   , getValueSafelyHook
   , createIservProcessHook
   , stgToCmmHook
   , cmmToRawCmmHook
   )
where

import GhcPrelude

import GHC.Driver.Session
import GHC.Driver.Pipeline.Monad
import GHC.Driver.Types
import GHC.Hs.Decls
import GHC.Hs.Binds
import GHC.Hs.Expr
import OrdList
import TcRnTypes
import Bag
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Id
import GHC.Core
import GHCi.RemoteTypes
import GHC.Types.SrcLoc
import GHC.Core.Type
import System.Process
import GHC.Types.Basic
import GHC.Types.Module
import GHC.Core.TyCon
import GHC.Types.CostCentre
import GHC.Stg.Syntax
import Stream
import GHC.Cmm
import GHC.Hs.Extension
import GHC.StgToCmm.Types (ModuleLFInfos)

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
  , hscCompileCoreExprHook = Nothing
  , ghcPrimIfaceHook       = Nothing
  , runPhaseHook           = Nothing
  , runMetaHook            = Nothing
  , linkHook               = Nothing
  , runRnSpliceHook        = Nothing
  , getValueSafelyHook     = Nothing
  , createIservProcessHook = Nothing
  , stgToCmmHook           = Nothing
  , cmmToRawCmmHook        = Nothing
  }

data Hooks = Hooks
  { dsForeignsHook         :: Maybe ([LForeignDecl GhcTc]
                           -> DsM (ForeignStubs, OrdList (Id, CoreExpr)))
  , tcForeignImportsHook   :: Maybe ([LForeignDecl GhcRn]
                          -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt))
  , tcForeignExportsHook   :: Maybe ([LForeignDecl GhcRn]
            -> TcM (LHsBinds GhcTcId, [LForeignDecl GhcTcId], Bag GlobalRdrElt))
  , hscFrontendHook        :: Maybe (ModSummary -> Hsc FrontendResult)
  , hscCompileCoreExprHook ::
               Maybe (HscEnv -> SrcSpan -> CoreExpr -> IO ForeignHValue)
  , ghcPrimIfaceHook       :: Maybe ModIface
  , runPhaseHook           :: Maybe (PhasePlus -> FilePath -> DynFlags
                                         -> CompPipeline (PhasePlus, FilePath))
  , runMetaHook            :: Maybe (MetaHook TcM)
  , linkHook               :: Maybe (GhcLink -> DynFlags -> Bool
                                         -> HomePackageTable -> IO SuccessFlag)
  , runRnSpliceHook        :: Maybe (HsSplice GhcRn -> RnM (HsSplice GhcRn))
  , getValueSafelyHook     :: Maybe (HscEnv -> Name -> Type
                                                          -> IO (Maybe HValue))
  , createIservProcessHook :: Maybe (CreateProcess -> IO ProcessHandle)
  , stgToCmmHook           :: Maybe (DynFlags -> Module -> [TyCon] -> CollectedCCs
                                 -> [CgStgTopBinding] -> HpcInfo -> Stream IO CmmGroup ModuleLFInfos)
  , cmmToRawCmmHook        :: forall a . Maybe (DynFlags -> Maybe Module -> Stream IO CmmGroupSRTs a
                                 -> IO (Stream IO RawCmmGroup a))
  }

getHooked :: (Functor f, HasDynFlags f) => (Hooks -> Maybe a) -> a -> f a
getHooked hook def = fmap (lookupHook hook def) getDynFlags

lookupHook :: (Hooks -> Maybe a) -> a -> DynFlags -> a
lookupHook hook def = fromMaybe def . hook . hooks
