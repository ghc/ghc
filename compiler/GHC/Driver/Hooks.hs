-- \section[Hooks]{Low level API hooks}

-- NB: this module is SOURCE-imported by DynFlags, and should primarily
--     refer to *types*, rather than *code*

{-# LANGUAGE CPP, RankNTypes, TypeFamilies #-}

module GHC.Driver.Hooks
   ( Hooks
   , emptyHooks
   , lookupHook
   , getHooked
     -- the hooks:
   , DsForeignsHook
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

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session
import GHC.Driver.Pipeline.Monad

import GHC.Hs.Decls
import GHC.Hs.Binds
import GHC.Hs.Expr
import GHC.Hs.Extension

import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Id
import GHC.Types.SrcLoc
import GHC.Types.Basic
import GHC.Types.CostCentre
import GHC.Types.IPE
import GHC.Types.Meta
import GHC.Types.HpcInfo

import GHC.Unit.Module
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Home.ModInfo

import GHC.Core
import GHC.Core.TyCon
import GHC.Core.Type

import GHC.Tc.Types
import GHC.Stg.Syntax
import GHC.StgToCmm.Types (ModuleLFInfos)
import GHC.Cmm

import GHCi.RemoteTypes

import GHC.Data.Stream
import GHC.Data.Bag
import Data.IORef

import Data.Maybe
import qualified Data.Kind
import System.Process

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

{- Note [The Decoupling Abstract Data Hack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The "Abstract Data" idea is due to Richard Eisenberg in
https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1957, where the pattern is
described in more detail.

Here we use it as a temporary measure to break the dependency from the Parser on
the Desugarer until the parser is free of DynFlags. We introduced a nullary type
family @DsForeignsook@, whose single definition is in GHC.HsToCore.Types, where
we instantiate it to

   [LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList (Id, CoreExpr))

In doing so, the Hooks module (which is an hs-boot dependency of DynFlags) can
be decoupled from its use of the DsM definition in GHC.HsToCore.Types. Since
both DsM and the definition of @ForeignsHook@ live in the same module, there is
virtually no difference for plugin authors that want to write a foreign hook.
-}

-- See Note [The Decoupling Abstract Data Hack]
type family DsForeignsHook :: Data.Kind.Type

data Hooks = Hooks
  { dsForeignsHook         :: Maybe DsForeignsHook
  -- ^ Actual type:
  -- @Maybe ([LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList (Id, CoreExpr)))@
  , tcForeignImportsHook   :: Maybe ([LForeignDecl GhcRn]
                          -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt))
  , tcForeignExportsHook   :: Maybe ([LForeignDecl GhcRn]
            -> TcM (LHsBinds GhcTc, [LForeignDecl GhcTc], Bag GlobalRdrElt))
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
  , stgToCmmHook           :: Maybe (DynFlags -> Module -> InfoTableProvMap -> [TyCon] -> CollectedCCs
                                 -> [CgStgTopBinding] -> HpcInfo -> IORef [CmmInfoTable] -> Stream IO CmmGroup ModuleLFInfos)
  , cmmToRawCmmHook        :: forall a . Maybe (DynFlags -> Maybe Module -> Stream IO CmmGroupSRTs a
                                 -> IO (Stream IO RawCmmGroup a))
  }

getHooked :: (Functor f, HasDynFlags f) => (Hooks -> Maybe a) -> a -> f a
getHooked hook def = fmap (lookupHook hook def) getDynFlags

lookupHook :: (Hooks -> Maybe a) -> a -> DynFlags -> a
lookupHook hook def = fromMaybe def . hook . hooks
