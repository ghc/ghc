-- \section[Hooks]{Low level API hooks}

-- NB: this module is SOURCE-imported by DynFlags, and should primarily
--     refer to *types*, rather than *code*

{-# LANGUAGE CPP, RankNTypes, TypeFamilies #-}

module GHC.Driver.Hooks
   ( Hooks (..)
   , emptyHooks
     -- the hooks:
   , DsForeignsHook
   , TcForeignImportsHook
   , TcForeignExportsHook
   , CompileCoreExprHook
   , ThMetaHook
   , RnSpliceHook
   , StgToCmmHook
   , PhaseHook
   , GetValueSafelyHook
   , LinkHook
   , CmmToRawCmmHook
   , CreateIservProcessHook
   , GhcPrimIfaceHook
   )
where

import GHC.Prelude
import Data.Kind

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
type family DsForeignsHook         :: Type
type family TcForeignImportsHook   :: Type
type family TcForeignExportsHook   :: Type
type family CompileCoreExprHook    :: Type
type family ThMetaHook             :: Type
type family RnSpliceHook           :: Type
type family StgToCmmHook           :: Type
type family PhaseHook              :: Type
type family GetValueSafelyHook     :: Type
type family LinkHook               :: Type
data family CmmToRawCmmHook        :: Type
type family CreateIservProcessHook :: Type
type family GhcPrimIfaceHook       :: Type

data Hooks = Hooks
  { dsForeignsHook         :: Maybe DsForeignsHook
  , tcForeignImportsHook   :: Maybe TcForeignImportsHook
  , tcForeignExportsHook   :: Maybe TcForeignExportsHook
  , hscCompileCoreExprHook :: Maybe CompileCoreExprHook
  , ghcPrimIfaceHook       :: Maybe GhcPrimIfaceHook
  , runPhaseHook           :: Maybe PhaseHook
  , runMetaHook            :: Maybe ThMetaHook
  , linkHook               :: Maybe LinkHook
  , runRnSpliceHook        :: Maybe RnSpliceHook
  , getValueSafelyHook     :: Maybe GetValueSafelyHook
  , createIservProcessHook :: Maybe CreateIservProcessHook
  , stgToCmmHook           :: Maybe StgToCmmHook
  , cmmToRawCmmHook        :: Maybe CmmToRawCmmHook
  }
