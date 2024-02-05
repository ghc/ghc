{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
  -- Don't warn that `type instance DsForeignsHooks = ...`
  -- is an orphan; see Note [The Decoupling Abstract Data Hack]
  -- in GHC.Driver.Hooks

-- | Various types used during desugaring.
module GHC.HsToCore.Types (
        DsM, DsLclEnv(..), DsGblEnv(..),
        DsMetaEnv, DsMetaVal(..), CompleteMatches
    ) where

import GHC.Prelude (Int)

import Data.IORef

import GHC.Types.CostCentre.State
import GHC.Types.Error
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Name.Reader (GlobalRdrEnv)

import GHC.Hs (LForeignDecl, HsExpr, GhcTc)

import GHC.Tc.Types (TcRnIf, IfGblEnv, IfLclEnv)

import GHC.HsToCore.Pmc.Types (Nablas)
import GHC.HsToCore.Errors.Types

import GHC.Core (CoreExpr)
import GHC.Core.FamInstEnv
import GHC.Utils.Outputable as Outputable
import GHC.Unit.Module
import GHC.Driver.Hooks (DsForeignsHook)
import GHC.Data.OrdList (OrdList)

import GHC.Types.ForeignStubs (ForeignStubs)
import GHC.Types.CompleteMatch

import Data.Maybe( Maybe )

{-
************************************************************************
*                                                                      *
                Desugarer monad
*                                                                      *
************************************************************************

Now the mondo monad magic (yes, @DsM@ is a silly name)---carry around
a @UniqueSupply@ and some annotations, which
presumably include source-file location information:
-}

-- | Global read-only context and state of the desugarer.
-- The statefulness is implemented through 'IORef's.
data DsGblEnv
  = DsGblEnv
  { ds_mod          :: Module             -- For SCC profiling
  , ds_fam_inst_env :: FamInstEnv         -- Like tcg_fam_inst_env
  , ds_gbl_rdr_env  :: GlobalRdrEnv       -- needed only for the following reasons:
                                          --    - to know what newtype constructors are in scope
                                          --    - to check whether all members of a COMPLETE pragma are in scope
  , ds_name_ppr_ctx :: NamePprCtx
  , ds_msgs    :: IORef (Messages DsMessage) -- Diagnostic messages
  , ds_if_env  :: (IfGblEnv, IfLclEnv)    -- Used for looking up global,
                                          -- possibly-imported things
  , ds_complete_matches :: DsCompleteMatches
     -- Additional complete pattern matches
  , ds_cc_st   :: IORef CostCentreState
     -- Tracking indices for cost centre annotations
  , ds_next_wrapper_num :: IORef (ModuleEnv Int)
    -- ^ See Note [Generating fresh names for FFI wrappers]
  }

instance ContainsModule DsGblEnv where
  extractModule = ds_mod

-- | Local state of the desugarer, extended as we lexically descend
data DsLclEnv
  = DsLclEnv
  { dsl_meta    :: DsMetaEnv   -- ^ Template Haskell bindings
  , dsl_loc     :: RealSrcSpan -- ^ To put in pattern-matching error msgs
  , dsl_nablas  :: Nablas
  -- ^ See Note [Long-distance information] in "GHC.HsToCore.Pmc".
  -- The set of reaching values Nablas is augmented as we walk inwards, refined
  -- through each pattern match in turn

  , dsl_unspecables :: Maybe VarSet
  -- ^ See Note [Desugaring non-canonical evidence]
  -- This field collects all un-specialisable evidence variables in scope.
  -- Nothing <=> don't collect this info (used for the LHS of Rules)
  }

-- Inside [| |] brackets, the desugarer looks
-- up variables in the DsMetaEnv
type DsMetaEnv = NameEnv DsMetaVal

data DsMetaVal
  = DsBound Id         -- Bound by a pattern inside the [| |].
                       -- Will be dynamically alpha renamed.
                       -- The Id has type THSyntax.Var

  | DsSplice (HsExpr GhcTc) -- These bindings are introduced by
                            -- the PendingSplices on a Hs*Bracket

-- | Desugaring monad. See also 'TcM'.
type DsM = TcRnIf DsGblEnv DsLclEnv

-- See Note [The Decoupling Abstract Data Hack]
type instance DsForeignsHook = [LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
