{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Various types used during desugaring.
module GHC.HsToCore.Types (
        DsM, DsLclEnv(..), DsGblEnv(..),
        DsMetaEnv, DsMetaVal(..), CompleteMatches
    ) where

import Data.IORef

import GHC.Types.CostCentre.State
import GHC.Types.Name.Env
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Types.Name.Reader (GlobalRdrEnv)
import GHC.Hs (LForeignDecl, HsExpr, GhcTc)
import GHC.Tc.Types (TcRnIf, IfGblEnv, IfLclEnv, CompleteMatches)
import GHC.HsToCore.PmCheck.Types (Nablas)
import GHC.Core (CoreExpr)
import GHC.Core.FamInstEnv
import GHC.Utils.Error
import GHC.Utils.Outputable as Outputable
import GHC.Unit.Module
import GHC.Driver.Hooks (DsForeignsHook)
import GHC.Data.OrdList (OrdList)
import GHC.Driver.Types (ForeignStubs)

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
  , ds_gbl_rdr_env  :: GlobalRdrEnv       -- needed *only* to know what newtype
                                          -- constructors are in scope during
                                          -- pattern-match satisfiability checking
  , ds_unqual  :: PrintUnqualified
  , ds_msgs    :: IORef Messages          -- Warning messages
  , ds_if_env  :: (IfGblEnv, IfLclEnv)    -- Used for looking up global,
                                          -- possibly-imported things
  , ds_complete_matches :: CompleteMatches
     -- Additional complete pattern matches
  , ds_cc_st   :: IORef CostCentreState
     -- Tracking indices for cost centre annotations
  }

instance ContainsModule DsGblEnv where
  extractModule = ds_mod

-- | Local state of the desugarer, extended as we lexically descend
data DsLclEnv
  = DsLclEnv
  { dsl_meta    :: DsMetaEnv   -- ^ Template Haskell bindings
  , dsl_loc     :: RealSrcSpan -- ^ To put in pattern-matching error msgs
  , dsl_nablas  :: Nablas
  -- ^ See Note [Note [Long-distance information] in "GHC.HsToCore.PmCheck".
  -- The set of reaching values Nablas is augmented as we walk inwards, refined
  -- through each pattern match in turn
  }

-- Inside [| |] brackets, the desugarer looks
-- up variables in the DsMetaEnv
type DsMetaEnv = NameEnv DsMetaVal

data DsMetaVal
  = DsBound Id         -- Bound by a pattern inside the [| |].
                       -- Will be dynamically alpha renamed.
                       -- The Id has type THSyntax.Var

  | DsSplice (HsExpr GhcTc) -- These bindings are introduced by
                            -- the PendingSplices on a HsBracketOut

-- | Desugaring monad. See also 'TcM'.
type DsM = TcRnIf DsGblEnv DsLclEnv

-- See Note [The Decoupling Abstract Data Hack]
type instance DsForeignsHook = [LForeignDecl GhcTc] -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
