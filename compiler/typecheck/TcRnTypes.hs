{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-2002


Various types used during typechecking, please see TcRnMonad as well for
operations on these types. You probably want to import it, instead of this
module.

All the monads exported here are built on top of the same IOEnv monad. The
monad functions like a Reader monad in the way it passes the environment
around. This is done to allow the environment to be manipulated in a stack
like fashion when entering expressions... etc.

For state that is global and should be returned at the end (e.g not part
of the stack mechanism), you should use a TcRef (= IORef) to store them.
-}

{-# LANGUAGE CPP, ExistentialQuantification, GeneralizedNewtypeDeriving,
             ViewPatterns #-}

module TcRnTypes(
        TcRnIf, TcRn, TcM, RnM, IfM, IfL, IfG, -- The monad is opaque outside this module
        TcRef,

        -- The environment types
        Env(..),
        TcGblEnv(..), TcLclEnv(..),
        IfGblEnv(..), IfLclEnv(..),
        tcVisibleOrphanMods,

        -- Frontend types (shouldn't really be here)
        FrontendResult(..),

        -- Renamer types
        ErrCtxt, RecFieldEnv,
        ImportAvails(..), emptyImportAvails, plusImportAvails,
        WhereFrom(..), mkModDeps, modDepsElts,

        -- Typechecker types
        TcTypeEnv, TcBinderStack, TcBinder(..),
        TcTyThing(..), PromotionErr(..),
        IdBindingInfo(..), ClosedTypeId, RhsNames,
        IsGroupClosed(..),
        SelfBootInfo(..),
        pprTcTyThingCategory, pprPECategory, CompleteMatch(..),

        -- Desugaring types
        DsM, DsLclEnv(..), DsGblEnv(..),
        DsMetaEnv, DsMetaVal(..), CompleteMatchMap,
        mkCompleteMatchMap, extendCompleteMatchMap,

        -- Template Haskell
        ThStage(..), SpliceType(..), PendingStuff(..),
        topStage, topAnnStage, topSpliceStage,
        ThLevel, impLevel, outerLevel, thLevel,
        ForeignSrcLang(..),

        -- Arrows
        ArrowCtxt(..),

        -- TcSigInfo
        TcSigFun, TcSigInfo(..), TcIdSigInfo(..),
        TcIdSigInst(..), TcPatSynInfo(..),
        isPartialSig, hasCompleteSig,

        -- QCInst
        QCInst(..), isPendingScInst,

        -- Canonical constraints
        Xi, Ct(..), Cts, emptyCts, andCts, andManyCts, pprCts,
        singleCt, listToCts, ctsElts, consCts, snocCts, extendCtsList,
        isEmptyCts, isCTyEqCan, isCFunEqCan,
        isPendingScDict, superClassesMightHelp, getPendingWantedScs,
        isCDictCan_Maybe, isCFunEqCan_maybe,
        isCNonCanonical, isWantedCt, isDerivedCt,
        isGivenCt, isHoleCt, isOutOfScopeCt, isExprHoleCt, isTypeHoleCt,
        isUserTypeErrorCt, getUserTypeErrorMsg,
        ctEvidence, ctLoc, setCtLoc, ctPred, ctFlavour, ctEqRel, ctOrigin,
        ctEvId, mkTcEqPredLikeEv,
        mkNonCanonical, mkNonCanonicalCt, mkGivens,
        mkIrredCt, mkInsolubleCt,
        ctEvPred, ctEvLoc, ctEvOrigin, ctEvEqRel,
        ctEvExpr, ctEvTerm, ctEvCoercion, ctEvEvId,
        tyCoVarsOfCt, tyCoVarsOfCts,
        tyCoVarsOfCtList, tyCoVarsOfCtsList,

        WantedConstraints(..), insolubleWC, emptyWC, isEmptyWC,
        isSolvedWC, andWC, unionsWC, mkSimpleWC, mkImplicWC,
        addInsols, insolublesOnly, addSimples, addImplics,
        tyCoVarsOfWC, dropDerivedWC, dropDerivedSimples,
        tyCoVarsOfWCList, insolubleCt, insolubleEqCt,
        isDroppableCt, insolubleImplic,
        arisesFromGivens,

        Implication(..), newImplication, implicationPrototype,
        implicLclEnv, implicDynFlags,
        ImplicStatus(..), isInsolubleStatus, isSolvedStatus,
        SubGoalDepth, initialSubGoalDepth, maxSubGoalDepth,
        bumpSubGoalDepth, subGoalDepthExceeded,
        CtLoc(..), ctLocSpan, ctLocEnv, ctLocLevel, ctLocOrigin,
        ctLocTypeOrKind_maybe,
        ctLocDepth, bumpCtLocDepth, isGivenLoc,
        setCtLocOrigin, updateCtLocOrigin, setCtLocEnv, setCtLocSpan,
        CtOrigin(..), exprCtOrigin, lexprCtOrigin, matchesCtOrigin, grhssCtOrigin,
        isVisibleOrigin, toInvisibleOrigin,
        TypeOrKind(..), isTypeLevel, isKindLevel,
        pprCtOrigin, pprCtLoc,
        pushErrCtxt, pushErrCtxtSameOrigin,


        SkolemInfo(..), pprSigSkolInfo, pprSkolInfo,

        CtEvidence(..), TcEvDest(..),
        mkKindLoc, toKindLoc, mkGivenLoc,
        isWanted, isGiven, isDerived, isGivenOrWDeriv,
        ctEvRole,

        wrapType, wrapTypeWithImplication,
        removeBindingShadowing,

        -- Constraint solver plugins
        TcPlugin(..), TcPluginResult(..), TcPluginSolver,
        TcPluginM, runTcPluginM, unsafeTcPluginTcM,
        getEvBindsTcPluginM,

        CtFlavour(..), ShadowInfo(..), ctEvFlavour,
        CtFlavourRole, ctEvFlavourRole, ctFlavourRole,
        eqCanRewrite, eqCanRewriteFR, eqMayRewriteFR,
        eqCanDischargeFR,
        funEqCanDischarge, funEqCanDischargeF,

        -- Pretty printing
        pprEvVarTheta,
        pprEvVars, pprEvVarWithType,

        -- Misc other types
        TcId, TcIdSet,
        Hole(..), holeOcc,
        NameShape(..),

        -- Role annotations
        RoleAnnotEnv, emptyRoleAnnotEnv, mkRoleAnnotEnv,
        lookupRoleAnnot, getRoleAnnots,

  ) where

#include "HsVersions.h"

import GhcPrelude

import HsSyn
import CoreSyn
import HscTypes
import TcEvidence
import Type
import Class    ( Class )
import TyCon    ( TyCon, TyConFlavour, tyConKind )
import TyCoRep  ( coHoleCoVar )
import Coercion ( Coercion, mkHoleCo )
import ConLike  ( ConLike(..) )
import DataCon  ( DataCon, dataConUserType, dataConOrigArgTys )
import PatSyn   ( PatSyn, pprPatSynType )
import Id       ( idType, idName )
import FieldLabel ( FieldLabel )
import TcType
import Annotations
import InstEnv
import FamInstEnv
import PmExpr
import IOEnv
import RdrName
import Name
import NameEnv
import NameSet
import Avail
import Var
import FV
import VarEnv
import Module
import SrcLoc
import VarSet
import ErrUtils
import UniqFM
import UniqSupply
import BasicTypes
import Bag
import DynFlags
import Outputable
import ListSetOps
import FastString
import qualified GHC.LanguageExtensions as LangExt
import Fingerprint
import Util
import PrelNames ( isUnboundName )
import CostCentreState

import Control.Monad (ap, liftM, msum)
import qualified Control.Monad.Fail as MonadFail
import Data.Set      ( Set )
import qualified Data.Set as S

import Data.List ( sort )
import Data.Map ( Map )
import Data.Dynamic  ( Dynamic )
import Data.Typeable ( TypeRep )
import Data.Maybe    ( mapMaybe )
import GHCi.Message
import GHCi.RemoteTypes

import qualified Language.Haskell.TH as TH

-- | A 'NameShape' is a substitution on 'Name's that can be used
-- to refine the identities of a hole while we are renaming interfaces
-- (see 'RnModIface').  Specifically, a 'NameShape' for
-- 'ns_module_name' @A@, defines a mapping from @{A.T}@
-- (for some 'OccName' @T@) to some arbitrary other 'Name'.
--
-- The most intruiging thing about a 'NameShape', however, is
-- how it's constructed.  A 'NameShape' is *implied* by the
-- exported 'AvailInfo's of the implementor of an interface:
-- if an implementor of signature @<H>@ exports @M.T@, you implicitly
-- define a substitution from @{H.T}@ to @M.T@.  So a 'NameShape'
-- is computed from the list of 'AvailInfo's that are exported
-- by the implementation of a module, or successively merged
-- together by the export lists of signatures which are joining
-- together.
--
-- It's not the most obvious way to go about doing this, but it
-- does seem to work!
--
-- NB: Can't boot this and put it in NameShape because then we
-- start pulling in too many DynFlags things.
data NameShape = NameShape {
        ns_mod_name :: ModuleName,
        ns_exports :: [AvailInfo],
        ns_map :: OccEnv Name
    }


{-
************************************************************************
*                                                                      *
               Standard monad definition for TcRn
    All the combinators for the monad can be found in TcRnMonad
*                                                                      *
************************************************************************

The monad itself has to be defined here, because it is mentioned by ErrCtxt
-}

type TcRnIf a b = IOEnv (Env a b)
type TcRn       = TcRnIf TcGblEnv TcLclEnv    -- Type inference
type IfM lcl    = TcRnIf IfGblEnv lcl         -- Iface stuff
type IfG        = IfM ()                      --    Top level
type IfL        = IfM IfLclEnv                --    Nested
type DsM        = TcRnIf DsGblEnv DsLclEnv    -- Desugaring

-- TcRn is the type-checking and renaming monad: the main monad that
-- most type-checking takes place in.  The global environment is
-- 'TcGblEnv', which tracks all of the top-level type-checking
-- information we've accumulated while checking a module, while the
-- local environment is 'TcLclEnv', which tracks local information as
-- we move inside expressions.

-- | Historical "renaming monad" (now it's just 'TcRn').
type RnM  = TcRn

-- | Historical "type-checking monad" (now it's just 'TcRn').
type TcM  = TcRn

-- We 'stack' these envs through the Reader like monad infrastructure
-- as we move into an expression (although the change is focused in
-- the lcl type).
data Env gbl lcl
  = Env {
        env_top  :: !HscEnv, -- Top-level stuff that never changes
                             -- Includes all info about imported things
                             -- BangPattern is to fix leak, see #15111

        env_us   :: {-# UNPACK #-} !(IORef UniqSupply),
                             -- Unique supply for local variables

        env_gbl  :: gbl,     -- Info about things defined at the top level
                             -- of the module being compiled

        env_lcl  :: lcl      -- Nested stuff; changes as we go into
    }

instance ContainsDynFlags (Env gbl lcl) where
    extractDynFlags env = hsc_dflags (env_top env)

instance ContainsModule gbl => ContainsModule (Env gbl lcl) where
    extractModule env = extractModule (env_gbl env)


{-
************************************************************************
*                                                                      *
                The interface environments
              Used when dealing with IfaceDecls
*                                                                      *
************************************************************************
-}

data IfGblEnv
  = IfGblEnv {
        -- Some information about where this environment came from;
        -- useful for debugging.
        if_doc :: SDoc,
        -- The type environment for the module being compiled,
        -- in case the interface refers back to it via a reference that
        -- was originally a hi-boot file.
        -- We need the module name so we can test when it's appropriate
        -- to look in this env.
        -- See Note [Tying the knot] in TcIface
        if_rec_types :: Maybe (Module, IfG TypeEnv)
                -- Allows a read effect, so it can be in a mutable
                -- variable; c.f. handling the external package type env
                -- Nothing => interactive stuff, no loops possible
    }

data IfLclEnv
  = IfLclEnv {
        -- The module for the current IfaceDecl
        -- So if we see   f = \x -> x
        -- it means M.f = \x -> x, where M is the if_mod
        -- NB: This is a semantic module, see
        -- Note [Identity versus semantic module]
        if_mod :: Module,

        -- Whether or not the IfaceDecl came from a boot
        -- file or not; we'll use this to choose between
        -- NoUnfolding and BootUnfolding
        if_boot :: Bool,

        -- The field is used only for error reporting
        -- if (say) there's a Lint error in it
        if_loc :: SDoc,
                -- Where the interface came from:
                --      .hi file, or GHCi state, or ext core
                -- plus which bit is currently being examined

        if_nsubst :: Maybe NameShape,

        -- This field is used to make sure "implicit" declarations
        -- (anything that cannot be exported in mi_exports) get
        -- wired up correctly in typecheckIfacesForMerging.  Most
        -- of the time it's @Nothing@.  See Note [Resolving never-exported Names in TcIface]
        -- in TcIface.
        if_implicits_env :: Maybe TypeEnv,

        if_tv_env  :: FastStringEnv TyVar,     -- Nested tyvar bindings
        if_id_env  :: FastStringEnv Id         -- Nested id binding
    }

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

data DsGblEnv
        = DsGblEnv
        { ds_mod          :: Module             -- For SCC profiling
        , ds_fam_inst_env :: FamInstEnv         -- Like tcg_fam_inst_env
        , ds_unqual  :: PrintUnqualified
        , ds_msgs    :: IORef Messages          -- Warning messages
        , ds_if_env  :: (IfGblEnv, IfLclEnv)    -- Used for looking up global,
                                                -- possibly-imported things
        , ds_complete_matches :: CompleteMatchMap
           -- Additional complete pattern matches
        , ds_cc_st   :: IORef CostCentreState
           -- Tracking indices for cost centre annotations
        }

instance ContainsModule DsGblEnv where
    extractModule = ds_mod

data DsLclEnv = DsLclEnv {
        dsl_meta    :: DsMetaEnv,        -- Template Haskell bindings
        dsl_loc     :: RealSrcSpan,      -- To put in pattern-matching error msgs

        -- See Note [Note [Type and Term Equality Propagation] in Check.hs
        -- These two fields are augmented as we walk inwards,
        -- through each patttern match in turn
        dsl_dicts   :: Bag EvVar,     -- Constraints from GADT pattern-matching
        dsl_tm_cs   :: Bag SimpleEq,  -- Constraints form term-level pattern matching

        dsl_pm_iter :: IORef Int  -- Number of iterations for pmcheck so far
                                  -- We fail if this gets too big
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


{-
************************************************************************
*                                                                      *
                Global typechecker environment
*                                                                      *
************************************************************************
-}

-- | 'FrontendResult' describes the result of running the
-- frontend of a Haskell module.  Usually, you'll get
-- a 'FrontendTypecheck', since running the frontend involves
-- typechecking a program, but for an hs-boot merge you'll
-- just get a ModIface, since no actual typechecking occurred.
--
-- This data type really should be in HscTypes, but it needs
-- to have a TcGblEnv which is only defined here.
data FrontendResult
        = FrontendTypecheck TcGblEnv

-- Note [Identity versus semantic module]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- When typechecking an hsig file, it is convenient to keep track
-- of two different "this module" identifiers:
--
--      - The IDENTITY module is simply thisPackage + the module
--        name; i.e. it uniquely *identifies* the interface file
--        we're compiling.  For example, p[A=<A>]:A is an
--        identity module identifying the requirement named A
--        from library p.
--
--      - The SEMANTIC module, which is the actual module that
--        this signature is intended to represent (e.g. if
--        we have a identity module p[A=base:Data.IORef]:A,
--        then the semantic module is base:Data.IORef)
--
-- Which one should you use?
--
--      - In the desugarer and later phases of compilation,
--        identity and semantic modules coincide, since we never compile
--        signatures (we just generate blank object files for
--        hsig files.)
--
--        A corrolary of this is that the following invariant holds at any point
--        past desugaring,
--
--            if I have a Module, this_mod, in hand representing the module
--            currently being compiled,
--            then moduleUnitId this_mod == thisPackage dflags
--
--      - For any code involving Names, we want semantic modules.
--        See lookupIfaceTop in IfaceEnv, mkIface and addFingerprints
--        in MkIface, and tcLookupGlobal in TcEnv
--
--      - When reading interfaces, we want the identity module to
--        identify the specific interface we want (such interfaces
--        should never be loaded into the EPS).  However, if a
--        hole module <A> is requested, we look for A.hi
--        in the home library we are compiling.  (See LoadIface.)
--        Similarly, in RnNames we check for self-imports using
--        identity modules, to allow signatures to import their implementor.
--
--      - For recompilation avoidance, you want the identity module,
--        since that will actually say the specific interface you
--        want to track (and recompile if it changes)

-- | 'TcGblEnv' describes the top-level of the module at the
-- point at which the typechecker is finished work.
-- It is this structure that is handed on to the desugarer
-- For state that needs to be updated during the typechecking
-- phase and returned at end, use a 'TcRef' (= 'IORef').
data TcGblEnv
  = TcGblEnv {
        tcg_mod     :: Module,         -- ^ Module being compiled
        tcg_semantic_mod :: Module,    -- ^ If a signature, the backing module
            -- See also Note [Identity versus semantic module]
        tcg_src     :: HscSource,
          -- ^ What kind of module (regular Haskell, hs-boot, hsig)

        tcg_rdr_env :: GlobalRdrEnv,   -- ^ Top level envt; used during renaming
        tcg_default :: Maybe [Type],
          -- ^ Types used for defaulting. @Nothing@ => no @default@ decl

        tcg_fix_env   :: FixityEnv,     -- ^ Just for things in this module
        tcg_field_env :: RecFieldEnv,   -- ^ Just for things in this module
                                        -- See Note [The interactive package] in HscTypes

        tcg_type_env :: TypeEnv,
          -- ^ Global type env for the module we are compiling now.  All
          -- TyCons and Classes (for this module) end up in here right away,
          -- along with their derived constructors, selectors.
          --
          -- (Ids defined in this module start in the local envt, though they
          --  move to the global envt during zonking)
          --
          -- NB: for what "things in this module" means, see
          -- Note [The interactive package] in HscTypes

        tcg_type_env_var :: TcRef TypeEnv,
                -- Used only to initialise the interface-file
                -- typechecker in initIfaceTcRn, so that it can see stuff
                -- bound in this module when dealing with hi-boot recursions
                -- Updated at intervals (e.g. after dealing with types and classes)

        tcg_inst_env     :: !InstEnv,
          -- ^ Instance envt for all /home-package/ modules;
          -- Includes the dfuns in tcg_insts
          -- NB. BangPattern is to fix a leak, see #15111
        tcg_fam_inst_env :: !FamInstEnv, -- ^ Ditto for family instances
          -- NB. BangPattern is to fix a leak, see #15111
        tcg_ann_env      :: AnnEnv,     -- ^ And for annotations

                -- Now a bunch of things about this module that are simply
                -- accumulated, but never consulted until the end.
                -- Nevertheless, it's convenient to accumulate them along
                -- with the rest of the info from this module.
        tcg_exports :: [AvailInfo],     -- ^ What is exported
        tcg_imports :: ImportAvails,
          -- ^ Information about what was imported from where, including
          -- things bound in this module. Also store Safe Haskell info
          -- here about transitive trusted package requirements.
          --
          -- There are not many uses of this field, so you can grep for
          -- all them.
          --
          -- The ImportAvails records information about the following
          -- things:
          --
          --    1. All of the modules you directly imported (tcRnImports)
          --    2. The orphans (only!) of all imported modules in a GHCi
          --       session (runTcInteractive)
          --    3. The module that instantiated a signature
          --    4. Each of the signatures that merged in
          --
          -- It is used in the following ways:
          --    - imp_orphs is used to determine what orphan modules should be
          --      visible in the context (tcVisibleOrphanMods)
          --    - imp_finsts is used to determine what family instances should
          --      be visible (tcExtendLocalFamInstEnv)
          --    - To resolve the meaning of the export list of a module
          --      (tcRnExports)
          --    - imp_mods is used to compute usage info (mkIfaceTc, deSugar)
          --    - imp_trust_own_pkg is used for Safe Haskell in interfaces
          --      (mkIfaceTc, as well as in HscMain)
          --    - To create the Dependencies field in interface (mkDependencies)

        tcg_dus       :: DefUses,   -- ^ What is defined in this module and what is used.
        tcg_used_gres :: TcRef [GlobalRdrElt],  -- ^ Records occurrences of imported entities
          -- One entry for each occurrence; but may have different GREs for
          -- the same Name See Note [Tracking unused binding and imports]

        tcg_keep :: TcRef NameSet,
          -- ^ Locally-defined top-level names to keep alive.
          --
          -- "Keep alive" means give them an Exported flag, so that the
          -- simplifier does not discard them as dead code, and so that they
          -- are exposed in the interface file (but not to export to the
          -- user).
          --
          -- Some things, like dict-fun Ids and default-method Ids are "born"
          -- with the Exported flag on, for exactly the above reason, but some
          -- we only discover as we go.  Specifically:
          --
          --   * The to/from functions for generic data types
          --
          --   * Top-level variables appearing free in the RHS of an orphan
          --     rule
          --
          --   * Top-level variables appearing free in a TH bracket

        tcg_th_used :: TcRef Bool,
          -- ^ @True@ <=> Template Haskell syntax used.
          --
          -- We need this so that we can generate a dependency on the
          -- Template Haskell package, because the desugarer is going
          -- to emit loads of references to TH symbols.  The reference
          -- is implicit rather than explicit, so we have to zap a
          -- mutable variable.

        tcg_th_splice_used :: TcRef Bool,
          -- ^ @True@ <=> A Template Haskell splice was used.
          --
          -- Splices disable recompilation avoidance (see #481)

        tcg_th_top_level_locs :: TcRef (Set RealSrcSpan),
          -- ^ Locations of the top-level splices; used for providing details on
          -- scope in error messages for out-of-scope variables

        tcg_dfun_n  :: TcRef OccSet,
          -- ^ Allows us to choose unique DFun names.

        tcg_merged :: [(Module, Fingerprint)],
          -- ^ The requirements we merged with; we always have to recompile
          -- if any of these changed.

        -- The next fields accumulate the payload of the module
        -- The binds, rules and foreign-decl fields are collected
        -- initially in un-zonked form and are finally zonked in tcRnSrcDecls

        tcg_rn_exports :: Maybe [(Located (IE GhcRn), Avails)],
                -- Nothing <=> no explicit export list
                -- Is always Nothing if we don't want to retain renamed
                -- exports.
                -- If present contains each renamed export list item
                -- together with its exported names.

        tcg_rn_imports :: [LImportDecl GhcRn],
                -- Keep the renamed imports regardless.  They are not
                -- voluminous and are needed if you want to report unused imports

        tcg_rn_decls :: Maybe (HsGroup GhcRn),
          -- ^ Renamed decls, maybe.  @Nothing@ <=> Don't retain renamed
          -- decls.

        tcg_dependent_files :: TcRef [FilePath], -- ^ dependencies from addDependentFile

        tcg_th_topdecls :: TcRef [LHsDecl GhcPs],
        -- ^ Top-level declarations from addTopDecls

        tcg_th_foreign_files :: TcRef [(ForeignSrcLang, FilePath)],
        -- ^ Foreign files emitted from TH.

        tcg_th_topnames :: TcRef NameSet,
        -- ^ Exact names bound in top-level declarations in tcg_th_topdecls

        tcg_th_modfinalizers :: TcRef [(TcLclEnv, ThModFinalizers)],
        -- ^ Template Haskell module finalizers.
        --
        -- They can use particular local environments.

        tcg_th_coreplugins :: TcRef [String],
        -- ^ Core plugins added by Template Haskell code.

        tcg_th_state :: TcRef (Map TypeRep Dynamic),
        tcg_th_remote_state :: TcRef (Maybe (ForeignRef (IORef QState))),
        -- ^ Template Haskell state

        tcg_ev_binds  :: Bag EvBind,        -- Top-level evidence bindings

        -- Things defined in this module, or (in GHCi)
        -- in the declarations for a single GHCi command.
        -- For the latter, see Note [The interactive package] in HscTypes
        tcg_tr_module :: Maybe Id,   -- Id for $trModule :: GHC.Types.Module
                                             -- for which every module has a top-level defn
                                             -- except in GHCi in which case we have Nothing
        tcg_binds     :: LHsBinds GhcTc,     -- Value bindings in this module
        tcg_sigs      :: NameSet,            -- ...Top-level names that *lack* a signature
        tcg_imp_specs :: [LTcSpecPrag],      -- ...SPECIALISE prags for imported Ids
        tcg_warns     :: Warnings,           -- ...Warnings and deprecations
        tcg_anns      :: [Annotation],       -- ...Annotations
        tcg_tcs       :: [TyCon],            -- ...TyCons and Classes
        tcg_insts     :: [ClsInst],          -- ...Instances
        tcg_fam_insts :: [FamInst],          -- ...Family instances
        tcg_rules     :: [LRuleDecl GhcTc],  -- ...Rules
        tcg_fords     :: [LForeignDecl GhcTc], -- ...Foreign import & exports
        tcg_patsyns   :: [PatSyn],            -- ...Pattern synonyms

        tcg_doc_hdr   :: Maybe LHsDocString, -- ^ Maybe Haddock header docs
        tcg_hpc       :: !AnyHpcUsage,       -- ^ @True@ if any part of the
                                             --  prog uses hpc instrumentation.
           -- NB. BangPattern is to fix a leak, see #15111

        tcg_self_boot :: SelfBootInfo,       -- ^ Whether this module has a
                                             -- corresponding hi-boot file

        tcg_main      :: Maybe Name,         -- ^ The Name of the main
                                             -- function, if this module is
                                             -- the main module.

        tcg_safeInfer :: TcRef (Bool, WarningMessages),
        -- ^ Has the typechecker inferred this module as -XSafe (Safe Haskell)
        -- See Note [Safe Haskell Overlapping Instances Implementation],
        -- although this is used for more than just that failure case.

        tcg_tc_plugins :: [TcPluginSolver],
        -- ^ A list of user-defined plugins for the constraint solver.

        tcg_top_loc :: RealSrcSpan,
        -- ^ The RealSrcSpan this module came from

        tcg_static_wc :: TcRef WantedConstraints,
          -- ^ Wanted constraints of static forms.
        -- See Note [Constraints in static forms].
        tcg_complete_matches :: [CompleteMatch],

        -- ^ Tracking indices for cost centre annotations
        tcg_cc_st   :: TcRef CostCentreState
    }

-- NB: topModIdentity, not topModSemantic!
-- Definition sites of orphan identities will be identity modules, not semantic
-- modules.

-- Note [Constraints in static forms]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- When a static form produces constraints like
--
-- f :: StaticPtr (Bool -> String)
-- f = static show
--
-- we collect them in tcg_static_wc and resolve them at the end
-- of type checking. They need to be resolved separately because
-- we don't want to resolve them in the context of the enclosing
-- expression. Consider
--
-- g :: Show a => StaticPtr (a -> String)
-- g = static show
--
-- If the @Show a0@ constraint that the body of the static form produces was
-- resolved in the context of the enclosing expression, then the body of the
-- static form wouldn't be closed because the Show dictionary would come from
-- g's context instead of coming from the top level.

tcVisibleOrphanMods :: TcGblEnv -> ModuleSet
tcVisibleOrphanMods tcg_env
    = mkModuleSet (tcg_mod tcg_env : imp_orphs (tcg_imports tcg_env))

instance ContainsModule TcGblEnv where
    extractModule env = tcg_semantic_mod env

type RecFieldEnv = NameEnv [FieldLabel]
        -- Maps a constructor name *in this module*
        -- to the fields for that constructor.
        -- This is used when dealing with ".." notation in record
        -- construction and pattern matching.
        -- The FieldEnv deals *only* with constructors defined in *this*
        -- module.  For imported modules, we get the same info from the
        -- TypeEnv

data SelfBootInfo
  = NoSelfBoot    -- No corresponding hi-boot file
  | SelfBoot
       { sb_mds :: ModDetails   -- There was a hi-boot file,
       , sb_tcs :: NameSet }    -- defining these TyCons,
-- What is sb_tcs used for?  See Note [Extra dependencies from .hs-boot files]
-- in RnSource


{- Note [Tracking unused binding and imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We gather two sorts of usage information

 * tcg_dus (defs/uses)
      Records *defined* Names (local, top-level)
          and *used*    Names (local or imported)

      Used (a) to report "defined but not used"
               (see RnNames.reportUnusedNames)
           (b) to generate version-tracking usage info in interface
               files (see MkIface.mkUsedNames)
   This usage info is mainly gathered by the renamer's
   gathering of free-variables

 * tcg_used_gres
      Used only to report unused import declarations

      Records each *occurrence* an *imported* (not locally-defined) entity.
      The occurrence is recorded by keeping a GlobalRdrElt for it.
      These is not the GRE that is in the GlobalRdrEnv; rather it
      is recorded *after* the filtering done by pickGREs.  So it reflect
      /how that occurrence is in scope/.   See Note [GRE filtering] in
      RdrName.


************************************************************************
*                                                                      *
                The local typechecker environment
*                                                                      *
************************************************************************

Note [The Global-Env/Local-Env story]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During type checking, we keep in the tcg_type_env
        * All types and classes
        * All Ids derived from types and classes (constructors, selectors)

At the end of type checking, we zonk the local bindings,
and as we do so we add to the tcg_type_env
        * Locally defined top-level Ids

Why?  Because they are now Ids not TcIds.  This final GlobalEnv is
        a) fed back (via the knot) to typechecking the
           unfoldings of interface signatures
        b) used in the ModDetails of this module
-}

data TcLclEnv           -- Changes as we move inside an expression
                        -- Discarded after typecheck/rename; not passed on to desugarer
  = TcLclEnv {
        tcl_loc        :: RealSrcSpan,     -- Source span
        tcl_ctxt       :: [ErrCtxt],       -- Error context, innermost on top
        tcl_tclvl      :: TcLevel,         -- Birthplace for new unification variables

        tcl_th_ctxt    :: ThStage,         -- Template Haskell context
        tcl_th_bndrs   :: ThBindEnv,       -- and binder info
            -- The ThBindEnv records the TH binding level of in-scope Names
            -- defined in this module (not imported)
            -- We can't put this info in the TypeEnv because it's needed
            -- (and extended) in the renamer, for untyed splices

        tcl_arrow_ctxt :: ArrowCtxt,       -- Arrow-notation context

        tcl_rdr :: LocalRdrEnv,         -- Local name envt
                -- Maintained during renaming, of course, but also during
                -- type checking, solely so that when renaming a Template-Haskell
                -- splice we have the right environment for the renamer.
                --
                --   Does *not* include global name envt; may shadow it
                --   Includes both ordinary variables and type variables;
                --   they are kept distinct because tyvar have a different
                --   occurrence constructor (Name.TvOcc)
                -- We still need the unsullied global name env so that
                --   we can look up record field names

        tcl_env  :: TcTypeEnv,    -- The local type environment:
                                  -- Ids and TyVars defined in this module

        tcl_bndrs :: TcBinderStack,   -- Used for reporting relevant bindings,
                                      -- and for tidying types

        tcl_tyvars :: TcRef TcTyVarSet, -- The "global tyvars"
                        -- Namely, the in-scope TyVars bound in tcl_env,
                        -- plus the tyvars mentioned in the types of Ids bound
                        -- in tcl_lenv.
                        -- Why mutable? see notes with tcGetGlobalTyCoVars

        tcl_lie  :: TcRef WantedConstraints,    -- Place to accumulate type constraints
        tcl_errs :: TcRef Messages              -- Place to accumulate errors
    }

type ErrCtxt = (Bool, TidyEnv -> TcM (TidyEnv, MsgDoc))
        -- Monadic so that we have a chance
        -- to deal with bound type variables just before error
        -- message construction

        -- Bool:  True <=> this is a landmark context; do not
        --                 discard it when trimming for display

type TcTypeEnv = NameEnv TcTyThing

type ThBindEnv = NameEnv (TopLevelFlag, ThLevel)
   -- Domain = all Ids bound in this module (ie not imported)
   -- The TopLevelFlag tells if the binding is syntactically top level.
   -- We need to know this, because the cross-stage persistence story allows
   -- cross-stage at arbitrary types if the Id is bound at top level.
   --
   -- Nota bene: a ThLevel of 'outerLevel' is *not* the same as being
   -- bound at top level!  See Note [Template Haskell levels] in TcSplice

{- Note [Given Insts]
   ~~~~~~~~~~~~~~~~~~
Because of GADTs, we have to pass inwards the Insts provided by type signatures
and existential contexts. Consider
        data T a where { T1 :: b -> b -> T [b] }
        f :: Eq a => T a -> Bool
        f (T1 x y) = [x]==[y]

The constructor T1 binds an existential variable 'b', and we need Eq [b].
Well, we have it, because Eq a refines to Eq [b], but we can only spot that if we
pass it inwards.

-}

-- | Type alias for 'IORef'; the convention is we'll use this for mutable
-- bits of data in 'TcGblEnv' which are updated during typechecking and
-- returned at the end.
type TcRef a     = IORef a
-- ToDo: when should I refer to it as a 'TcId' instead of an 'Id'?
type TcId        = Id
type TcIdSet     = IdSet

---------------------------
-- The TcBinderStack
---------------------------

type TcBinderStack = [TcBinder]
   -- This is a stack of locally-bound ids and tyvars,
   --   innermost on top
   -- Used only in error reporting (relevantBindings in TcError),
   --   and in tidying
   -- We can't use the tcl_env type environment, because it doesn't
   --   keep track of the nesting order

data TcBinder
  = TcIdBndr
       TcId
       TopLevelFlag    -- Tells whether the binding is syntactically top-level
                       -- (The monomorphic Ids for a recursive group count
                       --  as not-top-level for this purpose.)

  | TcIdBndr_ExpType  -- Variant that allows the type to be specified as
                      -- an ExpType
       Name
       ExpType
       TopLevelFlag

  | TcTvBndr          -- e.g.   case x of P (y::a) -> blah
       Name           -- We bind the lexical name "a" to the type of y,
       TyVar          -- which might be an utterly different (perhaps
                      -- existential) tyvar

instance Outputable TcBinder where
   ppr (TcIdBndr id top_lvl)           = ppr id <> brackets (ppr top_lvl)
   ppr (TcIdBndr_ExpType id _ top_lvl) = ppr id <> brackets (ppr top_lvl)
   ppr (TcTvBndr name tv)              = ppr name <+> ppr tv

instance HasOccName TcBinder where
    occName (TcIdBndr id _)             = occName (idName id)
    occName (TcIdBndr_ExpType name _ _) = occName name
    occName (TcTvBndr name _)           = occName name

-- fixes #12177
-- Builds up a list of bindings whose OccName has not been seen before
-- i.e., If    ys  = removeBindingShadowing xs
-- then
--  - ys is obtained from xs by deleting some elements
--  - ys has no duplicate OccNames
--  - The first duplicated OccName in xs is retained in ys
-- Overloaded so that it can be used for both GlobalRdrElt in typed-hole
-- substitutions and TcBinder when looking for relevant bindings.
removeBindingShadowing :: HasOccName a => [a] -> [a]
removeBindingShadowing bindings = reverse $ fst $ foldl
    (\(bindingAcc, seenNames) binding ->
    if occName binding `elemOccSet` seenNames -- if we've seen it
        then (bindingAcc, seenNames)              -- skip it
        else (binding:bindingAcc, extendOccSet seenNames (occName binding)))
    ([], emptyOccSet) bindings

---------------------------
-- Template Haskell stages and levels
---------------------------

data SpliceType = Typed | Untyped

data ThStage    -- See Note [Template Haskell state diagram] in TcSplice
  = Splice SpliceType -- Inside a top-level splice
                      -- This code will be run *at compile time*;
                      --   the result replaces the splice
                      -- Binding level = 0

  | RunSplice (TcRef [ForeignRef (TH.Q ())])
      -- Set when running a splice, i.e. NOT when renaming or typechecking the
      -- Haskell code for the splice. See Note [RunSplice ThLevel].
      --
      -- Contains a list of mod finalizers collected while executing the splice.
      --
      -- 'addModFinalizer' inserts finalizers here, and from here they are taken
      -- to construct an @HsSpliced@ annotation for untyped splices. See Note
      -- [Delaying modFinalizers in untyped splices] in "RnSplice".
      --
      -- For typed splices, the typechecker takes finalizers from here and
      -- inserts them in the list of finalizers in the global environment.
      --
      -- See Note [Collecting modFinalizers in typed splices] in "TcSplice".

  | Comp        -- Ordinary Haskell code
                -- Binding level = 1

  | Brack                       -- Inside brackets
      ThStage                   --   Enclosing stage
      PendingStuff

data PendingStuff
  = RnPendingUntyped              -- Renaming the inside of an *untyped* bracket
      (TcRef [PendingRnSplice])   -- Pending splices in here

  | RnPendingTyped                -- Renaming the inside of a *typed* bracket

  | TcPending                     -- Typechecking the inside of a typed bracket
      (TcRef [PendingTcSplice])   --   Accumulate pending splices here
      (TcRef WantedConstraints)   --     and type constraints here

topStage, topAnnStage, topSpliceStage :: ThStage
topStage       = Comp
topAnnStage    = Splice Untyped
topSpliceStage = Splice Untyped

instance Outputable ThStage where
   ppr (Splice _)    = text "Splice"
   ppr (RunSplice _) = text "RunSplice"
   ppr Comp          = text "Comp"
   ppr (Brack s _)   = text "Brack" <> parens (ppr s)

type ThLevel = Int
    -- NB: see Note [Template Haskell levels] in TcSplice
    -- Incremented when going inside a bracket,
    -- decremented when going inside a splice
    -- NB: ThLevel is one greater than the 'n' in Fig 2 of the
    --     original "Template meta-programming for Haskell" paper

impLevel, outerLevel :: ThLevel
impLevel = 0    -- Imported things; they can be used inside a top level splice
outerLevel = 1  -- Things defined outside brackets

thLevel :: ThStage -> ThLevel
thLevel (Splice _)    = 0
thLevel (RunSplice _) =
    -- See Note [RunSplice ThLevel].
    panic "thLevel: called when running a splice"
thLevel Comp          = 1
thLevel (Brack s _)   = thLevel s + 1

{- Node [RunSplice ThLevel]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'RunSplice' stage is set when executing a splice, and only when running a
splice. In particular it is not set when the splice is renamed or typechecked.

'RunSplice' is needed to provide a reference where 'addModFinalizer' can insert
the finalizer (see Note [Delaying modFinalizers in untyped splices]), and
'addModFinalizer' runs when doing Q things. Therefore, It doesn't make sense to
set 'RunSplice' when renaming or typechecking the splice, where 'Splice', 
'Brack' or 'Comp' are used instead.

-}

---------------------------
-- Arrow-notation context
---------------------------

{- Note [Escaping the arrow scope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In arrow notation, a variable bound by a proc (or enclosed let/kappa)
is not in scope to the left of an arrow tail (-<) or the head of (|..|).
For example

        proc x -> (e1 -< e2)

Here, x is not in scope in e1, but it is in scope in e2.  This can get
a bit complicated:

        let x = 3 in
        proc y -> (proc z -> e1) -< e2

Here, x and z are in scope in e1, but y is not.

We implement this by
recording the environment when passing a proc (using newArrowScope),
and returning to that (using escapeArrowScope) on the left of -< and the
head of (|..|).

All this can be dealt with by the *renamer*. But the type checker needs
to be involved too.  Example (arrowfail001)
  class Foo a where foo :: a -> ()
  data Bar = forall a. Foo a => Bar a
  get :: Bar -> ()
  get = proc x -> case x of Bar a -> foo -< a
Here the call of 'foo' gives rise to a (Foo a) constraint that should not
be captured by the pattern match on 'Bar'.  Rather it should join the
constraints from further out.  So we must capture the constraint bag
from further out in the ArrowCtxt that we push inwards.
-}

data ArrowCtxt   -- Note [Escaping the arrow scope]
  = NoArrowCtxt
  | ArrowCtxt LocalRdrEnv (TcRef WantedConstraints)


---------------------------
-- TcTyThing
---------------------------

-- | A typecheckable thing available in a local context.  Could be
-- 'AGlobal' 'TyThing', but also lexically scoped variables, etc.
-- See 'TcEnv' for how to retrieve a 'TyThing' given a 'Name'.
data TcTyThing
  = AGlobal TyThing             -- Used only in the return type of a lookup

  | ATcId           -- Ids defined in this module; may not be fully zonked
      { tct_id   :: TcId
      , tct_info :: IdBindingInfo   -- See Note [Meaning of IdBindingInfo]
      }

  | ATyVar  Name TcTyVar   -- See Note [Type variables in the type environment]

  | ATcTyCon TyCon   -- Used temporarily, during kind checking, for the
                     -- tycons and clases in this recursive group
                     -- The TyCon is always a TcTyCon.  Its kind
                     -- can be a mono-kind or a poly-kind; in TcTyClsDcls see
                     -- Note [Type checking recursive type and class declarations]

  | APromotionErr PromotionErr

data PromotionErr
  = TyConPE          -- TyCon used in a kind before we are ready
                     --     data T :: T -> * where ...
  | ClassPE          -- Ditto Class

  | FamDataConPE     -- Data constructor for a data family
                     -- See Note [AFamDataCon: not promoting data family constructors]
                     -- in TcEnv.
  | ConstrainedDataConPE PredType
                     -- Data constructor with a non-equality context
                     -- See Note [Don't promote data constructors with
                     --           non-equality contexts] in TcHsType
  | PatSynPE         -- Pattern synonyms
                     -- See Note [Don't promote pattern synonyms] in TcEnv

  | PatSynExPE       -- Pattern synonym existential type variable
                     -- See Note [Pattern synonym existentials do not scope] in TcPatSyn

  | RecDataConPE     -- Data constructor in a recursive loop
                     -- See Note [Recursion and promoting data constructors] in TcTyClsDecls
  | NoDataKindsTC    -- -XDataKinds not enabled (for a tycon)
  | NoDataKindsDC    -- -XDataKinds not enabled (for a datacon)

instance Outputable TcTyThing where     -- Debugging only
   ppr (AGlobal g)      = ppr g
   ppr elt@(ATcId {})   = text "Identifier" <>
                          brackets (ppr (tct_id elt) <> dcolon
                                 <> ppr (varType (tct_id elt)) <> comma
                                 <+> ppr (tct_info elt))
   ppr (ATyVar n tv)    = text "Type variable" <+> quotes (ppr n) <+> equals <+> ppr tv
                            <+> dcolon <+> ppr (varType tv)
   ppr (ATcTyCon tc)    = text "ATcTyCon" <+> ppr tc <+> dcolon <+> ppr (tyConKind tc)
   ppr (APromotionErr err) = text "APromotionErr" <+> ppr err

-- | IdBindingInfo describes how an Id is bound.
--
-- It is used for the following purposes:
-- a) for static forms in TcExpr.checkClosedInStaticForm and
-- b) to figure out when a nested binding can be generalised,
--    in TcBinds.decideGeneralisationPlan.
--
data IdBindingInfo -- See Note [Meaning of IdBindingInfo and ClosedTypeId]
    = NotLetBound
    | ClosedLet
    | NonClosedLet
         RhsNames        -- Used for (static e) checks only
         ClosedTypeId    -- Used for generalisation checks
                         -- and for (static e) checks

-- | IsGroupClosed describes a group of mutually-recursive bindings
data IsGroupClosed
  = IsGroupClosed
      (NameEnv RhsNames)  -- Free var info for the RHS of each binding in the goup
                          -- Used only for (static e) checks

      ClosedTypeId        -- True <=> all the free vars of the group are
                          --          imported or ClosedLet or
                          --          NonClosedLet with ClosedTypeId=True.
                          --          In particular, no tyvars, no NotLetBound

type RhsNames = NameSet   -- Names of variables, mentioned on the RHS of
                          -- a definition, that are not Global or ClosedLet

type ClosedTypeId = Bool
  -- See Note [Meaning of IdBindingInfo and ClosedTypeId]

{- Note [Meaning of IdBindingInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NotLetBound means that
  the Id is not let-bound (e.g. it is bound in a
  lambda-abstraction or in a case pattern)

ClosedLet means that
   - The Id is let-bound,
   - Any free term variables are also Global or ClosedLet
   - Its type has no free variables (NB: a top-level binding subject
     to the MR might have free vars in its type)
   These ClosedLets can definitely be floated to top level; and we
   may need to do so for static forms.

   Property:   ClosedLet
             is equivalent to
               NonClosedLet emptyNameSet True

(NonClosedLet (fvs::RhsNames) (cl::ClosedTypeId)) means that
   - The Id is let-bound

   - The fvs::RhsNames contains the free names of the RHS,
     excluding Global and ClosedLet ones.

   - For the ClosedTypeId field see Note [Bindings with closed types]

For (static e) to be valid, we need for every 'x' free in 'e',
that x's binding is floatable to the top level.  Specifically:
   * x's RhsNames must be empty
   * x's type has no free variables
See Note [Grand plan for static forms] in StaticPtrTable.hs.
This test is made in TcExpr.checkClosedInStaticForm.
Actually knowing x's RhsNames (rather than just its emptiness
or otherwise) is just so we can produce better error messages

Note [Bindings with closed types: ClosedTypeId]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  f x = let g ys = map not ys
        in ...

Can we generalise 'g' under the OutsideIn algorithm?  Yes,
because all g's free variables are top-level; that is they themselves
have no free type variables, and it is the type variables in the
environment that makes things tricky for OutsideIn generalisation.

Here's the invariant:
   If an Id has ClosedTypeId=True (in its IdBindingInfo), then
   the Id's type is /definitely/ closed (has no free type variables).
   Specifically,
       a) The Id's acutal type is closed (has no free tyvars)
       b) Either the Id has a (closed) user-supplied type signature
          or all its free variables are Global/ClosedLet
             or NonClosedLet with ClosedTypeId=True.
          In particular, none are NotLetBound.

Why is (b) needed?   Consider
    \x. (x :: Int, let y = x+1 in ...)
Initially x::alpha.  If we happen to typecheck the 'let' before the
(x::Int), y's type will have a free tyvar; but if the other way round
it won't.  So we treat any let-bound variable with a free
non-let-bound variable as not ClosedTypeId, regardless of what the
free vars of its type actually are.

But if it has a signature, all is well:
   \x. ...(let { y::Int; y = x+1 } in
           let { v = y+2 } in ...)...
Here the signature on 'v' makes 'y' a ClosedTypeId, so we can
generalise 'v'.

Note that:

  * A top-level binding may not have ClosedTypeId=True, if it suffers
    from the MR

  * A nested binding may be closed (eg 'g' in the example we started
    with). Indeed, that's the point; whether a function is defined at
    top level or nested is orthogonal to the question of whether or
    not it is closed.

  * A binding may be non-closed because it mentions a lexically scoped
    *type variable*  Eg
        f :: forall a. blah
        f x = let g y = ...(y::a)...

Under OutsideIn we are free to generalise an Id all of whose free
variables have ClosedTypeId=True (or imported).  This is an extension
compared to the JFP paper on OutsideIn, which used "top-level" as a
proxy for "closed".  (It's not a good proxy anyway -- the MR can make
a top-level binding with a free type variable.)

Note [Type variables in the type environment]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type environment has a binding for each lexically-scoped
type variable that is in scope.  For example

  f :: forall a. a -> a
  f x = (x :: a)

  g1 :: [a] -> a
  g1 (ys :: [b]) = head ys :: b

  g2 :: [Int] -> Int
  g2 (ys :: [c]) = head ys :: c

* The forall'd variable 'a' in the signature scopes over f's RHS.

* The pattern-bound type variable 'b' in 'g1' scopes over g1's
  RHS; note that it is bound to a skolem 'a' which is not itself
  lexically in scope.

* The pattern-bound type variable 'c' in 'g2' is bound to
  Int; that is, pattern-bound type variables can stand for
  arbitrary types. (see
    GHC proposal #128 "Allow ScopedTypeVariables to refer to types"
    https://github.com/ghc-proposals/ghc-proposals/pull/128,
  and the paper
    "Type variables in patterns", Haskell Symposium 2018.


This is implemented by the constructor
   ATyVar Name TcTyVar
in the type environment.

* The Name is the name of the original, lexically scoped type
  variable

* The TcTyVar is sometimes a skolem (like in 'f'), and sometimes
  a unification variable (like in 'g1', 'g2').  We never zonk the
  type environment so in the latter case it always stays as a
  unification variable, although that variable may be later
  unified with a type (such as Int in 'g2').
-}

instance Outputable IdBindingInfo where
  ppr NotLetBound = text "NotLetBound"
  ppr ClosedLet = text "TopLevelLet"
  ppr (NonClosedLet fvs closed_type) =
    text "TopLevelLet" <+> ppr fvs <+> ppr closed_type

instance Outputable PromotionErr where
  ppr ClassPE                     = text "ClassPE"
  ppr TyConPE                     = text "TyConPE"
  ppr PatSynPE                    = text "PatSynPE"
  ppr PatSynExPE                  = text "PatSynExPE"
  ppr FamDataConPE                = text "FamDataConPE"
  ppr (ConstrainedDataConPE pred) = text "ConstrainedDataConPE"
                                      <+> parens (ppr pred)
  ppr RecDataConPE                = text "RecDataConPE"
  ppr NoDataKindsTC               = text "NoDataKindsTC"
  ppr NoDataKindsDC               = text "NoDataKindsDC"

pprTcTyThingCategory :: TcTyThing -> SDoc
pprTcTyThingCategory (AGlobal thing)    = pprTyThingCategory thing
pprTcTyThingCategory (ATyVar {})        = text "Type variable"
pprTcTyThingCategory (ATcId {})         = text "Local identifier"
pprTcTyThingCategory (ATcTyCon {})     = text "Local tycon"
pprTcTyThingCategory (APromotionErr pe) = pprPECategory pe

pprPECategory :: PromotionErr -> SDoc
pprPECategory ClassPE                = text "Class"
pprPECategory TyConPE                = text "Type constructor"
pprPECategory PatSynPE               = text "Pattern synonym"
pprPECategory PatSynExPE             = text "Pattern synonym existential"
pprPECategory FamDataConPE           = text "Data constructor"
pprPECategory ConstrainedDataConPE{} = text "Data constructor"
pprPECategory RecDataConPE           = text "Data constructor"
pprPECategory NoDataKindsTC          = text "Type constructor"
pprPECategory NoDataKindsDC          = text "Data constructor"

{-
************************************************************************
*                                                                      *
        Operations over ImportAvails
*                                                                      *
************************************************************************
-}

-- | 'ImportAvails' summarises what was imported from where, irrespective of
-- whether the imported things are actually used or not.  It is used:
--
--  * when processing the export list,
--
--  * when constructing usage info for the interface file,
--
--  * to identify the list of directly imported modules for initialisation
--    purposes and for optimised overlap checking of family instances,
--
--  * when figuring out what things are really unused
--
data ImportAvails
   = ImportAvails {
        imp_mods :: ImportedMods,
          --      = ModuleEnv [ImportedModsVal],
          -- ^ Domain is all directly-imported modules
          --
          -- See the documentation on ImportedModsVal in HscTypes for the
          -- meaning of the fields.
          --
          -- We need a full ModuleEnv rather than a ModuleNameEnv here,
          -- because we might be importing modules of the same name from
          -- different packages. (currently not the case, but might be in the
          -- future).

        imp_dep_mods :: ModuleNameEnv (ModuleName, IsBootInterface),
          -- ^ Home-package modules needed by the module being compiled
          --
          -- It doesn't matter whether any of these dependencies
          -- are actually /used/ when compiling the module; they
          -- are listed if they are below it at all.  For
          -- example, suppose M imports A which imports X.  Then
          -- compiling M might not need to consult X.hi, but X
          -- is still listed in M's dependencies.

        imp_dep_pkgs :: Set InstalledUnitId,
          -- ^ Packages needed by the module being compiled, whether directly,
          -- or via other modules in this package, or via modules imported
          -- from other packages.

        imp_trust_pkgs :: Set InstalledUnitId,
          -- ^ This is strictly a subset of imp_dep_pkgs and records the
          -- packages the current module needs to trust for Safe Haskell
          -- compilation to succeed. A package is required to be trusted if
          -- we are dependent on a trustworthy module in that package.
          -- While perhaps making imp_dep_pkgs a tuple of (UnitId, Bool)
          -- where True for the bool indicates the package is required to be
          -- trusted is the more logical  design, doing so complicates a lot
          -- of code not concerned with Safe Haskell.
          -- See Note [RnNames . Tracking Trust Transitively]

        imp_trust_own_pkg :: Bool,
          -- ^ Do we require that our own package is trusted?
          -- This is to handle efficiently the case where a Safe module imports
          -- a Trustworthy module that resides in the same package as it.
          -- See Note [RnNames . Trust Own Package]

        imp_orphs :: [Module],
          -- ^ Orphan modules below us in the import tree (and maybe including
          -- us for imported modules)

        imp_finsts :: [Module]
          -- ^ Family instance modules below us in the import tree (and maybe
          -- including us for imported modules)
      }

mkModDeps :: [(ModuleName, IsBootInterface)]
          -> ModuleNameEnv (ModuleName, IsBootInterface)
mkModDeps deps = foldl' add emptyUFM deps
               where
                 add env elt@(m,_) = addToUFM env m elt

modDepsElts
  :: ModuleNameEnv (ModuleName, IsBootInterface)
  -> [(ModuleName, IsBootInterface)]
modDepsElts = sort . nonDetEltsUFM
  -- It's OK to use nonDetEltsUFM here because sorting by module names
  -- restores determinism

emptyImportAvails :: ImportAvails
emptyImportAvails = ImportAvails { imp_mods          = emptyModuleEnv,
                                   imp_dep_mods      = emptyUFM,
                                   imp_dep_pkgs      = S.empty,
                                   imp_trust_pkgs    = S.empty,
                                   imp_trust_own_pkg = False,
                                   imp_orphs         = [],
                                   imp_finsts        = [] }

-- | Union two ImportAvails
--
-- This function is a key part of Import handling, basically
-- for each import we create a separate ImportAvails structure
-- and then union them all together with this function.
plusImportAvails ::  ImportAvails ->  ImportAvails ->  ImportAvails
plusImportAvails
  (ImportAvails { imp_mods = mods1,
                  imp_dep_mods = dmods1, imp_dep_pkgs = dpkgs1,
                  imp_trust_pkgs = tpkgs1, imp_trust_own_pkg = tself1,
                  imp_orphs = orphs1, imp_finsts = finsts1 })
  (ImportAvails { imp_mods = mods2,
                  imp_dep_mods = dmods2, imp_dep_pkgs = dpkgs2,
                  imp_trust_pkgs = tpkgs2, imp_trust_own_pkg = tself2,
                  imp_orphs = orphs2, imp_finsts = finsts2 })
  = ImportAvails { imp_mods          = plusModuleEnv_C (++) mods1 mods2,
                   imp_dep_mods      = plusUFM_C plus_mod_dep dmods1 dmods2,
                   imp_dep_pkgs      = dpkgs1 `S.union` dpkgs2,
                   imp_trust_pkgs    = tpkgs1 `S.union` tpkgs2,
                   imp_trust_own_pkg = tself1 || tself2,
                   imp_orphs         = orphs1 `unionLists` orphs2,
                   imp_finsts        = finsts1 `unionLists` finsts2 }
  where
    plus_mod_dep r1@(m1, boot1) r2@(m2, boot2)
      | ASSERT2( m1 == m2, (ppr m1 <+> ppr m2) $$ (ppr boot1 <+> ppr boot2) )
        boot1 = r2
      | otherwise = r1
      -- If either side can "see" a non-hi-boot interface, use that
      -- Reusing existing tuples saves 10% of allocations on test
      -- perf/compiler/MultiLayerModules

{-
************************************************************************
*                                                                      *
\subsection{Where from}
*                                                                      *
************************************************************************

The @WhereFrom@ type controls where the renamer looks for an interface file
-}

data WhereFrom
  = ImportByUser IsBootInterface        -- Ordinary user import (perhaps {-# SOURCE #-})
  | ImportBySystem                      -- Non user import.
  | ImportByPlugin                      -- Importing a plugin;
                                        -- See Note [Care with plugin imports] in LoadIface

instance Outputable WhereFrom where
  ppr (ImportByUser is_boot) | is_boot     = text "{- SOURCE -}"
                             | otherwise   = empty
  ppr ImportBySystem                       = text "{- SYSTEM -}"
  ppr ImportByPlugin                       = text "{- PLUGIN -}"


{- *********************************************************************
*                                                                      *
                Type signatures
*                                                                      *
********************************************************************* -}

-- These data types need to be here only because
-- TcSimplify uses them, and TcSimplify is fairly
-- low down in the module hierarchy

type TcSigFun  = Name -> Maybe TcSigInfo

data TcSigInfo = TcIdSig     TcIdSigInfo
               | TcPatSynSig TcPatSynInfo

data TcIdSigInfo   -- See Note [Complete and partial type signatures]
  = CompleteSig    -- A complete signature with no wildcards,
                   -- so the complete polymorphic type is known.
      { sig_bndr :: TcId          -- The polymorphic Id with that type

      , sig_ctxt :: UserTypeCtxt  -- In the case of type-class default methods,
                                  -- the Name in the FunSigCtxt is not the same
                                  -- as the TcId; the former is 'op', while the
                                  -- latter is '$dmop' or some such

      , sig_loc  :: SrcSpan       -- Location of the type signature
      }

  | PartialSig     -- A partial type signature (i.e. includes one or more
                   -- wildcards). In this case it doesn't make sense to give
                   -- the polymorphic Id, because we are going to /infer/ its
                   -- type, so we can't make the polymorphic Id ab-initio
      { psig_name  :: Name   -- Name of the function; used when report wildcards
      , psig_hs_ty :: LHsSigWcType GhcRn  -- The original partial signature in
                                          -- HsSyn form
      , sig_ctxt   :: UserTypeCtxt
      , sig_loc    :: SrcSpan            -- Location of the type signature
      }


{- Note [Complete and partial type signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A type signature is partial when it contains one or more wildcards
(= type holes).  The wildcard can either be:
* A (type) wildcard occurring in sig_theta or sig_tau. These are
  stored in sig_wcs.
      f :: Bool -> _
      g :: Eq _a => _a -> _a -> Bool
* Or an extra-constraints wildcard, stored in sig_cts:
      h :: (Num a, _) => a -> a

A type signature is a complete type signature when there are no
wildcards in the type signature, i.e. iff sig_wcs is empty and
sig_extra_cts is Nothing.
-}

data TcIdSigInst
  = TISI { sig_inst_sig :: TcIdSigInfo

         , sig_inst_skols :: [(Name, TcTyVar)]
               -- Instantiated type and kind variables, TyVarTvs
               -- The Name is the Name that the renamer chose;
               --   but the TcTyVar may come from instantiating
               --   the type and hence have a different unique.
               -- No need to keep track of whether they are truly lexically
               --   scoped because the renamer has named them uniquely
               -- See Note [Binding scoped type variables] in TcSigs

         , sig_inst_theta  :: TcThetaType
               -- Instantiated theta.  In the case of a
               -- PartialSig, sig_theta does not include
               -- the extra-constraints wildcard

         , sig_inst_tau :: TcSigmaType   -- Instantiated tau
               -- See Note [sig_inst_tau may be polymorphic]

         -- Relevant for partial signature only
         , sig_inst_wcs   :: [(Name, TcTyVar)]
               -- Like sig_inst_skols, but for wildcards.  The named
               -- wildcards scope over the binding, and hence their
               -- Names may appear in type signatures in the binding

         , sig_inst_wcx   :: Maybe TcType
               -- Extra-constraints wildcard to fill in, if any
               -- If this exists, it is surely of the form (meta_tv |> co)
               -- (where the co might be reflexive). This is filled in
               -- only from the return value of TcHsType.tcWildCardOcc
         }

{- Note [sig_inst_tau may be polymorphic]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Note that "sig_inst_tau" might actually be a polymorphic type,
if the original function had a signature like
   forall a. Eq a => forall b. Ord b => ....
But that's ok: tcMatchesFun (called by tcRhs) can deal with that
It happens, too!  See Note [Polymorphic methods] in TcClassDcl.

Note [Wildcards in partial signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wildcards in psig_wcs may stand for a type mentioning
the universally-quantified tyvars of psig_ty

E.g.  f :: forall a. _ -> a
      f x = x
We get sig_inst_skols = [a]
       sig_inst_tau   = _22 -> a
       sig_inst_wcs   = [_22]
and _22 in the end is unified with the type 'a'

Moreover the kind of a wildcard in sig_inst_wcs may mention
the universally-quantified tyvars sig_inst_skols
e.g.   f :: t a -> t _
Here we get
   sig_inst_skols = [k:*, (t::k ->*), (a::k)]
   sig_inst_tau   = t a -> t _22
   sig_inst_wcs   = [ _22::k ]
-}

data TcPatSynInfo
  = TPSI {
        patsig_name           :: Name,
        patsig_implicit_bndrs :: [TyVarBinder], -- Implicitly-bound kind vars (Inferred) and
                                                -- implicitly-bound type vars (Specified)
          -- See Note [The pattern-synonym signature splitting rule] in TcPatSyn
        patsig_univ_bndrs     :: [TyVar],       -- Bound by explicit user forall
        patsig_req            :: TcThetaType,
        patsig_ex_bndrs       :: [TyVar],       -- Bound by explicit user forall
        patsig_prov           :: TcThetaType,
        patsig_body_ty        :: TcSigmaType
    }

instance Outputable TcSigInfo where
  ppr (TcIdSig     idsi) = ppr idsi
  ppr (TcPatSynSig tpsi) = text "TcPatSynInfo" <+> ppr tpsi

instance Outputable TcIdSigInfo where
    ppr (CompleteSig { sig_bndr = bndr })
        = ppr bndr <+> dcolon <+> ppr (idType bndr)
    ppr (PartialSig { psig_name = name, psig_hs_ty = hs_ty })
        = text "psig" <+> ppr name <+> dcolon <+> ppr hs_ty

instance Outputable TcIdSigInst where
    ppr (TISI { sig_inst_sig = sig, sig_inst_skols = skols
              , sig_inst_theta = theta, sig_inst_tau = tau })
        = hang (ppr sig) 2 (vcat [ ppr skols, ppr theta <+> darrow <+> ppr tau ])

instance Outputable TcPatSynInfo where
    ppr (TPSI{ patsig_name = name}) = ppr name

isPartialSig :: TcIdSigInst -> Bool
isPartialSig (TISI { sig_inst_sig = PartialSig {} }) = True
isPartialSig _                                       = False

-- | No signature or a partial signature
hasCompleteSig :: TcSigFun -> Name -> Bool
hasCompleteSig sig_fn name
  = case sig_fn name of
      Just (TcIdSig (CompleteSig {})) -> True
      _                               -> False


{-
************************************************************************
*                                                                      *
*                       Canonical constraints                          *
*                                                                      *
*   These are the constraints the low-level simplifier works with      *
*                                                                      *
************************************************************************
-}

-- The syntax of xi (ξ) types:
-- xi ::= a | T xis | xis -> xis | ... | forall a. tau
-- Two important notes:
--      (i) No type families, unless we are under a ForAll
--      (ii) Note that xi types can contain unexpanded type synonyms;
--           however, the (transitive) expansions of those type synonyms
--           will not contain any type functions, unless we are under a ForAll.
-- We enforce the structure of Xi types when we flatten (TcCanonical)

type Xi = Type       -- In many comments, "xi" ranges over Xi

type Cts = Bag Ct

data Ct
  -- Atomic canonical constraints
  = CDictCan {  -- e.g.  Num xi
      cc_ev     :: CtEvidence, -- See Note [Ct/evidence invariant]

      cc_class  :: Class,
      cc_tyargs :: [Xi],   -- cc_tyargs are function-free, hence Xi

      cc_pend_sc :: Bool   -- See Note [The superclass story] in TcCanonical
                           -- True <=> (a) cc_class has superclasses
                           --          (b) we have not (yet) added those
                           --              superclasses as Givens
    }

  | CIrredCan {  -- These stand for yet-unusable predicates
      cc_ev    :: CtEvidence,   -- See Note [Ct/evidence invariant]
      cc_insol :: Bool   -- True  <=> definitely an error, can never be solved
                         -- False <=> might be soluble

        -- For the might-be-soluble case, the ctev_pred of the evidence is
        -- of form   (tv xi1 xi2 ... xin)   with a tyvar at the head
        --      or   (tv1 ~ ty2)   where the CTyEqCan  kind invariant fails
        --      or   (F tys ~ ty)  where the CFunEqCan kind invariant fails
        -- See Note [CIrredCan constraints]

        -- The definitely-insoluble case is for things like
        --    Int ~ Bool      tycons don't match
        --    a ~ [a]         occurs check
    }

  | CTyEqCan {  -- tv ~ rhs
       -- Invariants:
       --   * See Note [Applying the inert substitution] in TcFlatten
       --   * tv not in tvs(rhs)   (occurs check)
       --   * If tv is a TauTv, then rhs has no foralls
       --       (this avoids substituting a forall for the tyvar in other types)
       --   * tcTypeKind ty `tcEqKind` tcTypeKind tv; Note [Ct kind invariant]
       --   * rhs may have at most one top-level cast
       --   * rhs (perhaps under the one cast) is not necessarily function-free,
       --       but it has no top-level function.
       --     E.g. a ~ [F b]  is fine
       --     but  a ~ F b    is not
       --   * If the equality is representational, rhs has no top-level newtype
       --     See Note [No top-level newtypes on RHS of representational
       --     equalities] in TcCanonical
       --   * If rhs (perhaps under the cast) is also a tv, then it is oriented
       --     to give best chance of
       --     unification happening; eg if rhs is touchable then lhs is too
      cc_ev     :: CtEvidence, -- See Note [Ct/evidence invariant]
      cc_tyvar  :: TcTyVar,
      cc_rhs    :: TcType,     -- Not necessarily function-free (hence not Xi)
                               -- See invariants above

      cc_eq_rel :: EqRel       -- INVARIANT: cc_eq_rel = ctEvEqRel cc_ev
    }

  | CFunEqCan {  -- F xis ~ fsk
       -- Invariants:
       --   * isTypeFamilyTyCon cc_fun
       --   * tcTypeKind (F xis) = tyVarKind fsk; Note [Ct kind invariant]
       --   * always Nominal role
      cc_ev     :: CtEvidence,  -- See Note [Ct/evidence invariant]
      cc_fun    :: TyCon,       -- A type function

      cc_tyargs :: [Xi],        -- cc_tyargs are function-free (hence Xi)
        -- Either under-saturated or exactly saturated
        --    *never* over-saturated (because if so
        --    we should have decomposed)

      cc_fsk    :: TcTyVar  -- [G]  always a FlatSkolTv
                            -- [W], [WD], or [D] always a FlatMetaTv
        -- See Note [The flattening story] in TcFlatten
    }

  | CNonCanonical {        -- See Note [NonCanonical Semantics] in TcSMonad
      cc_ev  :: CtEvidence
    }

  | CHoleCan {             -- See Note [Hole constraints]
       -- Treated as an "insoluble" constraint
       -- See Note [Insoluble constraints]
      cc_ev   :: CtEvidence,
      cc_hole :: Hole
    }

  | CQuantCan QCInst       -- A quantified constraint
      -- NB: I expect to make more of the cases in Ct
      --     look like this, with the payload in an
      --     auxiliary type

------------
data QCInst  -- A much simplified version of ClsInst
             -- See Note [Quantified constraints] in TcCanonical
  = QCI { qci_ev   :: CtEvidence -- Always of type forall tvs. context => ty
                                 -- Always Given
        , qci_tvs  :: [TcTyVar]  -- The tvs
        , qci_pred :: TcPredType -- The ty
        , qci_pend_sc :: Bool    -- Same as cc_pend_sc flag in CDictCan
                                 -- Invariant: True => qci_pred is a ClassPred
    }

instance Outputable QCInst where
  ppr (QCI { qci_ev = ev }) = ppr ev

------------
-- | An expression or type hole
data Hole = ExprHole UnboundVar
            -- ^ Either an out-of-scope variable or a "true" hole in an
            -- expression (TypedHoles)
          | TypeHole OccName
            -- ^ A hole in a type (PartialTypeSignatures)

instance Outputable Hole where
  ppr (ExprHole ub)  = ppr ub
  ppr (TypeHole occ) = text "TypeHole" <> parens (ppr occ)

holeOcc :: Hole -> OccName
holeOcc (ExprHole uv)  = unboundVarOcc uv
holeOcc (TypeHole occ) = occ

{- Note [Hole constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~
CHoleCan constraints are used for two kinds of holes,
distinguished by cc_hole:

  * For holes in expressions (including variables not in scope)
    e.g.   f x = g _ x

  * For holes in type signatures
    e.g.   f :: _ -> _
           f x = [x,True]

Note [CIrredCan constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CIrredCan constraints are used for constraints that are "stuck"
   - we can't solve them (yet)
   - we can't use them to solve other constraints
   - but they may become soluble if we substitute for some
     of the type variables in the constraint

Example 1:  (c Int), where c :: * -> Constraint.  We can't do anything
            with this yet, but if later c := Num, *then* we can solve it

Example 2:  a ~ b, where a :: *, b :: k, where k is a kind variable
            We don't want to use this to substitute 'b' for 'a', in case
            'k' is subsequently unifed with (say) *->*, because then
            we'd have ill-kinded types floating about.  Rather we want
            to defer using the equality altogether until 'k' get resolved.

Note [Ct/evidence invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If  ct :: Ct, then extra fields of 'ct' cache precisely the ctev_pred field
of (cc_ev ct), and is fully rewritten wrt the substitution.   Eg for CDictCan,
   ctev_pred (cc_ev ct) = (cc_class ct) (cc_tyargs ct)
This holds by construction; look at the unique place where CDictCan is
built (in TcCanonical).

In contrast, the type of the evidence *term* (ctev_dest / ctev_evar) in
the evidence may *not* be fully zonked; we are careful not to look at it
during constraint solving. See Note [Evidence field of CtEvidence].

Note [Ct kind invariant]
~~~~~~~~~~~~~~~~~~~~~~~~
CTyEqCan and CFunEqCan both require that the kind of the lhs matches the kind
of the rhs. This is necessary because both constraints are used for substitutions
during solving. If the kinds differed, then the substitution would take a well-kinded
type to an ill-kinded one.

-}

mkNonCanonical :: CtEvidence -> Ct
mkNonCanonical ev = CNonCanonical { cc_ev = ev }

mkNonCanonicalCt :: Ct -> Ct
mkNonCanonicalCt ct = CNonCanonical { cc_ev = cc_ev ct }

mkIrredCt :: CtEvidence -> Ct
mkIrredCt ev = CIrredCan { cc_ev = ev, cc_insol = False }

mkInsolubleCt :: CtEvidence -> Ct
mkInsolubleCt ev = CIrredCan { cc_ev = ev, cc_insol = True }

mkGivens :: CtLoc -> [EvId] -> [Ct]
mkGivens loc ev_ids
  = map mk ev_ids
  where
    mk ev_id = mkNonCanonical (CtGiven { ctev_evar = ev_id
                                       , ctev_pred = evVarPred ev_id
                                       , ctev_loc = loc })

ctEvidence :: Ct -> CtEvidence
ctEvidence (CQuantCan (QCI { qci_ev = ev })) = ev
ctEvidence ct = cc_ev ct

ctLoc :: Ct -> CtLoc
ctLoc = ctEvLoc . ctEvidence

setCtLoc :: Ct -> CtLoc -> Ct
setCtLoc ct loc = ct { cc_ev = (cc_ev ct) { ctev_loc = loc } }

ctOrigin :: Ct -> CtOrigin
ctOrigin = ctLocOrigin . ctLoc

ctPred :: Ct -> PredType
-- See Note [Ct/evidence invariant]
ctPred ct = ctEvPred (ctEvidence ct)

ctEvId :: Ct -> EvVar
-- The evidence Id for this Ct
ctEvId ct = ctEvEvId (ctEvidence ct)

-- | Makes a new equality predicate with the same role as the given
-- evidence.
mkTcEqPredLikeEv :: CtEvidence -> TcType -> TcType -> TcType
mkTcEqPredLikeEv ev
  = case predTypeEqRel pred of
      NomEq  -> mkPrimEqPred
      ReprEq -> mkReprPrimEqPred
  where
    pred = ctEvPred ev

-- | Get the flavour of the given 'Ct'
ctFlavour :: Ct -> CtFlavour
ctFlavour = ctEvFlavour . ctEvidence

-- | Get the equality relation for the given 'Ct'
ctEqRel :: Ct -> EqRel
ctEqRel = ctEvEqRel . ctEvidence

instance Outputable Ct where
  ppr ct = ppr (ctEvidence ct) <+> parens pp_sort
    where
      pp_sort = case ct of
         CTyEqCan {}      -> text "CTyEqCan"
         CFunEqCan {}     -> text "CFunEqCan"
         CNonCanonical {} -> text "CNonCanonical"
         CDictCan { cc_pend_sc = pend_sc }
            | pend_sc   -> text "CDictCan(psc)"
            | otherwise -> text "CDictCan"
         CIrredCan { cc_insol = insol }
            | insol     -> text "CIrredCan(insol)"
            | otherwise -> text "CIrredCan(sol)"
         CHoleCan { cc_hole = hole } -> text "CHoleCan:" <+> ppr hole
         CQuantCan (QCI { qci_pend_sc = pend_sc })
            | pend_sc   -> text "CQuantCan(psc)"
            | otherwise -> text "CQuantCan"

{-
************************************************************************
*                                                                      *
        Simple functions over evidence variables
*                                                                      *
************************************************************************
-}

---------------- Getting free tyvars -------------------------

-- | Returns free variables of constraints as a non-deterministic set
tyCoVarsOfCt :: Ct -> TcTyCoVarSet
tyCoVarsOfCt = fvVarSet . tyCoFVsOfCt

-- | Returns free variables of constraints as a deterministically ordered.
-- list. See Note [Deterministic FV] in FV.
tyCoVarsOfCtList :: Ct -> [TcTyCoVar]
tyCoVarsOfCtList = fvVarList . tyCoFVsOfCt

-- | Returns free variables of constraints as a composable FV computation.
-- See Note [Deterministic FV] in FV.
tyCoFVsOfCt :: Ct -> FV
tyCoFVsOfCt (CTyEqCan { cc_tyvar = tv, cc_rhs = xi })
  = tyCoFVsOfType xi `unionFV` FV.unitFV tv
                     `unionFV` tyCoFVsOfType (tyVarKind tv)
tyCoFVsOfCt (CFunEqCan { cc_tyargs = tys, cc_fsk = fsk })
  = tyCoFVsOfTypes tys `unionFV` FV.unitFV fsk
                       `unionFV` tyCoFVsOfType (tyVarKind fsk)
tyCoFVsOfCt (CDictCan { cc_tyargs = tys }) = tyCoFVsOfTypes tys
tyCoFVsOfCt ct = tyCoFVsOfType (ctPred ct)

-- | Returns free variables of a bag of constraints as a non-deterministic
-- set. See Note [Deterministic FV] in FV.
tyCoVarsOfCts :: Cts -> TcTyCoVarSet
tyCoVarsOfCts = fvVarSet . tyCoFVsOfCts

-- | Returns free variables of a bag of constraints as a deterministically
-- odered list. See Note [Deterministic FV] in FV.
tyCoVarsOfCtsList :: Cts -> [TcTyCoVar]
tyCoVarsOfCtsList = fvVarList . tyCoFVsOfCts

-- | Returns free variables of a bag of constraints as a composable FV
-- computation. See Note [Deterministic FV] in FV.
tyCoFVsOfCts :: Cts -> FV
tyCoFVsOfCts = foldrBag (unionFV . tyCoFVsOfCt) emptyFV

-- | Returns free variables of WantedConstraints as a non-deterministic
-- set. See Note [Deterministic FV] in FV.
tyCoVarsOfWC :: WantedConstraints -> TyCoVarSet
-- Only called on *zonked* things, hence no need to worry about flatten-skolems
tyCoVarsOfWC = fvVarSet . tyCoFVsOfWC

-- | Returns free variables of WantedConstraints as a deterministically
-- ordered list. See Note [Deterministic FV] in FV.
tyCoVarsOfWCList :: WantedConstraints -> [TyCoVar]
-- Only called on *zonked* things, hence no need to worry about flatten-skolems
tyCoVarsOfWCList = fvVarList . tyCoFVsOfWC

-- | Returns free variables of WantedConstraints as a composable FV
-- computation. See Note [Deterministic FV] in FV.
tyCoFVsOfWC :: WantedConstraints -> FV
-- Only called on *zonked* things, hence no need to worry about flatten-skolems
tyCoFVsOfWC (WC { wc_simple = simple, wc_impl = implic })
  = tyCoFVsOfCts simple `unionFV`
    tyCoFVsOfBag tyCoFVsOfImplic implic

-- | Returns free variables of Implication as a composable FV computation.
-- See Note [Deterministic FV] in FV.
tyCoFVsOfImplic :: Implication -> FV
-- Only called on *zonked* things, hence no need to worry about flatten-skolems
tyCoFVsOfImplic (Implic { ic_skols = skols
                        , ic_given = givens
                        , ic_wanted = wanted })
  | isEmptyWC wanted
  = emptyFV
  | otherwise
  = tyCoFVsVarBndrs skols  $
    tyCoFVsVarBndrs givens $
    tyCoFVsOfWC wanted

tyCoFVsOfBag :: (a -> FV) -> Bag a -> FV
tyCoFVsOfBag tvs_of = foldrBag (unionFV . tvs_of) emptyFV

---------------------------
dropDerivedWC :: WantedConstraints -> WantedConstraints
-- See Note [Dropping derived constraints]
dropDerivedWC wc@(WC { wc_simple = simples })
  = wc { wc_simple = dropDerivedSimples simples }
    -- The wc_impl implications are already (recursively) filtered

--------------------------
dropDerivedSimples :: Cts -> Cts
-- Drop all Derived constraints, but make [W] back into [WD],
-- so that if we re-simplify these constraints we will get all
-- the right derived constraints re-generated.  Forgetting this
-- step led to #12936
dropDerivedSimples simples = mapMaybeBag dropDerivedCt simples

dropDerivedCt :: Ct -> Maybe Ct
dropDerivedCt ct
  = case ctEvFlavour ev of
      Wanted WOnly -> Just (ct' { cc_ev = ev_wd })
      Wanted _     -> Just ct'
      _ | isDroppableCt ct -> Nothing
        | otherwise        -> Just ct
  where
    ev    = ctEvidence ct
    ev_wd = ev { ctev_nosh = WDeriv }
    ct'   = setPendingScDict ct -- See Note [Resetting cc_pend_sc]

{- Note [Resetting cc_pend_sc]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we discard Derived constraints, in dropDerivedSimples, we must
set the cc_pend_sc flag to True, so that if we re-process this
CDictCan we will re-generate its derived superclasses. Otherwise
we might miss some fundeps.  Trac #13662 showed this up.

See Note [The superclass story] in TcCanonical.
-}

isDroppableCt :: Ct -> Bool
isDroppableCt ct
  = isDerived ev && not keep_deriv
    -- Drop only derived constraints, and then only if they
    -- obey Note [Dropping derived constraints]
  where
    ev   = ctEvidence ct
    loc  = ctEvLoc ev
    orig = ctLocOrigin loc

    keep_deriv
      = case ct of
          CHoleCan {} -> True
          CIrredCan { cc_insol = insoluble }
                      -> keep_eq insoluble
          _           -> keep_eq False

    keep_eq definitely_insoluble
       | isGivenOrigin orig    -- Arising only from givens
       = definitely_insoluble  -- Keep only definitely insoluble
       | otherwise
       = case orig of
           KindEqOrigin {} -> True    -- See Note [Dropping derived constraints]

           -- See Note [Dropping derived constraints]
           -- For fundeps, drop wanted/wanted interactions
           FunDepOrigin2 {} -> True   -- Top-level/Wanted
           FunDepOrigin1 _ loc1 _ loc2
             | g1 || g2  -> True  -- Given/Wanted errors: keep all
             | otherwise -> False -- Wanted/Wanted errors: discard
             where
               g1 = isGivenLoc loc1
               g2 = isGivenLoc loc2

           _ -> False

arisesFromGivens :: Ct -> Bool
arisesFromGivens ct
  = case ctEvidence ct of
      CtGiven {}                   -> True
      CtWanted {}                  -> False
      CtDerived { ctev_loc = loc } -> isGivenLoc loc

isGivenLoc :: CtLoc -> Bool
isGivenLoc loc = isGivenOrigin (ctLocOrigin loc)

isGivenOrigin :: CtOrigin -> Bool
isGivenOrigin (GivenOrigin {})          = True
isGivenOrigin (FunDepOrigin1 _ l1 _ l2) = isGivenLoc l1 && isGivenLoc l2
isGivenOrigin (FunDepOrigin2 _ o1 _ _)  = isGivenOrigin o1
isGivenOrigin _                         = False

{- Note [Dropping derived constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we discard derived constraints at the end of constraint solving;
see dropDerivedWC.  For example

 * Superclasses: if we have an unsolved [W] (Ord a), we don't want to
   complain about an unsolved [D] (Eq a) as well.

 * If we have [W] a ~ Int, [W] a ~ Bool, improvement will generate
   [D] Int ~ Bool, and we don't want to report that because it's
   incomprehensible. That is why we don't rewrite wanteds with wanteds!

 * We might float out some Wanteds from an implication, leaving behind
   their insoluble Deriveds. For example:

   forall a[2]. [W] alpha[1] ~ Int
                [W] alpha[1] ~ Bool
                [D] Int ~ Bool

   The Derived is insoluble, but we very much want to drop it when floating
   out.

But (tiresomely) we do keep *some* Derived constraints:

 * Type holes are derived constraints, because they have no evidence
   and we want to keep them, so we get the error report

 * Insoluble kind equalities (e.g. [D] * ~ (* -> *)), with
   KindEqOrigin, may arise from a type equality a ~ Int#, say.  See
   Note [Equalities with incompatible kinds] in TcCanonical.
   Keeping these around produces better error messages, in practice.
   E.g., test case dependent/should_fail/T11471

 * We keep most derived equalities arising from functional dependencies
      - Given/Given interactions (subset of FunDepOrigin1):
        The definitely-insoluble ones reflect unreachable code.

        Others not-definitely-insoluble ones like [D] a ~ Int do not
        reflect unreachable code; indeed if fundeps generated proofs, it'd
        be a useful equality.  See Trac #14763.   So we discard them.

      - Given/Wanted interacGiven or Wanted interacting with an
        instance declaration (FunDepOrigin2)

      - Given/Wanted interactions (FunDepOrigin1); see Trac #9612

      - But for Wanted/Wanted interactions we do /not/ want to report an
        error (Trac #13506).  Consider [W] C Int Int, [W] C Int Bool, with
        a fundep on class C.  We don't want to report an insoluble Int~Bool;
        c.f. "wanteds do not rewrite wanteds".

To distinguish these cases we use the CtOrigin.

NB: we keep *all* derived insolubles under some circumstances:

  * They are looked at by simplifyInfer, to decide whether to
    generalise.  Example: [W] a ~ Int, [W] a ~ Bool
    We get [D] Int ~ Bool, and indeed the constraints are insoluble,
    and we want simplifyInfer to see that, even though we don't
    ultimately want to generate an (inexplicable) error message from it


************************************************************************
*                                                                      *
                    CtEvidence
         The "flavor" of a canonical constraint
*                                                                      *
************************************************************************
-}

isWantedCt :: Ct -> Bool
isWantedCt = isWanted . ctEvidence

isGivenCt :: Ct -> Bool
isGivenCt = isGiven . ctEvidence

isDerivedCt :: Ct -> Bool
isDerivedCt = isDerived . ctEvidence

isCTyEqCan :: Ct -> Bool
isCTyEqCan (CTyEqCan {})  = True
isCTyEqCan (CFunEqCan {}) = False
isCTyEqCan _              = False

isCDictCan_Maybe :: Ct -> Maybe Class
isCDictCan_Maybe (CDictCan {cc_class = cls })  = Just cls
isCDictCan_Maybe _              = Nothing

isCFunEqCan_maybe :: Ct -> Maybe (TyCon, [Type])
isCFunEqCan_maybe (CFunEqCan { cc_fun = tc, cc_tyargs = xis }) = Just (tc, xis)
isCFunEqCan_maybe _ = Nothing

isCFunEqCan :: Ct -> Bool
isCFunEqCan (CFunEqCan {}) = True
isCFunEqCan _ = False

isCNonCanonical :: Ct -> Bool
isCNonCanonical (CNonCanonical {}) = True
isCNonCanonical _ = False

isHoleCt:: Ct -> Bool
isHoleCt (CHoleCan {}) = True
isHoleCt _ = False

isOutOfScopeCt :: Ct -> Bool
-- We treat expression holes representing out-of-scope variables a bit
-- differently when it comes to error reporting
isOutOfScopeCt (CHoleCan { cc_hole = ExprHole (OutOfScope {}) }) = True
isOutOfScopeCt _ = False

isExprHoleCt :: Ct -> Bool
isExprHoleCt (CHoleCan { cc_hole = ExprHole {} }) = True
isExprHoleCt _ = False

isTypeHoleCt :: Ct -> Bool
isTypeHoleCt (CHoleCan { cc_hole = TypeHole {} }) = True
isTypeHoleCt _ = False


{- Note [Custom type errors in constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When GHC reports a type-error about an unsolved-constraint, we check
to see if the constraint contains any custom-type errors, and if so
we report them.  Here are some examples of constraints containing type
errors:

TypeError msg           -- The actual constraint is a type error

TypError msg ~ Int      -- Some type was supposed to be Int, but ended up
                        -- being a type error instead

Eq (TypeError msg)      -- A class constraint is stuck due to a type error

F (TypeError msg) ~ a   -- A type function failed to evaluate due to a type err

It is also possible to have constraints where the type error is nested deeper,
for example see #11990, and also:

Eq (F (TypeError msg))  -- Here the type error is nested under a type-function
                        -- call, which failed to evaluate because of it,
                        -- and so the `Eq` constraint was unsolved.
                        -- This may happen when one function calls another
                        -- and the called function produced a custom type error.
-}

-- | A constraint is considered to be a custom type error, if it contains
-- custom type errors anywhere in it.
-- See Note [Custom type errors in constraints]
getUserTypeErrorMsg :: Ct -> Maybe Type
getUserTypeErrorMsg ct = findUserTypeError (ctPred ct)
  where
  findUserTypeError t = msum ( userTypeError_maybe t
                             : map findUserTypeError (subTys t)
                             )

  subTys t            = case splitAppTys t of
                          (t,[]) ->
                            case splitTyConApp_maybe t of
                              Nothing     -> []
                              Just (_,ts) -> ts
                          (t,ts) -> t : ts




isUserTypeErrorCt :: Ct -> Bool
isUserTypeErrorCt ct = case getUserTypeErrorMsg ct of
                         Just _ -> True
                         _      -> False

isPendingScDict :: Ct -> Maybe Ct
-- Says whether this is a CDictCan with cc_pend_sc is True,
-- AND if so flips the flag
isPendingScDict ct@(CDictCan { cc_pend_sc = True })
                  = Just (ct { cc_pend_sc = False })
isPendingScDict _ = Nothing

isPendingScInst :: QCInst -> Maybe QCInst
-- Same as isPrendinScDict, but for QCInsts
isPendingScInst qci@(QCI { qci_pend_sc = True })
                  = Just (qci { qci_pend_sc = False })
isPendingScInst _ = Nothing

setPendingScDict :: Ct -> Ct
-- Set the cc_pend_sc flag to True
setPendingScDict ct@(CDictCan { cc_pend_sc = False })
                    = ct { cc_pend_sc = True }
setPendingScDict ct = ct

superClassesMightHelp :: WantedConstraints -> Bool
-- ^ True if taking superclasses of givens, or of wanteds (to perhaps
-- expose more equalities or functional dependencies) might help to
-- solve this constraint.  See Note [When superclasses help]
superClassesMightHelp (WC { wc_simple = simples, wc_impl = implics })
  = anyBag might_help_ct simples || anyBag might_help_implic implics
  where
    might_help_implic ic
       | IC_Unsolved <- ic_status ic = superClassesMightHelp (ic_wanted ic)
       | otherwise                   = False

    might_help_ct ct = isWantedCt ct && not (is_ip ct)

    is_ip (CDictCan { cc_class = cls }) = isIPClass cls
    is_ip _                             = False

getPendingWantedScs :: Cts -> ([Ct], Cts)
getPendingWantedScs simples
  = mapAccumBagL get [] simples
  where
    get acc ct | Just ct' <- isPendingScDict ct
               = (ct':acc, ct')
               | otherwise
               = (acc,     ct)

{- Note [When superclasses help]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
First read Note [The superclass story] in TcCanonical.

We expand superclasses and iterate only if there is at unsolved wanted
for which expansion of superclasses (e.g. from given constraints)
might actually help. The function superClassesMightHelp tells if
doing this superclass expansion might help solve this constraint.
Note that

  * We look inside implications; maybe it'll help to expand the Givens
    at level 2 to help solve an unsolved Wanted buried inside an
    implication.  E.g.
        forall a. Ord a => forall b. [W] Eq a

  * Superclasses help only for Wanted constraints.  Derived constraints
    are not really "unsolved" and we certainly don't want them to
    trigger superclass expansion. This was a good part of the loop
    in  Trac #11523

  * Even for Wanted constraints, we say "no" for implicit parameters.
    we have [W] ?x::ty, expanding superclasses won't help:
      - Superclasses can't be implicit parameters
      - If we have a [G] ?x:ty2, then we'll have another unsolved
        [D] ty ~ ty2 (from the functional dependency)
        which will trigger superclass expansion.

    It's a bit of a special case, but it's easy to do.  The runtime cost
    is low because the unsolved set is usually empty anyway (errors
    aside), and the first non-imlicit-parameter will terminate the search.

    The special case is worth it (Trac #11480, comment:2) because it
    applies to CallStack constraints, which aren't type errors. If we have
       f :: (C a) => blah
       f x = ...undefined...
    we'll get a CallStack constraint.  If that's the only unsolved
    constraint it'll eventually be solved by defaulting.  So we don't
    want to emit warnings about hitting the simplifier's iteration
    limit.  A CallStack constraint really isn't an unsolved
    constraint; it can always be solved by defaulting.
-}

singleCt :: Ct -> Cts
singleCt = unitBag

andCts :: Cts -> Cts -> Cts
andCts = unionBags

listToCts :: [Ct] -> Cts
listToCts = listToBag

ctsElts :: Cts -> [Ct]
ctsElts = bagToList

consCts :: Ct -> Cts -> Cts
consCts = consBag

snocCts :: Cts -> Ct -> Cts
snocCts = snocBag

extendCtsList :: Cts -> [Ct] -> Cts
extendCtsList cts xs | null xs   = cts
                     | otherwise = cts `unionBags` listToBag xs

andManyCts :: [Cts] -> Cts
andManyCts = unionManyBags

emptyCts :: Cts
emptyCts = emptyBag

isEmptyCts :: Cts -> Bool
isEmptyCts = isEmptyBag

pprCts :: Cts -> SDoc
pprCts cts = vcat (map ppr (bagToList cts))

{-
************************************************************************
*                                                                      *
                Wanted constraints
     These are forced to be in TcRnTypes because
           TcLclEnv mentions WantedConstraints
           WantedConstraint mentions CtLoc
           CtLoc mentions ErrCtxt
           ErrCtxt mentions TcM
*                                                                      *
v%************************************************************************
-}

data WantedConstraints
  = WC { wc_simple :: Cts              -- Unsolved constraints, all wanted
       , wc_impl   :: Bag Implication
    }

emptyWC :: WantedConstraints
emptyWC = WC { wc_simple = emptyBag, wc_impl = emptyBag }

mkSimpleWC :: [CtEvidence] -> WantedConstraints
mkSimpleWC cts
  = WC { wc_simple = listToBag (map mkNonCanonical cts)
       , wc_impl = emptyBag }

mkImplicWC :: Bag Implication -> WantedConstraints
mkImplicWC implic
  = WC { wc_simple = emptyBag, wc_impl = implic }

isEmptyWC :: WantedConstraints -> Bool
isEmptyWC (WC { wc_simple = f, wc_impl = i })
  = isEmptyBag f && isEmptyBag i


-- | Checks whether a the given wanted constraints are solved, i.e.
-- that there are no simple constraints left and all the implications
-- are solved.
isSolvedWC :: WantedConstraints -> Bool
isSolvedWC WC {wc_simple = wc_simple, wc_impl = wc_impl} =
  isEmptyBag wc_simple && allBag (isSolvedStatus . ic_status) wc_impl

andWC :: WantedConstraints -> WantedConstraints -> WantedConstraints
andWC (WC { wc_simple = f1, wc_impl = i1 })
      (WC { wc_simple = f2, wc_impl = i2 })
  = WC { wc_simple = f1 `unionBags` f2
       , wc_impl   = i1 `unionBags` i2 }

unionsWC :: [WantedConstraints] -> WantedConstraints
unionsWC = foldr andWC emptyWC

addSimples :: WantedConstraints -> Bag Ct -> WantedConstraints
addSimples wc cts
  = wc { wc_simple = wc_simple wc `unionBags` cts }
    -- Consider: Put the new constraints at the front, so they get solved first

addImplics :: WantedConstraints -> Bag Implication -> WantedConstraints
addImplics wc implic = wc { wc_impl = wc_impl wc `unionBags` implic }

addInsols :: WantedConstraints -> Bag Ct -> WantedConstraints
addInsols wc cts
  = wc { wc_simple = wc_simple wc `unionBags` cts }

insolublesOnly :: WantedConstraints -> WantedConstraints
-- Keep only the definitely-insoluble constraints
insolublesOnly (WC { wc_simple = simples, wc_impl = implics })
  = WC { wc_simple = filterBag insolubleCt simples
       , wc_impl   = mapBag implic_insols_only implics }
  where
    implic_insols_only implic
      = implic { ic_wanted = insolublesOnly (ic_wanted implic) }

isSolvedStatus :: ImplicStatus -> Bool
isSolvedStatus (IC_Solved {}) = True
isSolvedStatus _              = False

isInsolubleStatus :: ImplicStatus -> Bool
isInsolubleStatus IC_Insoluble    = True
isInsolubleStatus IC_BadTelescope = True
isInsolubleStatus _               = False

insolubleImplic :: Implication -> Bool
insolubleImplic ic = isInsolubleStatus (ic_status ic)

insolubleWC :: WantedConstraints -> Bool
insolubleWC (WC { wc_impl = implics, wc_simple = simples })
  =  anyBag insolubleCt simples
  || anyBag insolubleImplic implics

insolubleCt :: Ct -> Bool
-- Definitely insoluble, in particular /excluding/ type-hole constraints
-- Namely: a) an equality constraint
--         b) that is insoluble
--         c) and does not arise from a Given
insolubleCt ct
  | isHoleCt ct            = isOutOfScopeCt ct  -- See Note [Insoluble holes]
  | not (insolubleEqCt ct) = False
  | arisesFromGivens ct    = False              -- See Note [Given insolubles]
  | otherwise              = True

insolubleEqCt :: Ct -> Bool
-- Returns True of /equality/ constraints
-- that are /definitely/ insoluble
-- It won't detect some definite errors like
--       F a ~ T (F a)
-- where F is a type family, which actually has an occurs check
--
-- The function is tuned for application /after/ constraint solving
--       i.e. assuming canonicalisation has been done
-- E.g.  It'll reply True  for     a ~ [a]
--               but False for   [a] ~ a
-- and
--                   True for  Int ~ F a Int
--               but False for  Maybe Int ~ F a Int Int
--               (where F is an arity-1 type function)
insolubleEqCt (CIrredCan { cc_insol = insol }) = insol
insolubleEqCt _                                = False

instance Outputable WantedConstraints where
  ppr (WC {wc_simple = s, wc_impl = i})
   = text "WC" <+> braces (vcat
        [ ppr_bag (text "wc_simple") s
        , ppr_bag (text "wc_impl") i ])

ppr_bag :: Outputable a => SDoc -> Bag a -> SDoc
ppr_bag doc bag
 | isEmptyBag bag = empty
 | otherwise      = hang (doc <+> equals)
                       2 (foldrBag (($$) . ppr) empty bag)

{- Note [Given insolubles]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider (Trac #14325, comment:)
    class (a~b) => C a b

    foo :: C a c => a -> c
    foo x = x

    hm3 :: C (f b) b => b -> f b
    hm3 x = foo x

In the RHS of hm3, from the [G] C (f b) b we get the insoluble
[G] f b ~# b.  Then we also get an unsolved [W] C b (f b).
Residual implication looks like
    forall b. C (f b) b => [G] f b ~# b
                           [W] C f (f b)

We do /not/ want to set the implication status to IC_Insoluble,
because that'll suppress reports of [W] C b (f b).  But we
may not report the insoluble [G] f b ~# b either (see Note [Given errors]
in TcErrors), so we may fail to report anything at all!  Yikes.

The same applies to Derived constraints that /arise from/ Givens.
E.g.   f :: (C Int [a]) => blah
where a fundep means we get
       [D] Int ~ [a]
By the same reasoning we must not suppress other errors (Trac #15767)

Bottom line: insolubleWC (called in TcSimplify.setImplicationStatus)
             should ignore givens even if they are insoluble.

Note [Insoluble holes]
~~~~~~~~~~~~~~~~~~~~~~
Hole constraints that ARE NOT treated as truly insoluble:
  a) type holes, arising from PartialTypeSignatures,
  b) "true" expression holes arising from TypedHoles

An "expression hole" or "type hole" constraint isn't really an error
at all; it's a report saying "_ :: Int" here.  But an out-of-scope
variable masquerading as expression holes IS treated as truly
insoluble, so that it trumps other errors during error reporting.
Yuk!

************************************************************************
*                                                                      *
                Implication constraints
*                                                                      *
************************************************************************
-}

data Implication
  = Implic {   -- Invariants for a tree of implications:
               -- see TcType Note [TcLevel and untouchable type variables]

      ic_tclvl :: TcLevel,       -- TcLevel of unification variables
                                 -- allocated /inside/ this implication

      ic_skols :: [TcTyVar],     -- Introduced skolems
      ic_info  :: SkolemInfo,    -- See Note [Skolems in an implication]
                                 -- See Note [Shadowing in a constraint]
      ic_telescope :: Maybe SDoc,  -- User-written telescope, if there is one
                                   -- The list of skolems is order-checked
                                   -- if and only if this is a Just.
                                   -- See Note [Keeping scoped variables in order: Explicit]
                                   -- in TcHsType

      ic_given  :: [EvVar],      -- Given evidence variables
                                 --   (order does not matter)
                                 -- See Invariant (GivenInv) in TcType

      ic_no_eqs :: Bool,         -- True  <=> ic_givens have no equalities, for sure
                                 -- False <=> ic_givens might have equalities

      ic_env   :: Env TcGblEnv TcLclEnv,
                                 -- Records the Env at the time of creation.
                                 --
                                 -- This is primarly needed for the enclosed
                                 -- TcLclEnv, which gives the source location
                                 -- and error context for the implication, and
                                 -- hence for all the given evidence variables.
                                 --
                                 -- The enclosed DynFlags also influences error
                                 -- reporting. See Note [Avoid
                                 -- -Winaccessible-code when deriving] in
                                 -- TcInstDcls.

      ic_wanted :: WantedConstraints,  -- The wanteds
                                       -- See Invariang (WantedInf) in TcType

      ic_binds  :: EvBindsVar,    -- Points to the place to fill in the
                                  -- abstraction and bindings.

      -- The ic_need fields keep track of which Given evidence
      -- is used by this implication or its children
      -- NB: including stuff used by nested implications that have since
      --     been discarded
      ic_need_inner :: VarSet,    -- Includes all used Given evidence
      ic_need_outer :: VarSet,    -- Includes only the free Given evidence
                                  --  i.e. ic_need_inner after deleting
                                  --       (a) givens (b) binders of ic_binds

      ic_status   :: ImplicStatus
    }

-- | Create a new 'Implication' with as many sensible defaults for its fields
-- as possible. Note that the 'ic_tclvl', 'ic_binds', and 'ic_info' fields do
-- /not/ have sensible defaults, so they are initialized with lazy thunks that
-- will 'panic' if forced, so one should take care to initialize these fields
-- after creation.
--
-- This is monadic purely to look up the 'Env', which is used to initialize
-- 'ic_env'.
newImplication :: TcM Implication
newImplication
  = do env <- getEnv
       return (implicationPrototype { ic_env = env })

implicationPrototype :: Implication
implicationPrototype
   = Implic { -- These fields must be initialised
              ic_tclvl      = panic "newImplic:tclvl"
            , ic_binds      = panic "newImplic:binds"
            , ic_info       = panic "newImplic:info"
            , ic_env        = panic "newImplic:env"

              -- The rest have sensible default values
            , ic_skols      = []
            , ic_telescope  = Nothing
            , ic_given      = []
            , ic_wanted     = emptyWC
            , ic_no_eqs     = False
            , ic_status     = IC_Unsolved
            , ic_need_inner = emptyVarSet
            , ic_need_outer = emptyVarSet }

-- | Retrieve the enclosed 'TcLclEnv' from an 'Implication'.
implicLclEnv :: Implication -> TcLclEnv
implicLclEnv = env_lcl . ic_env

-- | Retrieve the enclosed 'DynFlags' from an 'Implication'.
implicDynFlags :: Implication -> DynFlags
implicDynFlags = hsc_dflags . env_top . ic_env

data ImplicStatus
  = IC_Solved     -- All wanteds in the tree are solved, all the way down
       { ics_dead :: [EvVar] }  -- Subset of ic_given that are not needed
         -- See Note [Tracking redundant constraints] in TcSimplify

  | IC_Insoluble  -- At least one insoluble constraint in the tree

  | IC_BadTelescope  -- solved, but the skolems in the telescope are out of
                     -- dependency order

  | IC_Unsolved   -- Neither of the above; might go either way

instance Outputable Implication where
  ppr (Implic { ic_tclvl = tclvl, ic_skols = skols
              , ic_given = given, ic_no_eqs = no_eqs
              , ic_wanted = wanted, ic_status = status
              , ic_binds = binds
              , ic_need_inner = need_in, ic_need_outer = need_out
              , ic_info = info })
   = hang (text "Implic" <+> lbrace)
        2 (sep [ text "TcLevel =" <+> ppr tclvl
               , text "Skolems =" <+> pprTyVars skols
               , text "No-eqs =" <+> ppr no_eqs
               , text "Status =" <+> ppr status
               , hang (text "Given =")  2 (pprEvVars given)
               , hang (text "Wanted =") 2 (ppr wanted)
               , text "Binds =" <+> ppr binds
               , whenPprDebug (text "Needed inner =" <+> ppr need_in)
               , whenPprDebug (text "Needed outer =" <+> ppr need_out)
               , pprSkolInfo info ] <+> rbrace)

instance Outputable ImplicStatus where
  ppr IC_Insoluble    = text "Insoluble"
  ppr IC_BadTelescope = text "Bad telescope"
  ppr IC_Unsolved     = text "Unsolved"
  ppr (IC_Solved { ics_dead = dead })
    = text "Solved" <+> (braces (text "Dead givens =" <+> ppr dead))

{-
Note [Needed evidence variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Th ic_need_evs field holds the free vars of ic_binds, and all the
ic_binds in nested implications.

  * Main purpose: if one of the ic_givens is not mentioned in here, it
    is redundant.

  * solveImplication may drop an implication altogether if it has no
    remaining 'wanteds'. But we still track the free vars of its
    evidence binds, even though it has now disappeared.

Note [Shadowing in a constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We assume NO SHADOWING in a constraint.  Specifically
 * The unification variables are all implicitly quantified at top
   level, and are all unique
 * The skolem variables bound in ic_skols are all freah when the
   implication is created.
So we can safely substitute. For example, if we have
   forall a.  a~Int => ...(forall b. ...a...)...
we can push the (a~Int) constraint inwards in the "givens" without
worrying that 'b' might clash.

Note [Skolems in an implication]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The skolems in an implication are not there to perform a skolem escape
check.  That happens because all the environment variables are in the
untouchables, and therefore cannot be unified with anything at all,
let alone the skolems.

Instead, ic_skols is used only when considering floating a constraint
outside the implication in TcSimplify.floatEqualities or
TcSimplify.approximateImplications

Note [Insoluble constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some of the errors that we get during canonicalization are best
reported when all constraints have been simplified as much as
possible. For instance, assume that during simplification the
following constraints arise:

 [Wanted]   F alpha ~  uf1
 [Wanted]   beta ~ uf1 beta

When canonicalizing the wanted (beta ~ uf1 beta), if we eagerly fail
we will simply see a message:
    'Can't construct the infinite type  beta ~ uf1 beta'
and the user has no idea what the uf1 variable is.

Instead our plan is that we will NOT fail immediately, but:
    (1) Record the "frozen" error in the ic_insols field
    (2) Isolate the offending constraint from the rest of the inerts
    (3) Keep on simplifying/canonicalizing

At the end, we will hopefully have substituted uf1 := F alpha, and we
will be able to report a more informative error:
    'Can't construct the infinite type beta ~ F alpha beta'

Insoluble constraints *do* include Derived constraints. For example,
a functional dependency might give rise to [D] Int ~ Bool, and we must
report that.  If insolubles did not contain Deriveds, reportErrors would
never see it.


************************************************************************
*                                                                      *
            Pretty printing
*                                                                      *
************************************************************************
-}

pprEvVars :: [EvVar] -> SDoc    -- Print with their types
pprEvVars ev_vars = vcat (map pprEvVarWithType ev_vars)

pprEvVarTheta :: [EvVar] -> SDoc
pprEvVarTheta ev_vars = pprTheta (map evVarPred ev_vars)

pprEvVarWithType :: EvVar -> SDoc
pprEvVarWithType v = ppr v <+> dcolon <+> pprType (evVarPred v)



-- | Wraps the given type with the constraints (via ic_given) in the given
-- implication, according to the variables mentioned (via ic_skols)
-- in the implication, but taking care to only wrap those variables
-- that are mentioned in the type or the implication.
wrapTypeWithImplication :: Type -> Implication -> Type
wrapTypeWithImplication ty impl = wrapType ty mentioned_skols givens
    where givens = map idType $ ic_given impl
          skols = ic_skols impl
          freeVars = fvVarSet $ tyCoFVsOfTypes (ty:givens)
          mentioned_skols = filter (`elemVarSet` freeVars) skols

wrapType :: Type -> [TyVar] -> [PredType] -> Type
wrapType ty skols givens = mkSpecForAllTys skols $ mkFunTys givens ty


{-
************************************************************************
*                                                                      *
            CtEvidence
*                                                                      *
************************************************************************

Note [Evidence field of CtEvidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During constraint solving we never look at the type of ctev_evar/ctev_dest;
instead we look at the ctev_pred field.  The evtm/evar field
may be un-zonked.

Note [Bind new Givens immediately]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For Givens we make new EvVars and bind them immediately. Two main reasons:
  * Gain sharing.  E.g. suppose we start with g :: C a b, where
       class D a => C a b
       class (E a, F a) => D a
    If we generate all g's superclasses as separate EvTerms we might
    get    selD1 (selC1 g) :: E a
           selD2 (selC1 g) :: F a
           selC1 g :: D a
    which we could do more economically as:
           g1 :: D a = selC1 g
           g2 :: E a = selD1 g1
           g3 :: F a = selD2 g1

  * For *coercion* evidence we *must* bind each given:
      class (a~b) => C a b where ....
      f :: C a b => ....
    Then in f's Givens we have g:(C a b) and the superclass sc(g,0):a~b.
    But that superclass selector can't (yet) appear in a coercion
    (see evTermCoercion), so the easy thing is to bind it to an Id.

So a Given has EvVar inside it rather than (as previously) an EvTerm.

-}

-- | A place for type-checking evidence to go after it is generated.
-- Wanted equalities are always HoleDest; other wanteds are always
-- EvVarDest.
data TcEvDest
  = EvVarDest EvVar         -- ^ bind this var to the evidence
              -- EvVarDest is always used for non-type-equalities
              -- e.g. class constraints

  | HoleDest  CoercionHole  -- ^ fill in this hole with the evidence
              -- HoleDest is always used for type-equalities
              -- See Note [Coercion holes] in TyCoRep

data CtEvidence
  = CtGiven    -- Truly given, not depending on subgoals
      { ctev_pred :: TcPredType      -- See Note [Ct/evidence invariant]
      , ctev_evar :: EvVar           -- See Note [Evidence field of CtEvidence]
      , ctev_loc  :: CtLoc }


  | CtWanted   -- Wanted goal
      { ctev_pred :: TcPredType     -- See Note [Ct/evidence invariant]
      , ctev_dest :: TcEvDest
      , ctev_nosh :: ShadowInfo     -- See Note [Constraint flavours]
      , ctev_loc  :: CtLoc }

  | CtDerived  -- A goal that we don't really have to solve and can't
               -- immediately rewrite anything other than a derived
               -- (there's no evidence!) but if we do manage to solve
               -- it may help in solving other goals.
      { ctev_pred :: TcPredType
      , ctev_loc  :: CtLoc }

ctEvPred :: CtEvidence -> TcPredType
-- The predicate of a flavor
ctEvPred = ctev_pred

ctEvLoc :: CtEvidence -> CtLoc
ctEvLoc = ctev_loc

ctEvOrigin :: CtEvidence -> CtOrigin
ctEvOrigin = ctLocOrigin . ctEvLoc

-- | Get the equality relation relevant for a 'CtEvidence'
ctEvEqRel :: CtEvidence -> EqRel
ctEvEqRel = predTypeEqRel . ctEvPred

-- | Get the role relevant for a 'CtEvidence'
ctEvRole :: CtEvidence -> Role
ctEvRole = eqRelRole . ctEvEqRel

ctEvTerm :: CtEvidence -> EvTerm
ctEvTerm ev = EvExpr (ctEvExpr ev)

ctEvExpr :: CtEvidence -> EvExpr
ctEvExpr ev@(CtWanted { ctev_dest = HoleDest _ })
            = Coercion $ ctEvCoercion ev
ctEvExpr ev = evId (ctEvEvId ev)

ctEvCoercion :: HasDebugCallStack => CtEvidence -> Coercion
ctEvCoercion (CtGiven { ctev_evar = ev_id })
  = mkTcCoVarCo ev_id
ctEvCoercion (CtWanted { ctev_dest = dest })
  | HoleDest hole <- dest
  = -- ctEvCoercion is only called on type equalities
    -- and they always have HoleDests
    mkHoleCo hole
ctEvCoercion ev
  = pprPanic "ctEvCoercion" (ppr ev)

ctEvEvId :: CtEvidence -> EvVar
ctEvEvId (CtWanted { ctev_dest = EvVarDest ev }) = ev
ctEvEvId (CtWanted { ctev_dest = HoleDest h })   = coHoleCoVar h
ctEvEvId (CtGiven  { ctev_evar = ev })           = ev
ctEvEvId ctev@(CtDerived {}) = pprPanic "ctEvId:" (ppr ctev)

instance Outputable TcEvDest where
  ppr (HoleDest h)   = text "hole" <> ppr h
  ppr (EvVarDest ev) = ppr ev

instance Outputable CtEvidence where
  ppr ev = ppr (ctEvFlavour ev)
           <+> pp_ev
           <+> braces (ppr (ctl_depth (ctEvLoc ev))) <> dcolon
                  -- Show the sub-goal depth too
           <+> ppr (ctEvPred ev)
    where
      pp_ev = case ev of
             CtGiven { ctev_evar = v } -> ppr v
             CtWanted {ctev_dest = d } -> ppr d
             CtDerived {}              -> text "_"

isWanted :: CtEvidence -> Bool
isWanted (CtWanted {}) = True
isWanted _ = False

isGiven :: CtEvidence -> Bool
isGiven (CtGiven {})  = True
isGiven _ = False

isDerived :: CtEvidence -> Bool
isDerived (CtDerived {}) = True
isDerived _              = False

{-
%************************************************************************
%*                                                                      *
            CtFlavour
%*                                                                      *
%************************************************************************

Note [Constraint flavours]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Constraints come in four flavours:

* [G] Given: we have evidence

* [W] Wanted WOnly: we want evidence

* [D] Derived: any solution must satisfy this constraint, but
      we don't need evidence for it.  Examples include:
        - superclasses of [W] class constraints
        - equalities arising from functional dependencies
          or injectivity

* [WD] Wanted WDeriv: a single constraint that represents
                      both [W] and [D]
  We keep them paired as one both for efficiency, and because
  when we have a finite map  F tys -> CFunEqCan, it's inconvenient
  to have two CFunEqCans in the range

The ctev_nosh field of a Wanted distinguishes between [W] and [WD]

Wanted constraints are born as [WD], but are split into [W] and its
"shadow" [D] in TcSMonad.maybeEmitShadow.

See Note [The improvement story and derived shadows] in TcSMonad
-}

data CtFlavour  -- See Note [Constraint flavours]
  = Given
  | Wanted ShadowInfo
  | Derived
  deriving Eq

data ShadowInfo
  = WDeriv   -- [WD] This Wanted constraint has no Derived shadow,
             -- so it behaves like a pair of a Wanted and a Derived
  | WOnly    -- [W] It has a separate derived shadow
             -- See Note [Derived shadows]
  deriving( Eq )

isGivenOrWDeriv :: CtFlavour -> Bool
isGivenOrWDeriv Given           = True
isGivenOrWDeriv (Wanted WDeriv) = True
isGivenOrWDeriv _               = False

instance Outputable CtFlavour where
  ppr Given           = text "[G]"
  ppr (Wanted WDeriv) = text "[WD]"
  ppr (Wanted WOnly)  = text "[W]"
  ppr Derived         = text "[D]"

ctEvFlavour :: CtEvidence -> CtFlavour
ctEvFlavour (CtWanted { ctev_nosh = nosh }) = Wanted nosh
ctEvFlavour (CtGiven {})                    = Given
ctEvFlavour (CtDerived {})                  = Derived

-- | Whether or not one 'Ct' can rewrite another is determined by its
-- flavour and its equality relation. See also
-- Note [Flavours with roles] in TcSMonad
type CtFlavourRole = (CtFlavour, EqRel)

-- | Extract the flavour, role, and boxity from a 'CtEvidence'
ctEvFlavourRole :: CtEvidence -> CtFlavourRole
ctEvFlavourRole ev = (ctEvFlavour ev, ctEvEqRel ev)

-- | Extract the flavour and role from a 'Ct'
ctFlavourRole :: Ct -> CtFlavourRole
-- Uses short-cuts to role for special cases
ctFlavourRole (CDictCan { cc_ev = ev })
  = (ctEvFlavour ev, NomEq)
ctFlavourRole (CTyEqCan { cc_ev = ev, cc_eq_rel = eq_rel })
  = (ctEvFlavour ev, eq_rel)
ctFlavourRole (CFunEqCan { cc_ev = ev })
  = (ctEvFlavour ev, NomEq)
ctFlavourRole (CHoleCan { cc_ev = ev })
  = (ctEvFlavour ev, NomEq)  -- NomEq: CHoleCans can be rewritten by
                             -- by nominal equalities but empahatically
                             -- not by representational equalities
ctFlavourRole ct
  = ctEvFlavourRole (ctEvidence ct)

{- Note [eqCanRewrite]
~~~~~~~~~~~~~~~~~~~~~~
(eqCanRewrite ct1 ct2) holds if the constraint ct1 (a CTyEqCan of form
tv ~ ty) can be used to rewrite ct2.  It must satisfy the properties of
a can-rewrite relation, see Definition [Can-rewrite relation] in
TcSMonad.

With the solver handling Coercible constraints like equality constraints,
the rewrite conditions must take role into account, never allowing
a representational equality to rewrite a nominal one.

Note [Wanteds do not rewrite Wanteds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't allow Wanteds to rewrite Wanteds, because that can give rise
to very confusing type error messages.  A good example is Trac #8450.
Here's another
   f :: a -> Bool
   f x = ( [x,'c'], [x,True] ) `seq` True
Here we get
  [W] a ~ Char
  [W] a ~ Bool
but we do not want to complain about Bool ~ Char!

Note [Deriveds do rewrite Deriveds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
However we DO allow Deriveds to rewrite Deriveds, because that's how
improvement works; see Note [The improvement story] in TcInteract.

However, for now at least I'm only letting (Derived,NomEq) rewrite
(Derived,NomEq) and not doing anything for ReprEq.  If we have
    eqCanRewriteFR (Derived, NomEq) (Derived, _)  = True
then we lose property R2 of Definition [Can-rewrite relation]
in TcSMonad
  R2.  If f1 >= f, and f2 >= f,
       then either f1 >= f2 or f2 >= f1
Consider f1 = (Given, ReprEq)
         f2 = (Derived, NomEq)
          f = (Derived, ReprEq)

I thought maybe we could never get Derived ReprEq constraints, but
we can; straight from the Wanteds during improvement. And from a Derived
ReprEq we could conceivably get a Derived NomEq improvement (by decomposing
a type constructor with Nomninal role), and hence unify.
-}

eqCanRewrite :: EqRel -> EqRel -> Bool
eqCanRewrite NomEq  _      = True
eqCanRewrite ReprEq ReprEq = True
eqCanRewrite ReprEq NomEq  = False

eqCanRewriteFR :: CtFlavourRole -> CtFlavourRole -> Bool
-- Can fr1 actually rewrite fr2?
-- Very important function!
-- See Note [eqCanRewrite]
-- See Note [Wanteds do not rewrite Wanteds]
-- See Note [Deriveds do rewrite Deriveds]
eqCanRewriteFR (Given,         r1)    (_,       r2)    = eqCanRewrite r1 r2
eqCanRewriteFR (Wanted WDeriv, NomEq) (Derived, NomEq) = True
eqCanRewriteFR (Derived,       NomEq) (Derived, NomEq) = True
eqCanRewriteFR _                      _                = False

eqMayRewriteFR :: CtFlavourRole -> CtFlavourRole -> Bool
-- Is it /possible/ that fr1 can rewrite fr2?
-- This is used when deciding which inerts to kick out,
-- at which time a [WD] inert may be split into [W] and [D]
eqMayRewriteFR (Wanted WDeriv, NomEq) (Wanted WDeriv, NomEq) = True
eqMayRewriteFR (Derived,       NomEq) (Wanted WDeriv, NomEq) = True
eqMayRewriteFR fr1 fr2 = eqCanRewriteFR fr1 fr2

-----------------
{- Note [funEqCanDischarge]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have two CFunEqCans with the same LHS:
    (x1:F ts ~ f1) `funEqCanDischarge` (x2:F ts ~ f2)
Can we drop x2 in favour of x1, either unifying
f2 (if it's a flatten meta-var) or adding a new Given
(f1 ~ f2), if x2 is a Given?

Answer: yes if funEqCanDischarge is true.
-}

funEqCanDischarge
  :: CtEvidence -> CtEvidence
  -> ( SwapFlag   -- NotSwapped => lhs can discharge rhs
                  -- Swapped    => rhs can discharge lhs
     , Bool)      -- True <=> upgrade non-discharded one
                  --          from [W] to [WD]
-- See Note [funEqCanDischarge]
funEqCanDischarge ev1 ev2
  = ASSERT2( ctEvEqRel ev1 == NomEq, ppr ev1 )
    ASSERT2( ctEvEqRel ev2 == NomEq, ppr ev2 )
    -- CFunEqCans are all Nominal, hence asserts
    funEqCanDischargeF (ctEvFlavour ev1) (ctEvFlavour ev2)

funEqCanDischargeF :: CtFlavour -> CtFlavour -> (SwapFlag, Bool)
funEqCanDischargeF Given           _               = (NotSwapped, False)
funEqCanDischargeF _               Given           = (IsSwapped,  False)
funEqCanDischargeF (Wanted WDeriv) _               = (NotSwapped, False)
funEqCanDischargeF _               (Wanted WDeriv) = (IsSwapped,  True)
funEqCanDischargeF (Wanted WOnly)  (Wanted WOnly)  = (NotSwapped, False)
funEqCanDischargeF (Wanted WOnly)  Derived         = (NotSwapped, True)
funEqCanDischargeF Derived         (Wanted WOnly)  = (IsSwapped,  True)
funEqCanDischargeF Derived         Derived         = (NotSwapped, False)


{- Note [eqCanDischarge]
~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have two identical CTyEqCan equality constraints
(i.e. both LHS and RHS are the same)
      (x1:a~t) `eqCanDischarge` (xs:a~t)
Can we just drop x2 in favour of x1?

Answer: yes if eqCanDischarge is true.

Note that we do /not/ allow Wanted to discharge Derived.
We must keep both.  Why?  Because the Derived may rewrite
other Deriveds in the model whereas the Wanted cannot.

However a Wanted can certainly discharge an identical Wanted.  So
eqCanDischarge does /not/ define a can-rewrite relation in the
sense of Definition [Can-rewrite relation] in TcSMonad.

We /do/ say that a [W] can discharge a [WD].  In evidence terms it
certainly can, and the /caller/ arranges that the otherwise-lost [D]
is spat out as a new Derived.  -}

eqCanDischargeFR :: CtFlavourRole -> CtFlavourRole -> Bool
-- See Note [eqCanDischarge]
eqCanDischargeFR (f1,r1) (f2, r2) =  eqCanRewrite r1 r2
                                  && eqCanDischargeF f1 f2

eqCanDischargeF :: CtFlavour -> CtFlavour -> Bool
eqCanDischargeF Given   _                  = True
eqCanDischargeF (Wanted _)      (Wanted _) = True
eqCanDischargeF (Wanted WDeriv) Derived    = True
eqCanDischargeF Derived         Derived    = True
eqCanDischargeF _               _          = False


{-
************************************************************************
*                                                                      *
            SubGoalDepth
*                                                                      *
************************************************************************

Note [SubGoalDepth]
~~~~~~~~~~~~~~~~~~~
The 'SubGoalDepth' takes care of stopping the constraint solver from looping.

The counter starts at zero and increases. It includes dictionary constraints,
equality simplification, and type family reduction. (Why combine these? Because
it's actually quite easy to mistake one for another, in sufficiently involved
scenarios, like ConstraintKinds.)

The flag -fcontext-stack=n (not very well named!) fixes the maximium
level.

* The counter includes the depth of type class instance declarations.  Example:
     [W] d{7} : Eq [Int]
  That is d's dictionary-constraint depth is 7.  If we use the instance
     $dfEqList :: Eq a => Eq [a]
  to simplify it, we get
     d{7} = $dfEqList d'{8}
  where d'{8} : Eq Int, and d' has depth 8.

  For civilised (decidable) instance declarations, each increase of
  depth removes a type constructor from the type, so the depth never
  gets big; i.e. is bounded by the structural depth of the type.

* The counter also increments when resolving
equalities involving type functions. Example:
  Assume we have a wanted at depth 7:
    [W] d{7} : F () ~ a
  If there is a type function equation "F () = Int", this would be rewritten to
    [W] d{8} : Int ~ a
  and remembered as having depth 8.

  Again, without UndecidableInstances, this counter is bounded, but without it
  can resolve things ad infinitum. Hence there is a maximum level.

* Lastly, every time an equality is rewritten, the counter increases. Again,
  rewriting an equality constraint normally makes progress, but it's possible
  the "progress" is just the reduction of an infinitely-reducing type family.
  Hence we need to track the rewrites.

When compiling a program requires a greater depth, then GHC recommends turning
off this check entirely by setting -freduction-depth=0. This is because the
exact number that works is highly variable, and is likely to change even between
minor releases. Because this check is solely to prevent infinite compilation
times, it seems safe to disable it when a user has ascertained that their program
doesn't loop at the type level.

-}

-- | See Note [SubGoalDepth]
newtype SubGoalDepth = SubGoalDepth Int
  deriving (Eq, Ord, Outputable)

initialSubGoalDepth :: SubGoalDepth
initialSubGoalDepth = SubGoalDepth 0

bumpSubGoalDepth :: SubGoalDepth -> SubGoalDepth
bumpSubGoalDepth (SubGoalDepth n) = SubGoalDepth (n + 1)

maxSubGoalDepth :: SubGoalDepth -> SubGoalDepth -> SubGoalDepth
maxSubGoalDepth (SubGoalDepth n) (SubGoalDepth m) = SubGoalDepth (n `max` m)

subGoalDepthExceeded :: DynFlags -> SubGoalDepth -> Bool
subGoalDepthExceeded dflags (SubGoalDepth d)
  = mkIntWithInf d > reductionDepth dflags

{-
************************************************************************
*                                                                      *
            CtLoc
*                                                                      *
************************************************************************

The 'CtLoc' gives information about where a constraint came from.
This is important for decent error message reporting because
dictionaries don't appear in the original source code.
type will evolve...

-}

data CtLoc = CtLoc { ctl_origin :: CtOrigin
                   , ctl_env    :: TcLclEnv
                   , ctl_t_or_k :: Maybe TypeOrKind  -- OK if we're not sure
                   , ctl_depth  :: !SubGoalDepth }

  -- The TcLclEnv includes particularly
  --    source location:  tcl_loc   :: RealSrcSpan
  --    context:          tcl_ctxt  :: [ErrCtxt]
  --    binder stack:     tcl_bndrs :: TcBinderStack
  --    level:            tcl_tclvl :: TcLevel

mkKindLoc :: TcType -> TcType   -- original *types* being compared
          -> CtLoc -> CtLoc
mkKindLoc s1 s2 loc = setCtLocOrigin (toKindLoc loc)
                        (KindEqOrigin s1 (Just s2) (ctLocOrigin loc)
                                      (ctLocTypeOrKind_maybe loc))

-- | Take a CtLoc and moves it to the kind level
toKindLoc :: CtLoc -> CtLoc
toKindLoc loc = loc { ctl_t_or_k = Just KindLevel }

mkGivenLoc :: TcLevel -> SkolemInfo -> TcLclEnv -> CtLoc
mkGivenLoc tclvl skol_info env
  = CtLoc { ctl_origin = GivenOrigin skol_info
          , ctl_env    = env { tcl_tclvl = tclvl }
          , ctl_t_or_k = Nothing    -- this only matters for error msgs
          , ctl_depth  = initialSubGoalDepth }

ctLocEnv :: CtLoc -> TcLclEnv
ctLocEnv = ctl_env

ctLocLevel :: CtLoc -> TcLevel
ctLocLevel loc = tcl_tclvl (ctLocEnv loc)

ctLocDepth :: CtLoc -> SubGoalDepth
ctLocDepth = ctl_depth

ctLocOrigin :: CtLoc -> CtOrigin
ctLocOrigin = ctl_origin

ctLocSpan :: CtLoc -> RealSrcSpan
ctLocSpan (CtLoc { ctl_env = lcl}) = tcl_loc lcl

ctLocTypeOrKind_maybe :: CtLoc -> Maybe TypeOrKind
ctLocTypeOrKind_maybe = ctl_t_or_k

setCtLocSpan :: CtLoc -> RealSrcSpan -> CtLoc
setCtLocSpan ctl@(CtLoc { ctl_env = lcl }) loc = setCtLocEnv ctl (lcl { tcl_loc = loc })

bumpCtLocDepth :: CtLoc -> CtLoc
bumpCtLocDepth loc@(CtLoc { ctl_depth = d }) = loc { ctl_depth = bumpSubGoalDepth d }

setCtLocOrigin :: CtLoc -> CtOrigin -> CtLoc
setCtLocOrigin ctl orig = ctl { ctl_origin = orig }

updateCtLocOrigin :: CtLoc -> (CtOrigin -> CtOrigin) -> CtLoc
updateCtLocOrigin ctl@(CtLoc { ctl_origin = orig }) upd
  = ctl { ctl_origin = upd orig }

setCtLocEnv :: CtLoc -> TcLclEnv -> CtLoc
setCtLocEnv ctl env = ctl { ctl_env = env }

pushErrCtxt :: CtOrigin -> ErrCtxt -> CtLoc -> CtLoc
pushErrCtxt o err loc@(CtLoc { ctl_env = lcl })
  = loc { ctl_origin = o, ctl_env = lcl { tcl_ctxt = err : tcl_ctxt lcl } }

pushErrCtxtSameOrigin :: ErrCtxt -> CtLoc -> CtLoc
-- Just add information w/o updating the origin!
pushErrCtxtSameOrigin err loc@(CtLoc { ctl_env = lcl })
  = loc { ctl_env = lcl { tcl_ctxt = err : tcl_ctxt lcl } }

{-
************************************************************************
*                                                                      *
                SkolemInfo
*                                                                      *
************************************************************************
-}

-- SkolemInfo gives the origin of *given* constraints
--   a) type variables are skolemised
--   b) an implication constraint is generated
data SkolemInfo
  = SigSkol -- A skolem that is created by instantiating
            -- a programmer-supplied type signature
            -- Location of the binding site is on the TyVar
            -- See Note [SigSkol SkolemInfo]
       UserTypeCtxt        -- What sort of signature
       TcType              -- Original type signature (before skolemisation)
       [(Name,TcTyVar)]    -- Maps the original name of the skolemised tyvar
                           -- to its instantiated version

  | SigTypeSkol UserTypeCtxt
                 -- like SigSkol, but when we're kind-checking the *type*
                 -- hence, we have less info

  | ForAllSkol SDoc     -- Bound by a user-written "forall".

  | DerivSkol Type      -- Bound by a 'deriving' clause;
                        -- the type is the instance we are trying to derive

  | InstSkol            -- Bound at an instance decl
  | InstSC TypeSize     -- A "given" constraint obtained by superclass selection.
                        -- If (C ty1 .. tyn) is the largest class from
                        --    which we made a superclass selection in the chain,
                        --    then TypeSize = sizeTypes [ty1, .., tyn]
                        -- See Note [Solving superclass constraints] in TcInstDcls

  | FamInstSkol         -- Bound at a family instance decl
  | PatSkol             -- An existential type variable bound by a pattern for
      ConLike           -- a data constructor with an existential type.
      (HsMatchContext Name)
             -- e.g.   data T = forall a. Eq a => MkT a
             --        f (MkT x) = ...
             -- The pattern MkT x will allocate an existential type
             -- variable for 'a'.

  | ArrowSkol           -- An arrow form (see TcArrows)

  | IPSkol [HsIPName]   -- Binding site of an implicit parameter

  | RuleSkol RuleName   -- The LHS of a RULE

  | InferSkol [(Name,TcType)]
                        -- We have inferred a type for these (mutually-recursivive)
                        -- polymorphic Ids, and are now checking that their RHS
                        -- constraints are satisfied.

  | BracketSkol         -- Template Haskell bracket

  | UnifyForAllSkol     -- We are unifying two for-all types
       TcType           -- The instantiated type *inside* the forall

  | TyConSkol TyConFlavour Name  -- bound in a type declaration of the given flavour

  | DataConSkol Name    -- bound as an existential in a Haskell98 datacon decl or
                        -- as any variable in a GADT datacon decl

  | ReifySkol           -- Bound during Template Haskell reification

  | QuantCtxtSkol       -- Quantified context, e.g.
                        --   f :: forall c. (forall a. c a => c [a]) => blah

  | UnkSkol             -- Unhelpful info (until I improve it)

instance Outputable SkolemInfo where
  ppr = pprSkolInfo

pprSkolInfo :: SkolemInfo -> SDoc
-- Complete the sentence "is a rigid type variable bound by..."
pprSkolInfo (SigSkol cx ty _) = pprSigSkolInfo cx ty
pprSkolInfo (SigTypeSkol cx)  = pprUserTypeCtxt cx
pprSkolInfo (ForAllSkol doc)  = quotes doc
pprSkolInfo (IPSkol ips)      = text "the implicit-parameter binding" <> plural ips <+> text "for"
                                 <+> pprWithCommas ppr ips
pprSkolInfo (DerivSkol pred)  = text "the deriving clause for" <+> quotes (ppr pred)
pprSkolInfo InstSkol          = text "the instance declaration"
pprSkolInfo (InstSC n)        = text "the instance declaration" <> whenPprDebug (parens (ppr n))
pprSkolInfo FamInstSkol       = text "a family instance declaration"
pprSkolInfo BracketSkol       = text "a Template Haskell bracket"
pprSkolInfo (RuleSkol name)   = text "the RULE" <+> pprRuleName name
pprSkolInfo ArrowSkol         = text "an arrow form"
pprSkolInfo (PatSkol cl mc)   = sep [ pprPatSkolInfo cl
                                    , text "in" <+> pprMatchContext mc ]
pprSkolInfo (InferSkol ids)   = hang (text "the inferred type" <> plural ids <+> text "of")
                                   2 (vcat [ ppr name <+> dcolon <+> ppr ty
                                                   | (name,ty) <- ids ])
pprSkolInfo (UnifyForAllSkol ty) = text "the type" <+> ppr ty
pprSkolInfo (TyConSkol flav name) = text "the" <+> ppr flav <+> text "declaration for" <+> quotes (ppr name)
pprSkolInfo (DataConSkol name)= text "the data constructor" <+> quotes (ppr name)
pprSkolInfo ReifySkol         = text "the type being reified"

pprSkolInfo (QuantCtxtSkol {}) = text "a quantified context"

-- UnkSkol
-- For type variables the others are dealt with by pprSkolTvBinding.
-- For Insts, these cases should not happen
pprSkolInfo UnkSkol = WARN( True, text "pprSkolInfo: UnkSkol" ) text "UnkSkol"

pprSigSkolInfo :: UserTypeCtxt -> TcType -> SDoc
-- The type is already tidied
pprSigSkolInfo ctxt ty
  = case ctxt of
       FunSigCtxt f _ -> vcat [ text "the type signature for:"
                              , nest 2 (pprPrefixOcc f <+> dcolon <+> ppr ty) ]
       PatSynCtxt {}  -> pprUserTypeCtxt ctxt  -- See Note [Skolem info for pattern synonyms]
       _              -> vcat [ pprUserTypeCtxt ctxt <> colon
                              , nest 2 (ppr ty) ]

pprPatSkolInfo :: ConLike -> SDoc
pprPatSkolInfo (RealDataCon dc)
  = sep [ text "a pattern with constructor:"
        , nest 2 $ ppr dc <+> dcolon
          <+> pprType (dataConUserType dc) <> comma ]
          -- pprType prints forall's regardless of -fprint-explicit-foralls
          -- which is what we want here, since we might be saying
          -- type variable 't' is bound by ...

pprPatSkolInfo (PatSynCon ps)
  = sep [ text "a pattern with pattern synonym:"
        , nest 2 $ ppr ps <+> dcolon
                   <+> pprPatSynType ps <> comma ]

{- Note [Skolem info for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For pattern synonym SkolemInfo we have
   SigSkol (PatSynCtxt p) ty _
but the type 'ty' is not very helpful.  The full pattern-synonym type
has the provided and required pieces, which it is inconvenient to
record and display here. So we simply don't display the type at all,
contenting outselves with just the name of the pattern synonym, which
is fine.  We could do more, but it doesn't seem worth it.

Note [SigSkol SkolemInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we (deeply) skolemise a type
   f :: forall a. a -> forall b. b -> a
Then we'll instantiate [a :-> a', b :-> b'], and with the instantiated
      a' -> b' -> a.
But when, in an error message, we report that "b is a rigid type
variable bound by the type signature for f", we want to show the foralls
in the right place.  So we proceed as follows:

* In SigSkol we record
    - the original signature forall a. a -> forall b. b -> a
    - the instantiation mapping [a :-> a', b :-> b']

* Then when tidying in TcMType.tidySkolemInfo, we first tidy a' to
  whatever it tidies to, say a''; and then we walk over the type
  replacing the binder a by the tidied version a'', to give
       forall a''. a'' -> forall b''. b'' -> a''
  We need to do this under function arrows, to match what deeplySkolemise
  does.

* Typically a'' will have a nice pretty name like "a", but the point is
  that the foral-bound variables of the signature we report line up with
  the instantiated skolems lying  around in other types.


************************************************************************
*                                                                      *
            CtOrigin
*                                                                      *
************************************************************************
-}

data CtOrigin
  = GivenOrigin SkolemInfo

  -- All the others are for *wanted* constraints
  | OccurrenceOf Name              -- Occurrence of an overloaded identifier
  | OccurrenceOfRecSel RdrName     -- Occurrence of a record selector
  | AppOrigin                      -- An application of some kind

  | SpecPragOrigin UserTypeCtxt    -- Specialisation pragma for
                                   -- function or instance

  | TypeEqOrigin { uo_actual   :: TcType
                 , uo_expected :: TcType
                 , uo_thing    :: Maybe SDoc
                       -- ^ The thing that has type "actual"
                 , uo_visible  :: Bool
                       -- ^ Is at least one of the three elements above visible?
                       -- (Errors from the polymorphic subsumption check are considered
                       -- visible.) Only used for prioritizing error messages.
                 }

  | KindEqOrigin  -- See Note [Equalities with incompatible kinds] in TcCanonical.
      TcType (Maybe TcType)     -- A kind equality arising from unifying these two types
      CtOrigin                  -- originally arising from this
      (Maybe TypeOrKind)        -- the level of the eq this arises from

  | IPOccOrigin  HsIPName       -- Occurrence of an implicit parameter
  | OverLabelOrigin FastString  -- Occurrence of an overloaded label

  | LiteralOrigin (HsOverLit GhcRn)     -- Occurrence of a literal
  | NegateOrigin                        -- Occurrence of syntactic negation

  | ArithSeqOrigin (ArithSeqInfo GhcRn) -- [x..], [x..y] etc
  | AssocFamPatOrigin   -- When matching the patterns of an associated
                        -- family instance with that of its parent class
  | SectionOrigin
  | TupleOrigin         -- (..,..)
  | ExprSigOrigin       -- e :: ty
  | PatSigOrigin        -- p :: ty
  | PatOrigin           -- Instantiating a polytyped pattern at a constructor
  | ProvCtxtOrigin      -- The "provided" context of a pattern synonym signature
        (PatSynBind GhcRn GhcRn) -- Information about the pattern synonym, in
                                 -- particular the name and the right-hand side
  | RecordUpdOrigin
  | ViewPatOrigin

  | ScOrigin TypeSize   -- Typechecking superclasses of an instance declaration
                        -- If the instance head is C ty1 .. tyn
                        --    then TypeSize = sizeTypes [ty1, .., tyn]
                        -- See Note [Solving superclass constraints] in TcInstDcls

  | DerivClauseOrigin   -- Typechecking a deriving clause (as opposed to
                        -- standalone deriving).
  | DerivOriginDC DataCon Int Bool
      -- Checking constraints arising from this data con and field index. The
      -- Bool argument in DerivOriginDC and DerivOriginCoerce is True if
      -- standalong deriving (with a wildcard constraint) is being used. This
      -- is used to inform error messages on how to recommended fixes (e.g., if
      -- the argument is True, then don't recommend "use standalone deriving",
      -- but rather "fill in the wildcard constraint yourself").
      -- See Note [Inferring the instance context] in TcDerivInfer
  | DerivOriginCoerce Id Type Type Bool
                        -- DerivOriginCoerce id ty1 ty2: Trying to coerce class method `id` from
                        -- `ty1` to `ty2`.
  | StandAloneDerivOrigin -- Typechecking stand-alone deriving. Useful for
                          -- constraints coming from a wildcard constraint,
                          -- e.g., deriving instance _ => Eq (Foo a)
                          -- See Note [Inferring the instance context]
                          -- in TcDerivInfer
  | DefaultOrigin       -- Typechecking a default decl
  | DoOrigin            -- Arising from a do expression
  | DoPatOrigin (LPat GhcRn) -- Arising from a failable pattern in
                             -- a do expression
  | MCompOrigin         -- Arising from a monad comprehension
  | MCompPatOrigin (LPat GhcRn) -- Arising from a failable pattern in a
                                -- monad comprehension
  | IfOrigin            -- Arising from an if statement
  | ProcOrigin          -- Arising from a proc expression
  | AnnOrigin           -- An annotation

  | FunDepOrigin1       -- A functional dependency from combining
        PredType CtLoc      -- This constraint arising from ...
        PredType CtLoc      -- and this constraint arising from ...

  | FunDepOrigin2       -- A functional dependency from combining
        PredType CtOrigin   -- This constraint arising from ...
        PredType SrcSpan    -- and this top-level instance
        -- We only need a CtOrigin on the first, because the location
        -- is pinned on the entire error message

  | HoleOrigin
  | UnboundOccurrenceOf OccName
  | ListOrigin          -- An overloaded list
  | StaticOrigin        -- A static form
  | FailablePattern (LPat GhcTcId) -- A failable pattern in do-notation for the
                                   -- MonadFail Proposal (MFP). Obsolete when
                                   -- actual desugaring to MonadFail.fail is
                                   -- live.
  | Shouldn'tHappenOrigin String
                            -- the user should never see this one,
                            -- unless ImpredicativeTypes is on, where all
                            -- bets are off
  | InstProvidedOrigin Module ClsInst
        -- Skolem variable arose when we were testing if an instance
        -- is solvable or not.

-- | Flag to see whether we're type-checking terms or kind-checking types
data TypeOrKind = TypeLevel | KindLevel
  deriving Eq

instance Outputable TypeOrKind where
  ppr TypeLevel = text "TypeLevel"
  ppr KindLevel = text "KindLevel"

isTypeLevel :: TypeOrKind -> Bool
isTypeLevel TypeLevel = True
isTypeLevel KindLevel = False

isKindLevel :: TypeOrKind -> Bool
isKindLevel TypeLevel = False
isKindLevel KindLevel = True

-- An origin is visible if the place where the constraint arises is manifest
-- in user code. Currently, all origins are visible except for invisible
-- TypeEqOrigins. This is used when choosing which error of
-- several to report
isVisibleOrigin :: CtOrigin -> Bool
isVisibleOrigin (TypeEqOrigin { uo_visible = vis }) = vis
isVisibleOrigin (KindEqOrigin _ _ sub_orig _)       = isVisibleOrigin sub_orig
isVisibleOrigin _                                   = True

-- Converts a visible origin to an invisible one, if possible. Currently,
-- this works only for TypeEqOrigin
toInvisibleOrigin :: CtOrigin -> CtOrigin
toInvisibleOrigin orig@(TypeEqOrigin {}) = orig { uo_visible = False }
toInvisibleOrigin orig                   = orig

instance Outputable CtOrigin where
  ppr = pprCtOrigin

ctoHerald :: SDoc
ctoHerald = text "arising from"

-- | Extract a suitable CtOrigin from a HsExpr
lexprCtOrigin :: LHsExpr GhcRn -> CtOrigin
lexprCtOrigin (L _ e) = exprCtOrigin e

exprCtOrigin :: HsExpr GhcRn -> CtOrigin
exprCtOrigin (HsVar _ (L _ name)) = OccurrenceOf name
exprCtOrigin (HsUnboundVar _ uv)  = UnboundOccurrenceOf (unboundVarOcc uv)
exprCtOrigin (HsConLikeOut {})    = panic "exprCtOrigin HsConLikeOut"
exprCtOrigin (HsRecFld _ f)    = OccurrenceOfRecSel (rdrNameAmbiguousFieldOcc f)
exprCtOrigin (HsOverLabel _ _ l)  = OverLabelOrigin l
exprCtOrigin (HsIPVar _ ip)       = IPOccOrigin ip
exprCtOrigin (HsOverLit _ lit)    = LiteralOrigin lit
exprCtOrigin (HsLit {})           = Shouldn'tHappenOrigin "concrete literal"
exprCtOrigin (HsLam _ matches)    = matchesCtOrigin matches
exprCtOrigin (HsLamCase _ ms)     = matchesCtOrigin ms
exprCtOrigin (HsApp _ e1 _)       = lexprCtOrigin e1
exprCtOrigin (HsAppType _ e1 _)   = lexprCtOrigin e1
exprCtOrigin (OpApp _ _ op _)     = lexprCtOrigin op
exprCtOrigin (NegApp _ e _)       = lexprCtOrigin e
exprCtOrigin (HsPar _ e)          = lexprCtOrigin e
exprCtOrigin (SectionL _ _ _)     = SectionOrigin
exprCtOrigin (SectionR _ _ _)     = SectionOrigin
exprCtOrigin (ExplicitTuple {})   = Shouldn'tHappenOrigin "explicit tuple"
exprCtOrigin ExplicitSum{}        = Shouldn'tHappenOrigin "explicit sum"
exprCtOrigin (HsCase _ _ matches) = matchesCtOrigin matches
exprCtOrigin (HsIf _ (Just syn) _ _ _) = exprCtOrigin (syn_expr syn)
exprCtOrigin (HsIf {})           = Shouldn'tHappenOrigin "if expression"
exprCtOrigin (HsMultiIf _ rhs)   = lGRHSCtOrigin rhs
exprCtOrigin (HsLet _ _ e)       = lexprCtOrigin e
exprCtOrigin (HsDo {})           = DoOrigin
exprCtOrigin (ExplicitList {})   = Shouldn'tHappenOrigin "list"
exprCtOrigin (RecordCon {})      = Shouldn'tHappenOrigin "record construction"
exprCtOrigin (RecordUpd {})      = Shouldn'tHappenOrigin "record update"
exprCtOrigin (ExprWithTySig {})  = ExprSigOrigin
exprCtOrigin (ArithSeq {})       = Shouldn'tHappenOrigin "arithmetic sequence"
exprCtOrigin (HsSCC _ _ _ e)     = lexprCtOrigin e
exprCtOrigin (HsCoreAnn _ _ _ e) = lexprCtOrigin e
exprCtOrigin (HsBracket {})      = Shouldn'tHappenOrigin "TH bracket"
exprCtOrigin (HsRnBracketOut {})= Shouldn'tHappenOrigin "HsRnBracketOut"
exprCtOrigin (HsTcBracketOut {})= panic "exprCtOrigin HsTcBracketOut"
exprCtOrigin (HsSpliceE {})      = Shouldn'tHappenOrigin "TH splice"
exprCtOrigin (HsProc {})         = Shouldn'tHappenOrigin "proc"
exprCtOrigin (HsStatic {})       = Shouldn'tHappenOrigin "static expression"
exprCtOrigin (HsArrApp {})       = panic "exprCtOrigin HsArrApp"
exprCtOrigin (HsArrForm {})      = panic "exprCtOrigin HsArrForm"
exprCtOrigin (HsTick _ _ e)           = lexprCtOrigin e
exprCtOrigin (HsBinTick _ _ _ e)      = lexprCtOrigin e
exprCtOrigin (HsTickPragma _ _ _ _ e) = lexprCtOrigin e
exprCtOrigin (EWildPat {})      = panic "exprCtOrigin EWildPat"
exprCtOrigin (EAsPat {})        = panic "exprCtOrigin EAsPat"
exprCtOrigin (EViewPat {})      = panic "exprCtOrigin EViewPat"
exprCtOrigin (ELazyPat {})      = panic "exprCtOrigin ELazyPat"
exprCtOrigin (HsWrap {})        = panic "exprCtOrigin HsWrap"
exprCtOrigin (XExpr {})         = panic "exprCtOrigin XExpr"

-- | Extract a suitable CtOrigin from a MatchGroup
matchesCtOrigin :: MatchGroup GhcRn (LHsExpr GhcRn) -> CtOrigin
matchesCtOrigin (MG { mg_alts = alts })
  | L _ [L _ match] <- alts
  , Match { m_grhss = grhss } <- match
  = grhssCtOrigin grhss

  | otherwise
  = Shouldn'tHappenOrigin "multi-way match"
matchesCtOrigin (XMatchGroup{}) = panic "matchesCtOrigin"

-- | Extract a suitable CtOrigin from guarded RHSs
grhssCtOrigin :: GRHSs GhcRn (LHsExpr GhcRn) -> CtOrigin
grhssCtOrigin (GRHSs { grhssGRHSs = lgrhss }) = lGRHSCtOrigin lgrhss
grhssCtOrigin (XGRHSs _) = panic "grhssCtOrigin"

-- | Extract a suitable CtOrigin from a list of guarded RHSs
lGRHSCtOrigin :: [LGRHS GhcRn (LHsExpr GhcRn)] -> CtOrigin
lGRHSCtOrigin [L _ (GRHS _ _ (L _ e))] = exprCtOrigin e
lGRHSCtOrigin [L _ (XGRHS _)] = panic "lGRHSCtOrigin"
lGRHSCtOrigin _ = Shouldn'tHappenOrigin "multi-way GRHS"

pprCtLoc :: CtLoc -> SDoc
-- "arising from ... at ..."
-- Not an instance of Outputable because of the "arising from" prefix
pprCtLoc (CtLoc { ctl_origin = o, ctl_env = lcl})
  = sep [ pprCtOrigin o
        , text "at" <+> ppr (tcl_loc lcl)]

pprCtOrigin :: CtOrigin -> SDoc
-- "arising from ..."
-- Not an instance of Outputable because of the "arising from" prefix
pprCtOrigin (GivenOrigin sk) = ctoHerald <+> ppr sk

pprCtOrigin (SpecPragOrigin ctxt)
  = case ctxt of
       FunSigCtxt n _ -> text "a SPECIALISE pragma for" <+> quotes (ppr n)
       SpecInstCtxt   -> text "a SPECIALISE INSTANCE pragma"
       _              -> text "a SPECIALISE pragma"  -- Never happens I think

pprCtOrigin (FunDepOrigin1 pred1 loc1 pred2 loc2)
  = hang (ctoHerald <+> text "a functional dependency between constraints:")
       2 (vcat [ hang (quotes (ppr pred1)) 2 (pprCtLoc loc1)
               , hang (quotes (ppr pred2)) 2 (pprCtLoc loc2) ])

pprCtOrigin (FunDepOrigin2 pred1 orig1 pred2 loc2)
  = hang (ctoHerald <+> text "a functional dependency between:")
       2 (vcat [ hang (text "constraint" <+> quotes (ppr pred1))
                    2 (pprCtOrigin orig1 )
               , hang (text "instance" <+> quotes (ppr pred2))
                    2 (text "at" <+> ppr loc2) ])

pprCtOrigin (KindEqOrigin t1 (Just t2) _ _)
  = hang (ctoHerald <+> text "a kind equality arising from")
       2 (sep [ppr t1, char '~', ppr t2])

pprCtOrigin AssocFamPatOrigin
  = text "when matching a family LHS with its class instance head"

pprCtOrigin (KindEqOrigin t1 Nothing _ _)
  = hang (ctoHerald <+> text "a kind equality when matching")
       2 (ppr t1)

pprCtOrigin (UnboundOccurrenceOf name)
  = ctoHerald <+> text "an undeclared identifier" <+> quotes (ppr name)

pprCtOrigin (DerivOriginDC dc n _)
  = hang (ctoHerald <+> text "the" <+> speakNth n
          <+> text "field of" <+> quotes (ppr dc))
       2 (parens (text "type" <+> quotes (ppr ty)))
  where
    ty = dataConOrigArgTys dc !! (n-1)

pprCtOrigin (DerivOriginCoerce meth ty1 ty2 _)
  = hang (ctoHerald <+> text "the coercion of the method" <+> quotes (ppr meth))
       2 (sep [ text "from type" <+> quotes (ppr ty1)
              , nest 2 $ text "to type" <+> quotes (ppr ty2) ])

pprCtOrigin (DoPatOrigin pat)
    = ctoHerald <+> text "a do statement"
      $$
      text "with the failable pattern" <+> quotes (ppr pat)

pprCtOrigin (MCompPatOrigin pat)
    = ctoHerald <+> hsep [ text "the failable pattern"
           , quotes (ppr pat)
           , text "in a statement in a monad comprehension" ]
pprCtOrigin (FailablePattern pat)
    = ctoHerald <+> text "the failable pattern" <+> quotes (ppr pat)
      $$
      text "(this will become an error in a future GHC release)"

pprCtOrigin (Shouldn'tHappenOrigin note)
  = sdocWithDynFlags $ \dflags ->
    if xopt LangExt.ImpredicativeTypes dflags
    then text "a situation created by impredicative types"
    else
    vcat [ text "<< This should not appear in error messages. If you see this"
         , text "in an error message, please report a bug mentioning" <+> quotes (text note) <+> text "at"
         , text "https://ghc.haskell.org/trac/ghc/wiki/ReportABug >>" ]

pprCtOrigin (ProvCtxtOrigin PSB{ psb_id = (L _ name) })
  = hang (ctoHerald <+> text "the \"provided\" constraints claimed by")
       2 (text "the signature of" <+> quotes (ppr name))

pprCtOrigin (InstProvidedOrigin mod cls_inst)
  = vcat [ text "arising when attempting to show that"
         , ppr cls_inst
         , text "is provided by" <+> quotes (ppr mod)]

pprCtOrigin simple_origin
  = ctoHerald <+> pprCtO simple_origin

-- | Short one-liners
pprCtO :: CtOrigin -> SDoc
pprCtO (OccurrenceOf name)   = hsep [text "a use of", quotes (ppr name)]
pprCtO (OccurrenceOfRecSel name) = hsep [text "a use of", quotes (ppr name)]
pprCtO AppOrigin             = text "an application"
pprCtO (IPOccOrigin name)    = hsep [text "a use of implicit parameter", quotes (ppr name)]
pprCtO (OverLabelOrigin l)   = hsep [text "the overloaded label"
                                    ,quotes (char '#' <> ppr l)]
pprCtO RecordUpdOrigin       = text "a record update"
pprCtO ExprSigOrigin         = text "an expression type signature"
pprCtO PatSigOrigin          = text "a pattern type signature"
pprCtO PatOrigin             = text "a pattern"
pprCtO ViewPatOrigin         = text "a view pattern"
pprCtO IfOrigin              = text "an if expression"
pprCtO (LiteralOrigin lit)   = hsep [text "the literal", quotes (ppr lit)]
pprCtO (ArithSeqOrigin seq)  = hsep [text "the arithmetic sequence", quotes (ppr seq)]
pprCtO SectionOrigin         = text "an operator section"
pprCtO AssocFamPatOrigin     = text "the LHS of a famly instance"
pprCtO TupleOrigin           = text "a tuple"
pprCtO NegateOrigin          = text "a use of syntactic negation"
pprCtO (ScOrigin n)          = text "the superclasses of an instance declaration"
                               <> whenPprDebug (parens (ppr n))
pprCtO DerivClauseOrigin     = text "the 'deriving' clause of a data type declaration"
pprCtO StandAloneDerivOrigin = text "a 'deriving' declaration"
pprCtO DefaultOrigin         = text "a 'default' declaration"
pprCtO DoOrigin              = text "a do statement"
pprCtO MCompOrigin           = text "a statement in a monad comprehension"
pprCtO ProcOrigin            = text "a proc expression"
pprCtO (TypeEqOrigin t1 t2 _ _)= text "a type equality" <+> sep [ppr t1, char '~', ppr t2]
pprCtO AnnOrigin             = text "an annotation"
pprCtO HoleOrigin            = text "a use of" <+> quotes (text "_")
pprCtO ListOrigin            = text "an overloaded list"
pprCtO StaticOrigin          = text "a static form"
pprCtO _                     = panic "pprCtOrigin"

{-
Constraint Solver Plugins
-------------------------
-}

type TcPluginSolver = [Ct]    -- given
                   -> [Ct]    -- derived
                   -> [Ct]    -- wanted
                   -> TcPluginM TcPluginResult

newtype TcPluginM a = TcPluginM (EvBindsVar -> TcM a)

instance Functor TcPluginM where
  fmap = liftM

instance Applicative TcPluginM where
  pure x = TcPluginM (const $ pure x)
  (<*>) = ap

instance Monad TcPluginM where
#if !MIN_VERSION_base(4,13,0)
  fail = MonadFail.fail
#endif
  TcPluginM m >>= k =
    TcPluginM (\ ev -> do a <- m ev
                          runTcPluginM (k a) ev)

instance MonadFail.MonadFail TcPluginM where
  fail x   = TcPluginM (const $ fail x)

runTcPluginM :: TcPluginM a -> EvBindsVar -> TcM a
runTcPluginM (TcPluginM m) = m

-- | This function provides an escape for direct access to
-- the 'TcM` monad.  It should not be used lightly, and
-- the provided 'TcPluginM' API should be favoured instead.
unsafeTcPluginTcM :: TcM a -> TcPluginM a
unsafeTcPluginTcM = TcPluginM . const

-- | Access the 'EvBindsVar' carried by the 'TcPluginM' during
-- constraint solving.  Returns 'Nothing' if invoked during
-- 'tcPluginInit' or 'tcPluginStop'.
getEvBindsTcPluginM :: TcPluginM EvBindsVar
getEvBindsTcPluginM = TcPluginM return


data TcPlugin = forall s. TcPlugin
  { tcPluginInit  :: TcPluginM s
    -- ^ Initialize plugin, when entering type-checker.

  , tcPluginSolve :: s -> TcPluginSolver
    -- ^ Solve some constraints.
    -- TODO: WRITE MORE DETAILS ON HOW THIS WORKS.

  , tcPluginStop  :: s -> TcPluginM ()
   -- ^ Clean up after the plugin, when exiting the type-checker.
  }

data TcPluginResult
  = TcPluginContradiction [Ct]
    -- ^ The plugin found a contradiction.
    -- The returned constraints are removed from the inert set,
    -- and recorded as insoluble.

  | TcPluginOk [(EvTerm,Ct)] [Ct]
    -- ^ The first field is for constraints that were solved.
    -- These are removed from the inert set,
    -- and the evidence for them is recorded.
    -- The second field contains new work, that should be processed by
    -- the constraint solver.

{- *********************************************************************
*                                                                      *
                        Role annotations
*                                                                      *
********************************************************************* -}

type RoleAnnotEnv = NameEnv (LRoleAnnotDecl GhcRn)

mkRoleAnnotEnv :: [LRoleAnnotDecl GhcRn] -> RoleAnnotEnv
mkRoleAnnotEnv role_annot_decls
 = mkNameEnv [ (name, ra_decl)
             | ra_decl <- role_annot_decls
             , let name = roleAnnotDeclName (unLoc ra_decl)
             , not (isUnboundName name) ]
       -- Some of the role annots will be unbound;
       -- we don't wish to include these

emptyRoleAnnotEnv :: RoleAnnotEnv
emptyRoleAnnotEnv = emptyNameEnv

lookupRoleAnnot :: RoleAnnotEnv -> Name -> Maybe (LRoleAnnotDecl GhcRn)
lookupRoleAnnot = lookupNameEnv

getRoleAnnots :: [Name] -> RoleAnnotEnv
              -> ([LRoleAnnotDecl GhcRn], RoleAnnotEnv)
getRoleAnnots bndrs role_env
  = ( mapMaybe (lookupRoleAnnot role_env) bndrs
    , delListFromNameEnv role_env bndrs )
