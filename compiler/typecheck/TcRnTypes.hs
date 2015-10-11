{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-2002


Various types used during typechecking, please see TcRnMonad as well for
operations on these types. You probably want to import it, instead of this
module.

All the monads exported here are built on top of the same IOEnv monad. The
monad functions like a Reader monad in the way it passes the environment
around. This is done to allow the environment to be manipulated in a stack
like fashion when entering expressions... ect.

For state that is global and should be returned at the end (e.g not part
of the stack mechanism), you should use an TcRef (= IORef) to store them.
-}

{-# LANGUAGE CPP, ExistentialQuantification, GeneralizedNewtypeDeriving #-}

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
        ErrCtxt, RecFieldEnv(..),
        ImportAvails(..), emptyImportAvails, plusImportAvails,
        WhereFrom(..), mkModDeps,

        -- Typechecker types
        TcTypeEnv, TcIdBinderStack, TcIdBinder(..),
        TcTyThing(..), PromotionErr(..),
        SelfBootInfo(..),
        pprTcTyThingCategory, pprPECategory,

        -- Desugaring types
        DsM, DsLclEnv(..), DsGblEnv(..), PArrBuiltin(..),
        DsMetaEnv, DsMetaVal(..),

        -- Template Haskell
        ThStage(..), PendingStuff(..), topStage, topAnnStage, topSpliceStage,
        ThLevel, impLevel, outerLevel, thLevel,

        -- Arrows
        ArrowCtxt(..),

        -- Canonical constraints
        Xi, Ct(..), Cts, emptyCts, andCts, andManyCts, pprCts,
        singleCt, listToCts, ctsElts, consCts, snocCts, extendCtsList,
        isEmptyCts, isCTyEqCan, isCFunEqCan,
        isCDictCan_Maybe, isCFunEqCan_maybe,
        isCIrredEvCan, isCNonCanonical, isWantedCt, isDerivedCt,
        isGivenCt, isHoleCt, isOutOfScopeCt, isExprHoleCt, isTypeHoleCt,
        ctEvidence, ctLoc, setCtLoc, ctPred, ctFlavour, ctEqRel, ctOrigin,
        mkNonCanonical, mkNonCanonicalCt,
        ctEvPred, ctEvLoc, ctEvOrigin, ctEvEqRel,
        ctEvTerm, ctEvCoercion, ctEvId,

        WantedConstraints(..), insolubleWC, emptyWC, isEmptyWC,
        andWC, unionsWC, addSimples, addImplics, mkSimpleWC, addInsols,
        dropDerivedWC, dropDerivedSimples, dropDerivedInsols,
        isDroppableDerivedLoc, insolubleImplic, trulyInsoluble,

        Implication(..), ImplicStatus(..), isInsolubleStatus,
        SubGoalDepth, initialSubGoalDepth,
        bumpSubGoalDepth, subGoalDepthExceeded,
        CtLoc(..), ctLocSpan, ctLocEnv, ctLocLevel, ctLocOrigin,
        ctLocDepth, bumpCtLocDepth,
        setCtLocOrigin, setCtLocEnv, setCtLocSpan,
        CtOrigin(..), pprCtOrigin, pprCtLoc,
        pushErrCtxt, pushErrCtxtSameOrigin,

        SkolemInfo(..),

        CtEvidence(..),
        mkGivenLoc,
        isWanted, isGiven, isDerived,
        ctEvRole,

        -- Constraint solver plugins
        TcPlugin(..), TcPluginResult(..), TcPluginSolver,
        TcPluginM, runTcPluginM, unsafeTcPluginTcM,
        getEvBindsTcPluginM_maybe,

        CtFlavour(..), ctEvFlavour,
        CtFlavourRole, ctEvFlavourRole, ctFlavourRole,
        eqCanRewrite, eqCanRewriteFR, canDischarge, canDischargeFR,

        -- Pretty printing
        pprEvVarTheta,
        pprEvVars, pprEvVarWithType,

        -- Misc other types
        TcId, TcIdSet, HoleSort(..)

  ) where

#include "HsVersions.h"

import HsSyn
import CoreSyn
import HscTypes
import TcEvidence
import Type
import CoAxiom  ( Role )
import Class    ( Class )
import TyCon    ( TyCon )
import ConLike  ( ConLike(..) )
import DataCon  ( DataCon, dataConUserType, dataConOrigArgTys )
import PatSyn   ( PatSyn, patSynType )
import TcType
import Annotations
import InstEnv
import FamInstEnv
import IOEnv
import RdrName
import Name
import NameEnv
import NameSet
import Avail
import Var
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
import GHC.Fingerprint

import Data.Set (Set)
import Control.Monad (ap, liftM)

#ifdef GHCI
import Data.Map      ( Map )
import Data.Dynamic  ( Dynamic )
import Data.Typeable ( TypeRep )

import qualified Language.Haskell.TH as TH
#endif

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

-- We 'stack' these envs through the Reader like monad infastructure
-- as we move into an expression (although the change is focused in
-- the lcl type).
data Env gbl lcl
  = Env {
        env_top  :: HscEnv,  -- Top-level stuff that never changes
                             -- Includes all info about imported things

        env_us   :: {-# UNPACK #-} !(IORef UniqSupply),
                             -- Unique supply for local varibles

        env_gbl  :: gbl,     -- Info about things defined at the top level
                             -- of the module being compiled

        env_lcl  :: lcl      -- Nested stuff; changes as we go into
    }

instance ContainsDynFlags (Env gbl lcl) where
    extractDynFlags env = hsc_dflags (env_top env)
    replaceDynFlags env dflags
        = env {env_top = replaceDynFlags (env_top env) dflags}

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
        -- The type environment for the module being compiled,
        -- in case the interface refers back to it via a reference that
        -- was originally a hi-boot file.
        -- We need the module name so we can test when it's appropriate
        -- to look in this env.
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
        if_mod :: Module,

        -- The field is used only for error reporting
        -- if (say) there's a Lint error in it
        if_loc :: SDoc,
                -- Where the interface came from:
                --      .hi file, or GHCi state, or ext core
                -- plus which bit is currently being examined

        if_tv_env  :: UniqFM TyVar,     -- Nested tyvar bindings
                                        -- (and coercions)
        if_id_env  :: UniqFM Id         -- Nested id binding
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

-- If '-XParallelArrays' is given, the desugarer populates this table with the corresponding
-- variables found in 'Data.Array.Parallel'.
--
data PArrBuiltin
        = PArrBuiltin
        { lengthPVar         :: Var     -- ^ lengthP
        , replicatePVar      :: Var     -- ^ replicateP
        , singletonPVar      :: Var     -- ^ singletonP
        , mapPVar            :: Var     -- ^ mapP
        , filterPVar         :: Var     -- ^ filterP
        , zipPVar            :: Var     -- ^ zipP
        , crossMapPVar       :: Var     -- ^ crossMapP
        , indexPVar          :: Var     -- ^ (!:)
        , emptyPVar          :: Var     -- ^ emptyP
        , appPVar            :: Var     -- ^ (+:+)
        , enumFromToPVar     :: Var     -- ^ enumFromToP
        , enumFromThenToPVar :: Var     -- ^ enumFromThenToP
        }

data DsGblEnv
        = DsGblEnv
        { ds_mod          :: Module             -- For SCC profiling
        , ds_fam_inst_env :: FamInstEnv         -- Like tcg_fam_inst_env
        , ds_unqual  :: PrintUnqualified
        , ds_msgs    :: IORef Messages          -- Warning messages
        , ds_if_env  :: (IfGblEnv, IfLclEnv)    -- Used for looking up global,
                                                -- possibly-imported things
        , ds_dph_env :: GlobalRdrEnv            -- exported entities of 'Data.Array.Parallel.Prim'
                                                -- iff '-fvectorise' flag was given as well as
                                                -- exported entities of 'Data.Array.Parallel' iff
                                                -- '-XParallelArrays' was given; otherwise, empty
        , ds_parr_bi :: PArrBuiltin             -- desugarar names for '-XParallelArrays'
        , ds_static_binds :: IORef [(Fingerprint, (Id,CoreExpr))]
          -- ^ Bindings resulted from floating static forms
        }

instance ContainsModule DsGblEnv where
    extractModule = ds_mod

data DsLclEnv = DsLclEnv {
        dsl_meta    :: DsMetaEnv,        -- Template Haskell bindings
        dsl_loc     :: SrcSpan           -- to put in pattern-matching error msgs
     }

-- Inside [| |] brackets, the desugarer looks
-- up variables in the DsMetaEnv
type DsMetaEnv = NameEnv DsMetaVal

data DsMetaVal
   = DsBound Id         -- Bound by a pattern inside the [| |].
                        -- Will be dynamically alpha renamed.
                        -- The Id has type THSyntax.Var

   | DsSplice (HsExpr Id) -- These bindings are introduced by
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
        | FrontendMerge     ModIface

-- | 'TcGblEnv' describes the top-level of the module at the
-- point at which the typechecker is finished work.
-- It is this structure that is handed on to the desugarer
-- For state that needs to be updated during the typechecking
-- phase and returned at end, use a 'TcRef' (= 'IORef').
data TcGblEnv
  = TcGblEnv {
        tcg_mod     :: Module,         -- ^ Module being compiled
        tcg_src     :: HscSource,
          -- ^ What kind of module (regular Haskell, hs-boot, hsig)
        tcg_sig_of  :: Maybe Module,
          -- ^ Are we being compiled as a signature of an implementation?
        tcg_impl_rdr_env :: Maybe GlobalRdrEnv,
          -- ^ Environment used only during -sig-of for resolving top level
          -- bindings.  See Note [Signature parameters in TcGblEnv and DynFlags]

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

        tcg_inst_env     :: InstEnv,
          -- ^ Instance envt for all /home-package/ modules;
          -- Includes the dfuns in tcg_insts
        tcg_fam_inst_env :: FamInstEnv, -- ^ Ditto for family instances
        tcg_ann_env      :: AnnEnv,     -- ^ And for annotations

                -- Now a bunch of things about this module that are simply
                -- accumulated, but never consulted until the end.
                -- Nevertheless, it's convenient to accumulate them along
                -- with the rest of the info from this module.
        tcg_exports :: [AvailInfo],     -- ^ What is exported
        tcg_imports :: ImportAvails,
          -- ^ Information about what was imported from where, including
          -- things bound in this module. Also store Safe Haskell info
          -- here about transative trusted packaage requirements.

        tcg_dus :: DefUses,   -- ^ What is defined in this module and what is used.
        tcg_used_rdrnames :: TcRef (Set RdrName),
          -- See Note [Tracking unused binding and imports]

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

        tcg_dfun_n  :: TcRef OccSet,
          -- ^ Allows us to choose unique DFun names.

        -- The next fields accumulate the payload of the module
        -- The binds, rules and foreign-decl fields are collected
        -- initially in un-zonked form and are finally zonked in tcRnSrcDecls

        tcg_rn_exports :: Maybe [Located (IE Name)],
                -- Nothing <=> no explicit export list

        tcg_rn_imports :: [LImportDecl Name],
                -- Keep the renamed imports regardless.  They are not
                -- voluminous and are needed if you want to report unused imports

        tcg_rn_decls :: Maybe (HsGroup Name),
          -- ^ Renamed decls, maybe.  @Nothing@ <=> Don't retain renamed
          -- decls.

        tcg_dependent_files :: TcRef [FilePath], -- ^ dependencies from addDependentFile

#ifdef GHCI
        tcg_th_topdecls :: TcRef [LHsDecl RdrName],
        -- ^ Top-level declarations from addTopDecls

        tcg_th_topnames :: TcRef NameSet,
        -- ^ Exact names bound in top-level declarations in tcg_th_topdecls

        tcg_th_modfinalizers :: TcRef [TH.Q ()],
        -- ^ Template Haskell module finalizers

        tcg_th_state :: TcRef (Map TypeRep Dynamic),
        -- ^ Template Haskell state
#endif /* GHCI */

        tcg_ev_binds  :: Bag EvBind,        -- Top-level evidence bindings

        -- Things defined in this module, or (in GHCi)
        -- in the declarations for a single GHCi command.
        -- For the latter, see Note [The interactive package] in HscTypes
        tcg_binds     :: LHsBinds Id,        -- Value bindings in this module
        tcg_sigs      :: NameSet,            -- ...Top-level names that *lack* a signature
        tcg_imp_specs :: [LTcSpecPrag],      -- ...SPECIALISE prags for imported Ids
        tcg_warns     :: Warnings,           -- ...Warnings and deprecations
        tcg_anns      :: [Annotation],       -- ...Annotations
        tcg_tcs       :: [TyCon],            -- ...TyCons and Classes
        tcg_insts     :: [ClsInst],          -- ...Instances
        tcg_fam_insts :: [FamInst],          -- ...Family instances
        tcg_rules     :: [LRuleDecl Id],     -- ...Rules
        tcg_fords     :: [LForeignDecl Id],  -- ...Foreign import & exports
        tcg_vects     :: [LVectDecl Id],     -- ...Vectorisation declarations
        tcg_patsyns   :: [PatSyn],           -- ...Pattern synonyms

        tcg_doc_hdr   :: Maybe LHsDocString, -- ^ Maybe Haddock header docs
        tcg_hpc       :: AnyHpcUsage,        -- ^ @True@ if any part of the
                                             --  prog uses hpc instrumentation.

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

        tcg_static_wc :: TcRef WantedConstraints
          -- ^ Wanted constraints of static forms.
    }

tcVisibleOrphanMods :: TcGblEnv -> ModuleSet
tcVisibleOrphanMods tcg_env
    = mkModuleSet (tcg_mod tcg_env : imp_orphs (tcg_imports tcg_env))

-- Note [Signature parameters in TcGblEnv and DynFlags]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- When compiling signature files, we need to know which implementation
-- we've actually linked against the signature.  There are three seemingly
-- redundant places where this information is stored: in DynFlags, there
-- is sigOf, and in TcGblEnv, there is tcg_sig_of and tcg_impl_rdr_env.
-- Here's the difference between each of them:
--
-- * DynFlags.sigOf is global per invocation of GHC.  If we are compiling
--   with --make, there may be multiple signature files being compiled; in
--   which case this parameter is a map from local module name to implementing
--   Module.
--
-- * HscEnv.tcg_sig_of is global per the compilation of a single file, so
--   it is simply the result of looking up tcg_mod in the DynFlags.sigOf
--   parameter.  It's setup in TcRnMonad.initTc.  This prevents us
--   from having to repeatedly do a lookup in DynFlags.sigOf.
--
-- * HscEnv.tcg_impl_rdr_env is a RdrEnv that lets us look up names
--   according to the sig-of module.  It's setup in TcRnDriver.tcRnSignature.
--   Here is an example showing why we need this map:
--
--  module A where
--      a = True
--
--  module ASig where
--      import B
--      a :: Bool
--
--  module B where
--      b = False
--
-- When we compile ASig --sig-of main:A, the default
-- global RdrEnv (tcg_rdr_env) has an entry for b, but not for a
-- (we never imported A).  So we have to look in a different environment
-- to actually get the original name.
--
-- By the way, why do we need to do the lookup; can't we just use A:a
-- as the name directly?  Well, if A is reexporting the entity from another
-- module, then the original name needs to be the real original name:
--
--  module C where
--      a = True
--
--  module A(a) where
--      import C

instance ContainsModule TcGblEnv where
    extractModule env = tcg_mod env

data RecFieldEnv
  = RecFields (NameEnv [Name])  -- Maps a constructor name *in this module*
                                -- to the fields for that constructor
              NameSet           -- Set of all fields declared *in this module*;
                                -- used to suppress name-shadowing complaints
                                -- when using record wild cards
                                -- E.g.  let fld = e in C {..}
        -- This is used when dealing with ".." notation in record
        -- construction and pattern matching.
        -- The FieldEnv deals *only* with constructors defined in *this*
        -- module.  For imported modules, we get the same info from the
        -- TypeEnv

data SelfBootInfo
  = NoSelfBoot    -- No corresponding hi-boot file
  | SelfBoot
       { sb_mds :: ModDetails   -- There was a hi-boot file,
       , sb_tcs :: NameSet      -- defining these TyCons,
       , sb_ids :: NameSet }    -- and these Ids
  -- We need this info to compute a safe approximation to
  -- recursive loops, to avoid infinite inlinings

{-
Note [Tracking unused binding and imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

 * tcg_used_rdrnames
      Records used *imported* (not locally-defined) RdrNames
      Used only to report unused import declarations
      Notice that they are RdrNames, not Names, so we can
      tell whether the reference was qualified or unqualified, which
      is esssential in deciding whether a particular import decl
      is unnecessary.  This info isn't present in Names.


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
        tcl_th_bndrs   :: ThBindEnv,       -- Binding level of in-scope Names
                                           -- defined in this module (not imported)

        tcl_arrow_ctxt :: ArrowCtxt,       -- Arrow-notation context

        tcl_rdr :: LocalRdrEnv,         -- Local name envt
                -- Maintained during renaming, of course, but also during
                -- type checking, solely so that when renaming a Template-Haskell
                -- splice we have the right environment for the renamer.
                --
                --   Does *not* include global name envt; may shadow it
                --   Includes both ordinary variables and type variables;
                --   they are kept distinct because tyvar have a different
                --   occurrence contructor (Name.TvOcc)
                -- We still need the unsullied global name env so that
                --   we can look up record field names

        tcl_env  :: TcTypeEnv,    -- The local type environment:
                                  -- Ids and TyVars defined in this module

        tcl_bndrs :: TcIdBinderStack,   -- Used for reporting relevant bindings

        tcl_tidy :: TidyEnv,      -- Used for tidying types; contains all
                                  -- in-scope type variables (but not term variables)

        tcl_tyvars :: TcRef TcTyVarSet, -- The "global tyvars"
                        -- Namely, the in-scope TyVars bound in tcl_env,
                        -- plus the tyvars mentioned in the types of Ids bound
                        -- in tcl_lenv.
                        -- Why mutable? see notes with tcGetGlobalTyVars

        tcl_lie  :: TcRef WantedConstraints,    -- Place to accumulate type constraints
        tcl_errs :: TcRef Messages              -- Place to accumulate errors
    }

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
-- The TcIdBinderStack
---------------------------

type TcIdBinderStack = [TcIdBinder]
   -- This is a stack of locally-bound ids, innermost on top
   -- Used ony in error reporting (relevantBindings in TcError)

data TcIdBinder
  = TcIdBndr
       TcId
       TopLevelFlag    -- Tells whether the bindind is syntactically top-level
                       -- (The monomorphic Ids for a recursive group count
                       --  as not-top-level for this purpose.)

instance Outputable TcIdBinder where
   ppr (TcIdBndr id top_lvl) = ppr id <> brackets (ppr top_lvl)

---------------------------
-- Template Haskell stages and levels
---------------------------

data ThStage    -- See Note [Template Haskell state diagram] in TcSplice
  = Splice      -- Inside a top-level splice splice
                -- This code will be run *at compile time*;
                --   the result replaces the splice
                -- Binding level = 0
      Bool      -- True if in a typed splice, False otherwise

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
topAnnStage    = Splice False
topSpliceStage = Splice False

instance Outputable ThStage where
   ppr (Splice _)  = text "Splice"
   ppr Comp        = text "Comp"
   ppr (Brack s _) = text "Brack" <> parens (ppr s)

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
thLevel (Splice _)  = 0
thLevel Comp        = 1
thLevel (Brack s _) = thLevel s + 1

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

  | ATcId   {           -- Ids defined in this module; may not be fully zonked
        tct_id     :: TcId,
        tct_closed :: TopLevelFlag }   -- See Note [Bindings with closed types]

  | ATyVar  Name TcTyVar        -- The type variable to which the lexically scoped type
                                -- variable is bound. We only need the Name
                                -- for error-message purposes; it is the corresponding
                                -- Name in the domain of the envt

  | AThing  TcKind   -- Used temporarily, during kind checking, for the
                     -- tycons and clases in this recursive group
                     -- Can be a mono-kind or a poly-kind; in TcTyClsDcls see
                     -- Note [Type checking recursive type and class declarations]

  | APromotionErr PromotionErr

data PromotionErr
  = TyConPE          -- TyCon used in a kind before we are ready
                     --     data T :: T -> * where ...
  | ClassPE          -- Ditto Class

  | FamDataConPE     -- Data constructor for a data family
                     -- See Note [AFamDataCon: not promoting data family constructors] in TcRnDriver

  | RecDataConPE     -- Data constructor in a recursive loop
                     -- See Note [ARecDataCon: recusion and promoting data constructors] in TcTyClsDecls
  | NoDataKinds      -- -XDataKinds not enabled

instance Outputable TcTyThing where     -- Debugging only
   ppr (AGlobal g)      = pprTyThing g
   ppr elt@(ATcId {})   = text "Identifier" <>
                          brackets (ppr (tct_id elt) <> dcolon
                                 <> ppr (varType (tct_id elt)) <> comma
                                 <+> ppr (tct_closed elt))
   ppr (ATyVar n tv)    = text "Type variable" <+> quotes (ppr n) <+> equals <+> ppr tv
   ppr (AThing k)       = text "AThing" <+> ppr k
   ppr (APromotionErr err) = text "APromotionErr" <+> ppr err

instance Outputable PromotionErr where
  ppr ClassPE      = text "ClassPE"
  ppr TyConPE      = text "TyConPE"
  ppr FamDataConPE = text "FamDataConPE"
  ppr RecDataConPE = text "RecDataConPE"
  ppr NoDataKinds  = text "NoDataKinds"

pprTcTyThingCategory :: TcTyThing -> SDoc
pprTcTyThingCategory (AGlobal thing)    = pprTyThingCategory thing
pprTcTyThingCategory (ATyVar {})        = ptext (sLit "Type variable")
pprTcTyThingCategory (ATcId {})         = ptext (sLit "Local identifier")
pprTcTyThingCategory (AThing {})        = ptext (sLit "Kinded thing")
pprTcTyThingCategory (APromotionErr pe) = pprPECategory pe

pprPECategory :: PromotionErr -> SDoc
pprPECategory ClassPE      = ptext (sLit "Class")
pprPECategory TyConPE      = ptext (sLit "Type constructor")
pprPECategory FamDataConPE = ptext (sLit "Data constructor")
pprPECategory RecDataConPE = ptext (sLit "Data constructor")
pprPECategory NoDataKinds  = ptext (sLit "Data constructor")

{- Note [Bindings with closed types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

  f x = let g ys = map not ys
        in ...

Can we generalise 'g' under the OutsideIn algorithm?  Yes,
because all g's free variables are top-level; that is they themselves
have no free type variables, and it is the type variables in the
environment that makes things tricky for OutsideIn generalisation.

Definition:
   A variable is "closed", and has tct_closed set to TopLevel,
iff
   a) all its free variables are imported, or are let-bound with closed types
   b) generalisation is not restricted by the monomorphism restriction

Under OutsideIn we are free to generalise a closed let-binding.
This is an extension compared to the JFP paper on OutsideIn, which
used "top-level" as a proxy for "closed".  (It's not a good proxy
anyway -- the MR can make a top-level binding with a free type
variable.)

Note that:
  * A top-level binding may not be closed, if it suffers from the MR

  * A nested binding may be closed (eg 'g' in the example we started with)
    Indeed, that's the point; whether a function is defined at top level
    or nested is orthogonal to the question of whether or not it is closed

  * A binding may be non-closed because it mentions a lexically scoped
    *type variable*  Eg
        f :: forall a. blah
        f x = let g y = ...(y::a)...
-}

type ErrCtxt = (Bool, TidyEnv -> TcM (TidyEnv, MsgDoc))
        -- Monadic so that we have a chance
        -- to deal with bound type variables just before error
        -- message construction

        -- Bool:  True <=> this is a landmark context; do not
        --                 discard it when trimming for display

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
          --      = ModuleEnv [(ModuleName, Bool, SrcSpan, Bool)],
          -- ^ Domain is all directly-imported modules
          -- The 'ModuleName' is what the module was imported as, e.g. in
          -- @
          --     import Foo as Bar
          -- @
          -- it is @Bar@.
          --
          -- The 'Bool' means:
          --
          --  - @True@ => import was @import Foo ()@
          --
          --  - @False@ => import was some other form
          --
          -- Used
          --
          --   (a) to help construct the usage information in the interface
          --       file; if we import something we need to recompile if the
          --       export version changes
          --
          --   (b) to specify what child modules to initialise
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

        imp_dep_pkgs :: [UnitId],
          -- ^ Packages needed by the module being compiled, whether directly,
          -- or via other modules in this package, or via modules imported
          -- from other packages.

        imp_trust_pkgs :: [UnitId],
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
mkModDeps deps = foldl add emptyUFM deps
               where
                 add env elt@(m,_) = addToUFM env m elt

emptyImportAvails :: ImportAvails
emptyImportAvails = ImportAvails { imp_mods          = emptyModuleEnv,
                                   imp_dep_mods      = emptyUFM,
                                   imp_dep_pkgs      = [],
                                   imp_trust_pkgs    = [],
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
                   imp_dep_pkgs      = dpkgs1 `unionLists` dpkgs2,
                   imp_trust_pkgs    = tpkgs1 `unionLists` tpkgs2,
                   imp_trust_own_pkg = tself1 || tself2,
                   imp_orphs         = orphs1 `unionLists` orphs2,
                   imp_finsts        = finsts1 `unionLists` finsts2 }
  where
    plus_mod_dep (m1, boot1) (m2, boot2)
        = WARN( not (m1 == m2), (ppr m1 <+> ppr m2) $$ (ppr boot1 <+> ppr boot2) )
                -- Check mod-names match
          (m1, boot1 && boot2) -- If either side can "see" a non-hi-boot interface, use that

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
  ppr (ImportByUser is_boot) | is_boot     = ptext (sLit "{- SOURCE -}")
                             | otherwise   = empty
  ppr ImportBySystem                       = ptext (sLit "{- SYSTEM -}")
  ppr ImportByPlugin                       = ptext (sLit "{- PLUGIN -}")

{-
************************************************************************
*                                                                      *
*                       Canonical constraints                          *
*                                                                      *
*   These are the constraints the low-level simplifier works with      *
*                                                                      *
************************************************************************
-}

-- The syntax of xi types:
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
      cc_tyargs :: [Xi]        -- cc_tyargs are function-free, hence Xi
    }

  | CIrredEvCan {  -- These stand for yet-unusable predicates
      cc_ev :: CtEvidence   -- See Note [Ct/evidence invariant]
        -- The ctev_pred of the evidence is
        -- of form   (tv xi1 xi2 ... xin)
        --      or   (tv1 ~ ty2)   where the CTyEqCan  kind invariant fails
        --      or   (F tys ~ ty)  where the CFunEqCan kind invariant fails
        -- See Note [CIrredEvCan constraints]
    }

  | CTyEqCan {  -- tv ~ rhs
       -- Invariants:
       --   * See Note [Applying the inert substitution] in TcFlatten
       --   * tv not in tvs(rhs)   (occurs check)
       --   * If tv is a TauTv, then rhs has no foralls
       --       (this avoids substituting a forall for the tyvar in other types)
       --   * typeKind ty `subKind` typeKind tv
       --       See Note [Kind orientation for CTyEqCan]
       --   * rhs is not necessarily function-free,
       --       but it has no top-level function.
       --     E.g. a ~ [F b]  is fine
       --     but  a ~ F b    is not
       --   * If the equality is representational, rhs has no top-level newtype
       --     See Note [No top-level newtypes on RHS of representational
       --     equalities] in TcCanonical
       --   * If rhs is also a tv, then it is oriented to give best chance of
       --     unification happening; eg if rhs is touchable then lhs is too
      cc_ev     :: CtEvidence, -- See Note [Ct/evidence invariant]
      cc_tyvar  :: TcTyVar,
      cc_rhs    :: TcType,     -- Not necessarily function-free (hence not Xi)
                               -- See invariants above
      cc_eq_rel :: EqRel
    }

  | CFunEqCan {  -- F xis ~ fsk
       -- Invariants:
       --   * isTypeFamilyTyCon cc_fun
       --   * typeKind (F xis) = tyVarKind fsk
       --   * always Nominal role
      cc_ev     :: CtEvidence,  -- See Note [Ct/evidence invariant]
      cc_fun    :: TyCon,       -- A type function

      cc_tyargs :: [Xi],        -- cc_tyargs are function-free (hence Xi)
        -- Either under-saturated or exactly saturated
        --    *never* over-saturated (because if so
        --    we should have decomposed)

      cc_fsk    :: TcTyVar  -- [Given]  always a FlatSkol skolem
                            -- [Wanted] always a FlatMetaTv unification variable
        -- See Note [The flattening story] in TcFlatten
    }

  | CNonCanonical {        -- See Note [NonCanonical Semantics]
      cc_ev  :: CtEvidence
    }

  | CHoleCan {             -- See Note [Hole constraints]
       -- Treated as an "insoluble" constraint
       -- See Note [Insoluble constraints]
      cc_ev   :: CtEvidence,
      cc_occ  :: OccName,   -- The name of this hole
      cc_hole :: HoleSort   -- The sort of this hole (expr, type, ...)
    }

-- | Used to indicate which sort of hole we have.
data HoleSort = ExprHole  -- ^ A hole in an expression (TypedHoles)
              | TypeHole  -- ^ A hole in a type (PartialTypeSignatures)

{-
Note [Hole constraints]
~~~~~~~~~~~~~~~~~~~~~~~
CHoleCan constraints are used for two kinds of holes,
distinguished by cc_hole:

  * For holes in expressions
    e.g.   f x = g _ x

  * For holes in type signatures
    e.g.   f :: _ -> _
           f x = [x,True]

Note [Kind orientation for CTyEqCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given an equality (t:* ~ s:Open), we can't solve it by updating t:=s,
ragardless of how touchable 't' is, because the kinds don't work.

Instead we absolutely must re-orient it. Reason: if that gets into the
inert set we'll start replacing t's by s's, and that might make a
kind-correct type into a kind error.  After re-orienting,
we may be able to solve by updating s:=t.

Hence in a CTyEqCan, (t:k1 ~ xi:k2) we require that k2 is a subkind of k1.

If the two have incompatible kinds, we just don't use a CTyEqCan at all.
See Note [Equalities with incompatible kinds] in TcCanonical

We can't require *equal* kinds, because
     * wanted constraints don't necessarily have identical kinds
               eg   alpha::? ~ Int
     * a solved wanted constraint becomes a given

Note [Kind orientation for CFunEqCan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For (F xis ~ rhs) we require that kind(lhs) is a subkind of kind(rhs).
This really only maters when rhs is an Open type variable (since only type
variables have Open kinds):
   F ty ~ (a:Open)
which can happen, say, from
      f :: F a b
      f = undefined   -- The a:Open comes from instantiating 'undefined'

Note that the kind invariant is maintained by rewriting.
Eg wanted1 rewrites wanted2; if both were compatible kinds before,
   wanted2 will be afterwards.  Similarly givens.

Caveat:
  - Givens from higher-rank, such as:
          type family T b :: * -> * -> *
          type instance T Bool = (->)

          f :: forall a. ((T a ~ (->)) => ...) -> a -> ...
          flop = f (...) True
     Whereas we would be able to apply the type instance, we would not be able to
     use the given (T Bool ~ (->)) in the body of 'flop'


Note [CIrredEvCan constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CIrredEvCan constraints are used for constraints that are "stuck"
   - we can't solve them (yet)
   - we can't use them to solve other constraints
   - but they may become soluble if we substitute for some
     of the type variables in the constraint

Example 1:  (c Int), where c :: * -> Constraint.  We can't do anything
            with this yet, but if later c := Num, *then* we can solve it

Example 2:  a ~ b, where a :: *, b :: k, where k is a kind variable
            We don't want to use this to substitute 'b' for 'a', in case
            'k' is subequently unifed with (say) *->*, because then
            we'd have ill-kinded types floating about.  Rather we want
            to defer using the equality altogether until 'k' get resolved.

Note [Ct/evidence invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If  ct :: Ct, then extra fields of 'ct' cache precisely the ctev_pred field
of (cc_ev ct), and is fully rewritten wrt the substitution.   Eg for CDictCan,
   ctev_pred (cc_ev ct) = (cc_class ct) (cc_tyargs ct)
This holds by construction; look at the unique place where CDictCan is
built (in TcCanonical).

In contrast, the type of the evidence *term* (ccev_evtm or ctev_evar) in
the evidence may *not* be fully zonked; we are careful not to look at it
during constraint solving.  See Note [Evidence field of CtEvidence]
-}

mkNonCanonical :: CtEvidence -> Ct
mkNonCanonical ev = CNonCanonical { cc_ev = ev }

mkNonCanonicalCt :: Ct -> Ct
mkNonCanonicalCt ct = CNonCanonical { cc_ev = cc_ev ct }

ctEvidence :: Ct -> CtEvidence
ctEvidence = cc_ev

ctLoc :: Ct -> CtLoc
ctLoc = ctEvLoc . ctEvidence

setCtLoc :: Ct -> CtLoc -> Ct
setCtLoc ct loc = ct { cc_ev = (cc_ev ct) { ctev_loc = loc } }

ctOrigin :: Ct -> CtOrigin
ctOrigin = ctLocOrigin . ctLoc

ctPred :: Ct -> PredType
-- See Note [Ct/evidence invariant]
ctPred ct = ctEvPred (cc_ev ct)

-- | Get the flavour of the given 'Ct'
ctFlavour :: Ct -> CtFlavour
ctFlavour = ctEvFlavour . ctEvidence

-- | Get the equality relation for the given 'Ct'
ctEqRel :: Ct -> EqRel
ctEqRel = ctEvEqRel . ctEvidence

dropDerivedWC :: WantedConstraints -> WantedConstraints
-- See Note [Dropping derived constraints]
dropDerivedWC wc@(WC { wc_simple = simples, wc_insol = insols })
  = wc { wc_simple = dropDerivedSimples simples
       , wc_insol  = dropDerivedInsols insols }
    -- The wc_impl implications are already (recursively) filtered

dropDerivedSimples :: Cts -> Cts
dropDerivedSimples simples = filterBag isWantedCt simples
                             -- simples are all Wanted or Derived

dropDerivedInsols :: Cts -> Cts
-- See Note [Dropping derived constraints]
dropDerivedInsols insols = filterBag keep insols
  where                    -- insols can include Given
    keep ct
      | isDerivedCt ct = not (isDroppableDerivedLoc (ctLoc ct))
      | otherwise      = True

isDroppableDerivedLoc :: CtLoc -> Bool
-- Note [Dropping derived constraints]
isDroppableDerivedLoc loc
  = case ctLocOrigin loc of
      KindEqOrigin {}  -> False
      GivenOrigin {}   -> False
      FunDepOrigin1 {} -> False
      FunDepOrigin2 {} -> False
      _                -> True


{- Note [Dropping derived constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general we discard derived constraints at the end of constraint solving;
see dropDerivedWC.  For example

 * If we have an unsolved [W] (Ord a), we don't want to complain about
   an unsolved [D] (Eq a) as well.

 * If we have [W] a ~ Int, [W] a ~ Bool, improvement will generate
   [D] Int ~ Bool, and we don't want to report that because it's incomprehensible.
   That is why we don't rewrite wanteds with wanteds!

But (tiresomely) we do keep *some* Derived insolubles:

 * Insoluble kind equalities (e.g. [D] * ~ (* -> *)) may arise from
   a type equality a ~ Int#, say.  In future they'll be Wanted, not Derived,
   but at the moment they are Derived.

 * Insoluble derived equalities (e.g. [D] Int ~ Bool) may arise from
   functional dependency interactions, either between Givens or
   Wanteds.  It seems sensible to retain these:
   - For Givens they reflect unreachable code
   - For Wanteds it is arguably better to get a fundep error than
     a no-instance error (Trac #9612)

Moreover, we keep *all* derived insolubles under some circumstances:

  * They are looked at by simplifyInfer, to decide whether to
    generalise.  Example: [W] a ~ Int, [W] a ~ Bool
    We get [D] Int ~ Bool, and indeed the constraints are insoluble,
    and we want simplifyInfer to see that, even though we don't
    ultimately want to generate an (inexplicable) error message from

To distinguish these cases we use the CtOrigin.


************************************************************************
*                                                                      *
                    CtEvidence
         The "flavor" of a canonical constraint
*                                                                      *
************************************************************************
-}

isWantedCt :: Ct -> Bool
isWantedCt = isWanted . cc_ev

isGivenCt :: Ct -> Bool
isGivenCt = isGiven . cc_ev

isDerivedCt :: Ct -> Bool
isDerivedCt = isDerived . cc_ev

isCTyEqCan :: Ct -> Bool
isCTyEqCan (CTyEqCan {})  = True
isCTyEqCan (CFunEqCan {}) = False
isCTyEqCan _              = False

isCDictCan_Maybe :: Ct -> Maybe Class
isCDictCan_Maybe (CDictCan {cc_class = cls })  = Just cls
isCDictCan_Maybe _              = Nothing

isCIrredEvCan :: Ct -> Bool
isCIrredEvCan (CIrredEvCan {}) = True
isCIrredEvCan _                = False

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
-- A Hole that does not have a leading underscore is
-- simply an out-of-scope variable, and we treat that
-- a bit differently when it comes to error reporting
isOutOfScopeCt (CHoleCan { cc_occ = occ }) = not (startsWithUnderscore occ)
isOutOfScopeCt _ = False

isExprHoleCt :: Ct -> Bool
isExprHoleCt (CHoleCan { cc_hole = ExprHole }) = True
isExprHoleCt _ = False

isTypeHoleCt :: Ct -> Bool
isTypeHoleCt (CHoleCan { cc_hole = TypeHole }) = True
isTypeHoleCt _ = False

instance Outputable Ct where
  ppr ct = ppr (cc_ev ct) <+> parens (text ct_sort)
         where ct_sort = case ct of
                           CTyEqCan {}      -> "CTyEqCan"
                           CFunEqCan {}     -> "CFunEqCan"
                           CNonCanonical {} -> "CNonCanonical"
                           CDictCan {}      -> "CDictCan"
                           CIrredEvCan {}   -> "CIrredEvCan"
                           CHoleCan {}      -> "CHoleCan"

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
       , wc_insol  :: Cts              -- Insoluble constraints, can be
                                       -- wanted, given, or derived
                                       -- See Note [Insoluble constraints]
    }

emptyWC :: WantedConstraints
emptyWC = WC { wc_simple = emptyBag, wc_impl = emptyBag, wc_insol = emptyBag }

mkSimpleWC :: [CtEvidence] -> WantedConstraints
mkSimpleWC cts
  = WC { wc_simple = listToBag (map mkNonCanonical cts)
       , wc_impl = emptyBag
       , wc_insol = emptyBag }

isEmptyWC :: WantedConstraints -> Bool
isEmptyWC (WC { wc_simple = f, wc_impl = i, wc_insol = n })
  = isEmptyBag f && isEmptyBag i && isEmptyBag n

andWC :: WantedConstraints -> WantedConstraints -> WantedConstraints
andWC (WC { wc_simple = f1, wc_impl = i1, wc_insol = n1 })
      (WC { wc_simple = f2, wc_impl = i2, wc_insol = n2 })
  = WC { wc_simple = f1 `unionBags` f2
       , wc_impl   = i1 `unionBags` i2
       , wc_insol  = n1 `unionBags` n2 }

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
  = wc { wc_insol = wc_insol wc `unionBags` cts }

isInsolubleStatus :: ImplicStatus -> Bool
isInsolubleStatus IC_Insoluble = True
isInsolubleStatus _            = False

insolubleImplic :: Implication -> Bool
insolubleImplic ic = isInsolubleStatus (ic_status ic)

insolubleWC :: TcLevel -> WantedConstraints -> Bool
insolubleWC tc_lvl (WC { wc_impl = implics, wc_insol = insols })
  =  anyBag (trulyInsoluble tc_lvl) insols
  || anyBag insolubleImplic implics

trulyInsoluble :: TcLevel -> Ct -> Bool
-- The constraint is in the wc_insol set,
-- but we do not treat as truly isoluble
--  a) type-holes, arising from PartialTypeSignatures,
--  b) an out-of-scope variable
-- Yuk!
trulyInsoluble tc_lvl insol
  =  isOutOfScopeCt insol
  || isRigidEqPred tc_lvl (classifyPredType (ctPred insol))

instance Outputable WantedConstraints where
  ppr (WC {wc_simple = s, wc_impl = i, wc_insol = n})
   = ptext (sLit "WC") <+> braces (vcat
        [ ppr_bag (ptext (sLit "wc_simple")) s
        , ppr_bag (ptext (sLit "wc_insol")) n
        , ppr_bag (ptext (sLit "wc_impl")) i ])

ppr_bag :: Outputable a => SDoc -> Bag a -> SDoc
ppr_bag doc bag
 | isEmptyBag bag = empty
 | otherwise      = hang (doc <+> equals)
                       2 (foldrBag (($$) . ppr) empty bag)

{-
************************************************************************
*                                                                      *
                Implication constraints
*                                                                      *
************************************************************************
-}

data Implication
  = Implic {
      ic_tclvl :: TcLevel, -- TcLevel: unification variables
                                -- free in the environment

      ic_skols  :: [TcTyVar],    -- Introduced skolems
      ic_info  :: SkolemInfo,    -- See Note [Skolems in an implication]
                                 -- See Note [Shadowing in a constraint]

      ic_given  :: [EvVar],      -- Given evidence variables
                                 --   (order does not matter)
                                 -- See Invariant (GivenInv) in TcType

      ic_no_eqs :: Bool,         -- True  <=> ic_givens have no equalities, for sure
                                 -- False <=> ic_givens might have equalities

      ic_env   :: TcLclEnv,      -- Gives the source location and error context
                                 -- for the implication, and hence for all the
                                 -- given evidence variables

      ic_wanted :: WantedConstraints,  -- The wanted

      ic_binds  :: EvBindsVar,    -- Points to the place to fill in the
                                  -- abstraction and bindings

      ic_status   :: ImplicStatus
    }

data ImplicStatus
  = IC_Solved     -- All wanteds in the tree are solved, all the way down
       { ics_need :: VarSet     -- Evidence variables needed by this implication
       , ics_dead :: [EvVar] }  -- Subset of ic_given that are not needed
         -- See Note [Tracking redundant constraints] in TcSimplify

  | IC_Insoluble  -- At least one insoluble constraint in the tree

  | IC_Unsolved   -- Neither of the above; might go either way

instance Outputable Implication where
  ppr (Implic { ic_tclvl = tclvl, ic_skols = skols
              , ic_given = given, ic_no_eqs = no_eqs
              , ic_wanted = wanted, ic_status = status
              , ic_binds = binds, ic_info = info })
   = hang (ptext (sLit "Implic") <+> lbrace)
        2 (sep [ ptext (sLit "TcLevel =") <+> ppr tclvl
               , ptext (sLit "Skolems =") <+> pprTvBndrs skols
               , ptext (sLit "No-eqs =") <+> ppr no_eqs
               , ptext (sLit "Status =") <+> ppr status
               , hang (ptext (sLit "Given ="))  2 (pprEvVars given)
               , hang (ptext (sLit "Wanted =")) 2 (ppr wanted)
               , ptext (sLit "Binds =") <+> ppr binds
               , pprSkolInfo info ] <+> rbrace)

instance Outputable ImplicStatus where
  ppr IC_Insoluble   = ptext (sLit "Insoluble")
  ppr IC_Unsolved    = ptext (sLit "Unsolved")
  ppr (IC_Solved { ics_need = vs, ics_dead = dead })
    = ptext (sLit "Solved")
      <+> (braces $ vcat [ ptext (sLit "Dead givens =") <+> ppr dead
                         , ptext (sLit "Needed =") <+> ppr vs ])

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
 * The skolem varibles bound in ic_skols are all freah when the
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

{-
************************************************************************
*                                                                      *
            CtEvidence
*                                                                      *
************************************************************************

Note [Evidence field of CtEvidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
During constraint solving we never look at the type of ctev_evar;
instead we look at the cte_pred field.  The evtm/evar field
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

So a Given has EvVar inside it rather that (as previously) an EvTerm.
-}


data CtEvidence
  = CtGiven { ctev_pred :: TcPredType      -- See Note [Ct/evidence invariant]
            , ctev_evar :: EvVar           -- See Note [Evidence field of CtEvidence]
            , ctev_loc  :: CtLoc }
    -- Truly given, not depending on subgoals
    -- NB: Spontaneous unifications belong here

  | CtWanted { ctev_pred :: TcPredType     -- See Note [Ct/evidence invariant]
             , ctev_evar :: EvVar          -- See Note [Evidence field of CtEvidence]
             , ctev_loc  :: CtLoc }
    -- Wanted goal

  | CtDerived { ctev_pred :: TcPredType
              , ctev_loc  :: CtLoc }
    -- A goal that we don't really have to solve and can't immediately
    -- rewrite anything other than a derived (there's no evidence!)
    -- but if we do manage to solve it may help in solving other goals.

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
ctEvTerm ev = EvId (ctEvId ev)

ctEvCoercion :: CtEvidence -> TcCoercion
ctEvCoercion ev = mkTcCoVarCo (ctEvId ev)

ctEvId :: CtEvidence -> TcId
ctEvId (CtWanted { ctev_evar = ev }) = ev
ctEvId (CtGiven  { ctev_evar = ev }) = ev
ctEvId ctev = pprPanic "ctEvId:" (ppr ctev)

instance Outputable CtEvidence where
  ppr fl = case fl of
             CtGiven {}   -> ptext (sLit "[G]") <+> ppr (ctev_evar fl) <+> ppr_pty
             CtWanted {}  -> ptext (sLit "[W]") <+> ppr (ctev_evar fl) <+> ppr_pty
             CtDerived {} -> ptext (sLit "[D]") <+> text "_" <+> ppr_pty
         where ppr_pty = dcolon <+> ppr (ctEvPred fl)

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

Just an enum type that tracks whether a constraint is wanted, derived,
or given, when we need to separate that info from the constraint itself.

-}

data CtFlavour = Given | Wanted | Derived
  deriving Eq

instance Outputable CtFlavour where
  ppr Given   = text "[G]"
  ppr Wanted  = text "[W]"
  ppr Derived = text "[D]"

ctEvFlavour :: CtEvidence -> CtFlavour
ctEvFlavour (CtWanted {})  = Wanted
ctEvFlavour (CtGiven {})   = Given
ctEvFlavour (CtDerived {}) = Derived

-- | Whether or not one 'Ct' can rewrite another is determined by its
-- flavour and its equality relation
type CtFlavourRole = (CtFlavour, EqRel)

-- | Extract the flavour and role from a 'CtEvidence'
ctEvFlavourRole :: CtEvidence -> CtFlavourRole
ctEvFlavourRole ev = (ctEvFlavour ev, ctEvEqRel ev)

-- | Extract the flavour and role from a 'Ct'
ctFlavourRole :: Ct -> CtFlavourRole
ctFlavourRole = ctEvFlavourRole . cc_ev

{- Note [eqCanRewrite]
~~~~~~~~~~~~~~~~~~~
(eqCanRewrite ct1 ct2) holds if the constraint ct1 (a CTyEqCan of form
tv ~ ty) can be used to rewrite ct2.  It must satisfy the properties of
a can-rewrite relation, see Definition [Can-rewrite relation]

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
then we lose the property of Note [Can-rewrite relation]
  R2.  If f1 >= f, and f2 >= f,
       then either f1 >= f2 or f2 >= f1
Consider f1 = (Given, ReprEq)
         f2 = (Derived, NomEq)
          f = (Derived, ReprEq)

I thought maybe we could never get Derived ReprEq constraints, but
we can; straight from the Wanteds during improvment. And from a Derived
ReprEq we could conceivably get a Derived NomEq improvment (by decomposing
a type constructor with Nomninal role), and hence unify.

Note [canRewriteOrSame]
~~~~~~~~~~~~~~~~~~~~~~~
canRewriteOrSame is similar but
 * returns True for Wanted/Wanted.
 * works for all kinds of constraints, not just CTyEqCans
See the call sites for explanations.
-}

eqCanRewrite :: CtEvidence -> CtEvidence -> Bool
eqCanRewrite ev1 ev2 = ctEvFlavourRole ev1 `eqCanRewriteFR` ctEvFlavourRole ev2

eqCanRewriteFR :: CtFlavourRole -> CtFlavourRole -> Bool
-- Very important function!
-- See Note [eqCanRewrite]
-- See Note [Wanteds do not rewrite Wanteds]
-- See Note [Deriveds do rewrite Deriveds]
eqCanRewriteFR (Given,   NomEq)   (_,       _)      = True
eqCanRewriteFR (Given,   ReprEq)  (_,       ReprEq) = True
eqCanRewriteFR (Derived, NomEq)   (Derived, NomEq)  = True
eqCanRewriteFR _                 _                  = False

canDischarge :: CtEvidence -> CtEvidence -> Bool
-- See Note [canRewriteOrSame]
canDischarge ev1 ev2 = ctEvFlavourRole ev1 `canDischargeFR` ctEvFlavourRole ev2

canDischargeFR :: CtFlavourRole -> CtFlavourRole -> Bool
canDischargeFR (_, ReprEq)  (_, NomEq)   = False
canDischargeFR (Given, _)   _            = True
canDischargeFR (Wanted, _)  (Wanted, _)  = True
canDischargeFR (Wanted, _)  (Derived, _) = True
canDischargeFR (Derived, _) (Derived, _) = True
canDischargeFR _             _           = False


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
  If thre is an type function equation "F () = Int", this would be rewritten to
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
                   , ctl_depth  :: !SubGoalDepth }
  -- The TcLclEnv includes particularly
  --    source location:  tcl_loc   :: RealSrcSpan
  --    context:          tcl_ctxt  :: [ErrCtxt]
  --    binder stack:     tcl_bndrs :: TcIdBinderStack
  --    level:            tcl_tclvl :: TcLevel

mkGivenLoc :: TcLevel -> SkolemInfo -> TcLclEnv -> CtLoc
mkGivenLoc tclvl skol_info env
  = CtLoc { ctl_origin = GivenOrigin skol_info
          , ctl_env    = env { tcl_tclvl = tclvl }
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

setCtLocSpan :: CtLoc -> RealSrcSpan -> CtLoc
setCtLocSpan ctl@(CtLoc { ctl_env = lcl }) loc = setCtLocEnv ctl (lcl { tcl_loc = loc })

bumpCtLocDepth :: CtLoc -> CtLoc
bumpCtLocDepth loc@(CtLoc { ctl_depth = d }) = loc { ctl_depth = bumpSubGoalDepth d }

setCtLocOrigin :: CtLoc -> CtOrigin -> CtLoc
setCtLocOrigin ctl orig = ctl { ctl_origin = orig }

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
  = SigSkol UserTypeCtxt        -- A skolem that is created by instantiating
            Type                -- a programmer-supplied type signature
                                -- Location of the binding site is on the TyVar

        -- The rest are for non-scoped skolems
  | ClsSkol Class       -- Bound at a class decl

  | InstSkol            -- Bound at an instance decl
  | InstSC TypeSize     -- A "given" constraint obtained by superclass selection.
                        -- If (C ty1 .. tyn) is the largest class from
                        --    which we made a superclass selection in the chain,
                        --    then TypeSize = sizeTypes [ty1, .., tyn]
                        -- See Note [Solving superclass constraints] in TcInstDcls

  | DataSkol            -- Bound at a data type declaration
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
       [TcTyVar]        -- The instantiated skolem variables
       TcType           -- The instantiated type *inside* the forall

  | UnkSkol             -- Unhelpful info (until I improve it)

instance Outputable SkolemInfo where
  ppr = pprSkolInfo

pprSkolInfo :: SkolemInfo -> SDoc
-- Complete the sentence "is a rigid type variable bound by..."
pprSkolInfo (SigSkol ctxt ty) = pprSigSkolInfo ctxt ty
pprSkolInfo (IPSkol ips)      = ptext (sLit "the implicit-parameter binding") <> plural ips <+> ptext (sLit "for")
                                <+> pprWithCommas ppr ips
pprSkolInfo (ClsSkol cls)     = ptext (sLit "the class declaration for") <+> quotes (ppr cls)
pprSkolInfo InstSkol          = ptext (sLit "the instance declaration")
pprSkolInfo (InstSC n)        = ptext (sLit "the instance declaration") <> ifPprDebug (parens (ppr n))
pprSkolInfo DataSkol          = ptext (sLit "a data type declaration")
pprSkolInfo FamInstSkol       = ptext (sLit "a family instance declaration")
pprSkolInfo BracketSkol       = ptext (sLit "a Template Haskell bracket")
pprSkolInfo (RuleSkol name)   = ptext (sLit "the RULE") <+> pprRuleName name
pprSkolInfo ArrowSkol         = ptext (sLit "an arrow form")
pprSkolInfo (PatSkol cl mc)   = sep [ pprPatSkolInfo cl
                                    , ptext (sLit "in") <+> pprMatchContext mc ]
pprSkolInfo (InferSkol ids)   = sep [ ptext (sLit "the inferred type of")
                                    , vcat [ ppr name <+> dcolon <+> ppr ty
                                           | (name,ty) <- ids ]]
pprSkolInfo (UnifyForAllSkol tvs ty) = ptext (sLit "the type") <+> ppr (mkForAllTys tvs ty)

-- UnkSkol
-- For type variables the others are dealt with by pprSkolTvBinding.
-- For Insts, these cases should not happen
pprSkolInfo UnkSkol = WARN( True, text "pprSkolInfo: UnkSkol" ) ptext (sLit "UnkSkol")

pprSigSkolInfo :: UserTypeCtxt -> Type -> SDoc
pprSigSkolInfo ctxt ty
  = case ctxt of
       FunSigCtxt f _ -> pp_sig f
       _              -> hang (pprUserTypeCtxt ctxt <> colon)
                            2 (ppr ty)
  where
    pp_sig f = vcat [ ptext (sLit "the type signature for:")
                    , nest 2 (pprPrefixOcc f <+> dcolon <+> ppr ty) ]

pprPatSkolInfo :: ConLike -> SDoc
pprPatSkolInfo (RealDataCon dc)
  = sep [ ptext (sLit "a pattern with constructor:")
        , nest 2 $ ppr dc <+> dcolon
          <+> pprType (dataConUserType dc) <> comma ]
          -- pprType prints forall's regardless of -fprint-explict-foralls
          -- which is what we want here, since we might be saying
          -- type variable 't' is bound by ...

pprPatSkolInfo (PatSynCon ps)
  = sep [ ptext (sLit "a pattern with pattern synonym:")
        , nest 2 $ ppr ps <+> dcolon
                   <+> pprType (patSynType ps) <> comma ]

{-
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
  | AppOrigin                      -- An application of some kind

  | SpecPragOrigin UserTypeCtxt    -- Specialisation pragma for
                                   -- function or instance

  | TypeEqOrigin { uo_actual   :: TcType
                 , uo_expected :: TcType }
  | KindEqOrigin
      TcType TcType             -- A kind equality arising from unifying these two types
      CtOrigin                  -- originally arising from this

  | IPOccOrigin  HsIPName       -- Occurrence of an implicit parameter

  | LiteralOrigin (HsOverLit Name)      -- Occurrence of a literal
  | NegateOrigin                        -- Occurrence of syntactic negation

  | ArithSeqOrigin (ArithSeqInfo Name) -- [x..], [x..y] etc
  | PArrSeqOrigin  (ArithSeqInfo Name) -- [:x..y:] and [:x,y..z:]
  | SectionOrigin
  | TupleOrigin                        -- (..,..)
  | ExprSigOrigin       -- e :: ty
  | PatSigOrigin        -- p :: ty
  | PatOrigin           -- Instantiating a polytyped pattern at a constructor
  | RecordUpdOrigin
  | ViewPatOrigin

  | ScOrigin TypeSize   -- Typechecking superclasses of an instance declaration
                        -- If the instance head is C ty1 .. tyn
                        --    then TypeSize = sizeTypes [ty1, .., tyn]
                        -- See Note [Solving superclass constraints] in TcInstDcls

  | DerivOrigin         -- Typechecking deriving
  | DerivOriginDC DataCon Int
                        -- Checking constraints arising from this data con and field index
  | DerivOriginCoerce Id Type Type
                        -- DerivOriginCoerce id ty1 ty2: Trying to coerce class method `id` from
                        -- `ty1` to `ty2`.
  | StandAloneDerivOrigin -- Typechecking stand-alone deriving
  | DefaultOrigin       -- Typechecking a default decl
  | DoOrigin            -- Arising from a do expression
  | MCompOrigin         -- Arising from a monad comprehension
  | IfOrigin            -- Arising from an if statement
  | ProcOrigin          -- Arising from a proc expression
  | AnnOrigin           -- An annotation

  | FunDepOrigin1       -- A functional dependency from combining
        PredType CtLoc      -- This constraint arising from ...
        PredType CtLoc      -- and this constraint arising from ...

  | FunDepOrigin2       -- A functional dependency from combining
        PredType CtOrigin   -- This constraint arising from ...
        PredType SrcSpan    -- and this instance
        -- We only need a CtOrigin on the first, because the location
        -- is pinned on the entire error message

  | HoleOrigin
  | UnboundOccurrenceOf RdrName
  | ListOrigin          -- An overloaded list
  | StaticOrigin        -- A static form

ctoHerald :: SDoc
ctoHerald = ptext (sLit "arising from")

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
       FunSigCtxt n _ -> ptext (sLit "a SPECIALISE pragma for") <+> quotes (ppr n)
       SpecInstCtxt   -> ptext (sLit "a SPECIALISE INSTANCE pragma")
       _              -> ptext (sLit "a SPECIALISE pragma")  -- Never happens I think

pprCtOrigin (FunDepOrigin1 pred1 loc1 pred2 loc2)
  = hang (ctoHerald <+> ptext (sLit "a functional dependency between constraints:"))
       2 (vcat [ hang (quotes (ppr pred1)) 2 (pprCtLoc loc1)
               , hang (quotes (ppr pred2)) 2 (pprCtLoc loc2) ])

pprCtOrigin (FunDepOrigin2 pred1 orig1 pred2 loc2)
  = hang (ctoHerald <+> ptext (sLit "a functional dependency between:"))
       2 (vcat [ hang (ptext (sLit "constraint") <+> quotes (ppr pred1))
                    2 (pprCtOrigin orig1 )
               , hang (ptext (sLit "instance") <+> quotes (ppr pred2))
                    2 (ptext (sLit "at") <+> ppr loc2) ])

pprCtOrigin (KindEqOrigin t1 t2 _)
  = hang (ctoHerald <+> ptext (sLit "a kind equality arising from"))
       2 (sep [ppr t1, char '~', ppr t2])

pprCtOrigin (UnboundOccurrenceOf name)
  = ctoHerald <+> ptext (sLit "an undeclared identifier") <+> quotes (ppr name)

pprCtOrigin (DerivOriginDC dc n)
  = hang (ctoHerald <+> ptext (sLit "the") <+> speakNth n
          <+> ptext (sLit "field of") <+> quotes (ppr dc))
       2 (parens (ptext (sLit "type") <+> quotes (ppr ty)))
  where
    ty = dataConOrigArgTys dc !! (n-1)

pprCtOrigin (DerivOriginCoerce meth ty1 ty2)
  = hang (ctoHerald <+> ptext (sLit "the coercion of the method") <+> quotes (ppr meth))
       2 (sep [ text "from type" <+> quotes (ppr ty1)
              , nest 2 $ text "to type" <+> quotes (ppr ty2) ])

pprCtOrigin simple_origin
  = ctoHerald <+> pprCtO simple_origin

----------------
pprCtO :: CtOrigin -> SDoc  -- Ones that are short one-liners
pprCtO (OccurrenceOf name)   = hsep [ptext (sLit "a use of"), quotes (ppr name)]
pprCtO AppOrigin             = ptext (sLit "an application")
pprCtO (IPOccOrigin name)    = hsep [ptext (sLit "a use of implicit parameter"), quotes (ppr name)]
pprCtO RecordUpdOrigin       = ptext (sLit "a record update")
pprCtO ExprSigOrigin         = ptext (sLit "an expression type signature")
pprCtO PatSigOrigin          = ptext (sLit "a pattern type signature")
pprCtO PatOrigin             = ptext (sLit "a pattern")
pprCtO ViewPatOrigin         = ptext (sLit "a view pattern")
pprCtO IfOrigin              = ptext (sLit "an if statement")
pprCtO (LiteralOrigin lit)   = hsep [ptext (sLit "the literal"), quotes (ppr lit)]
pprCtO (ArithSeqOrigin seq)  = hsep [ptext (sLit "the arithmetic sequence"), quotes (ppr seq)]
pprCtO (PArrSeqOrigin seq)   = hsep [ptext (sLit "the parallel array sequence"), quotes (ppr seq)]
pprCtO SectionOrigin         = ptext (sLit "an operator section")
pprCtO TupleOrigin           = ptext (sLit "a tuple")
pprCtO NegateOrigin          = ptext (sLit "a use of syntactic negation")
pprCtO (ScOrigin n)          = ptext (sLit "the superclasses of an instance declaration")
                               <> ifPprDebug (parens (ppr n))
pprCtO DerivOrigin           = ptext (sLit "the 'deriving' clause of a data type declaration")
pprCtO StandAloneDerivOrigin = ptext (sLit "a 'deriving' declaration")
pprCtO DefaultOrigin         = ptext (sLit "a 'default' declaration")
pprCtO DoOrigin              = ptext (sLit "a do statement")
pprCtO MCompOrigin           = ptext (sLit "a statement in a monad comprehension")
pprCtO ProcOrigin            = ptext (sLit "a proc expression")
pprCtO (TypeEqOrigin t1 t2)  = ptext (sLit "a type equality") <+> sep [ppr t1, char '~', ppr t2]
pprCtO AnnOrigin             = ptext (sLit "an annotation")
pprCtO HoleOrigin            = ptext (sLit "a use of") <+> quotes (ptext $ sLit "_")
pprCtO ListOrigin            = ptext (sLit "an overloaded list")
pprCtO StaticOrigin          = ptext (sLit "a static form")
pprCtO _                     = panic "pprCtOrigin"

{-
Constraint Solver Plugins
-------------------------
-}

type TcPluginSolver = [Ct]    -- given
                   -> [Ct]    -- derived
                   -> [Ct]    -- wanted
                   -> TcPluginM TcPluginResult

newtype TcPluginM a = TcPluginM (Maybe EvBindsVar -> TcM a)

instance Functor     TcPluginM where
  fmap = liftM

instance Applicative TcPluginM where
  pure  = return
  (<*>) = ap

instance Monad TcPluginM where
  return x = TcPluginM (const $ return x)
  fail x   = TcPluginM (const $ fail x)
  TcPluginM m >>= k =
    TcPluginM (\ ev -> do a <- m ev
                          runTcPluginM (k a) ev)

runTcPluginM :: TcPluginM a -> Maybe EvBindsVar -> TcM a
runTcPluginM (TcPluginM m) = m

-- | This function provides an escape for direct access to
-- the 'TcM` monad.  It should not be used lightly, and
-- the provided 'TcPluginM' API should be favoured instead.
unsafeTcPluginTcM :: TcM a -> TcPluginM a
unsafeTcPluginTcM = TcPluginM . const

-- | Access the 'EvBindsVar' carried by the 'TcPluginM' during
-- constraint solving.  Returns 'Nothing' if invoked during
-- 'tcPluginInit' or 'tcPluginStop'.
getEvBindsTcPluginM_maybe :: TcPluginM (Maybe EvBindsVar)
getEvBindsTcPluginM_maybe = TcPluginM return


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
    -- and recorded as insoluable.

  | TcPluginOk [(EvTerm,Ct)] [Ct]
    -- ^ The first field is for constraints that were solved.
    -- These are removed from the inert set,
    -- and the evidence for them is recorded.
    -- The second field contains new work, that should be processed by
    -- the constraint solver.
