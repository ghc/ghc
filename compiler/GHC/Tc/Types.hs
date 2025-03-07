
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-2002

-}

-- | Various types used during typechecking.
--
-- Please see "GHC.Tc.Utils.Monad" as well for operations on these types. You probably
-- want to import it, instead of this module.
--
-- All the monads exported here are built on top of the same IOEnv monad. The
-- monad functions like a Reader monad in the way it passes the environment
-- around. This is done to allow the environment to be manipulated in a stack
-- like fashion when entering expressions... etc.
--
-- For state that is global and should be returned at the end (e.g not part
-- of the stack mechanism), you should use a TcRef (= IORef) to store them.
module GHC.Tc.Types(
        TcRnIf, TcRn, TcM, RnM, IfM, IfL, IfG, -- The monad is opaque outside this module
        TcRef,

        -- The environment types
        Env(..),
        TcGblEnv(..), TcLclEnv(..), modifyLclCtxt, TcLclCtxt(..),
        setLclEnvTcLevel, getLclEnvTcLevel,
        setLclEnvLoc, getLclEnvLoc, lclEnvInGeneratedCode,
        IfGblEnv(..), IfLclEnv(..),
        tcVisibleOrphanMods,
        RewriteEnv(..),

        -- Frontend types (shouldn't really be here)
        FrontendResult(..),

        -- Renamer types
        ErrCtxt,
        ImportAvails(..), emptyImportAvails, plusImportAvails,
        ImportUserSpec(..),
        ImpUserList(..),
        mkModDeps,

        -- Typechecker types
        TcTypeEnv, TcBinderStack, TcBinder(..),
        TcTyThing(..), tcTyThingTyCon_maybe,
        PromotionErr(..),
        IdBindingInfo(..), ClosedTypeId, RhsNames,
        IsGroupClosed(..),
        SelfBootInfo(..), bootExports,
        tcTyThingCategory, pprTcTyThingCategory,
        peCategory, pprPECategory,
        CompleteMatch, CompleteMatches,

        -- Template Haskell
        ThStage(..), SpliceType(..), SpliceOrBracket(..), PendingStuff(..),
        topStage, topAnnStage, topSpliceStage,
        ThLevel, impLevel, outerLevel, thLevel,
        ForeignSrcLang(..), THDocs, DocLoc(..),
        ThBindEnv,

        -- Arrows
        ArrowCtxt(..),

        -- TcSigInfo
        TcSigFun,
        TcSigInfo(..), TcIdSig(..),
        TcCompleteSig(..), TcPartialSig(..), TcPatSynSig(..),
        TcIdSigInst(..),
        isPartialSig, hasCompleteSig, tcSigInfoName, tcIdSigLoc,
        completeSigPolyId_maybe,

        -- Misc other types
        TcId,
        NameShape(..),
        removeBindingShadowing,
        getPlatform,

        -- Constraint solver plugins
        TcPlugin(..),
        TcPluginSolveResult(TcPluginContradiction, TcPluginOk, ..),
        TcPluginRewriteResult(..),
        TcPluginSolver, TcPluginRewriter,
        TcPluginM(runTcPluginM), unsafeTcPluginTcM,

        -- Defaulting plugin
        DefaultingPlugin(..), DefaultingProposal(..),
        FillDefaulting,

        -- Role annotations
        RoleAnnotEnv, emptyRoleAnnotEnv, mkRoleAnnotEnv,
        lookupRoleAnnot, getRoleAnnots,

        -- Linting
        lintGblEnv,

        -- Diagnostics
        TcRnMessage
  ) where

import GHC.Prelude
import GHC.Platform

import GHC.Driver.Env
import GHC.Driver.Env.KnotVars
import GHC.Driver.Config.Core.Lint
import GHC.Driver.DynFlags
import {-# SOURCE #-} GHC.Driver.Hooks

import GHC.Linker.Types

import GHC.Hs

import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.CtLoc( CtLoc )
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.TH
import GHC.Tc.Types.TcRef
import GHC.Tc.Types.LclEnv
import GHC.Tc.Types.BasicTypes
import GHC.Tc.Types.ErrCtxt
import {-# SOURCE #-} GHC.Tc.Errors.Hole.Plugin ( HoleFitPlugin )
import GHC.Tc.Errors.Types

import GHC.Core.Reduction ( Reduction(..) )
import GHC.Core.Type
import GHC.Core.TyCon  ( TyCon )
import GHC.Core.PatSyn ( PatSyn )
import GHC.Core.Lint   ( lintAxioms )
import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core.Predicate

import GHC.Types.DefaultEnv ( DefaultEnv )
import GHC.Types.Fixity.Env
import GHC.Types.Annotations
import GHC.Types.CompleteMatch
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Set
import GHC.Types.Avail
import GHC.Types.Var
import GHC.Types.TypeEnv
import GHC.Types.SourceFile
import GHC.Types.SrcLoc
import GHC.Types.Unique.FM
import GHC.Types.Basic
import GHC.Types.CostCentre.State
import GHC.Types.HpcInfo

import GHC.Data.IOEnv
import GHC.Data.Bag
import GHC.Data.List.SetOps

import GHC.Unit
import GHC.Unit.Module.Warnings
import GHC.Unit.Module.Deps
import GHC.Unit.Module.ModDetails

import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Fingerprint
import GHC.Utils.Panic
import GHC.Utils.Logger

import GHC.Builtin.Names ( isUnboundName )

import GHCi.Message
import GHCi.RemoteTypes

import Data.Set      ( Set )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Dynamic  ( Dynamic )
import Data.Map ( Map )
import Data.Typeable ( TypeRep )
import Data.Maybe    ( mapMaybe )

-- | The import specification as written by the user, including
-- the list of explicitly imported names. Used in 'ModIface' to
-- allow GHCi to reconstruct the top level environment on demand.
--
-- This is distinct from 'ImportSpec' because we don't want to store
-- the list of explicitly imported names along with each 'GRE'
--
-- We don't want to store the entire GlobalRdrEnv for modules that
-- are imported without explicit export lists, as these may grow
-- to be very large. However, GlobalRdrEnvs which are the result
-- of explicit import lists are typically quite small.
--
-- Why do we not store something like (Maybe (ImportListInterpretation, [IE GhcPs]) in such a case?
-- Because we don't want to store source syntax including annotations in
-- interface files.
data ImportUserSpec
  = ImpUserSpec { ius_decl :: !ImpDeclSpec
                , ius_imports :: !ImpUserList
                }

data ImpUserList
  = ImpUserAll -- ^ no user import list
  | ImpUserExplicit ![AvailInfo]
  | ImpUserEverythingBut !NameSet

-- | A 'NameShape' is a substitution on 'Name's that can be used
-- to refine the identities of a hole while we are renaming interfaces
-- (see "GHC.Iface.Rename").  Specifically, a 'NameShape' for
-- 'ns_module_name' @A@, defines a mapping from @{A.T}@
-- (for some 'OccName' @T@) to some arbitrary other 'Name'.
--
-- The most intriguing thing about a 'NameShape', however, is
-- how it's constructed.  A 'NameShape' is *implied* by the
-- exported 'AvailInfo's of the implementor of an interface:
-- if an implementor of signature @\<H>@ exports @M.T@, you implicitly
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
    All the combinators for the monad can be found in GHC.Tc.Utils.Monad
*                                                                      *
************************************************************************

The monad itself has to be defined here, because it is mentioned by ErrCtxt
-}

type TcRnIf a b = IOEnv (Env a b)
type TcRn       = TcRnIf TcGblEnv TcLclEnv    -- Type inference
type IfM lcl    = TcRnIf IfGblEnv lcl         -- Iface stuff
type IfG        = IfM ()                      --    Top level
type IfL        = IfM IfLclEnv                --    Nested

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

        env_ut   :: {-# UNPACK #-} !Char,   -- Tag for Uniques

        env_gbl  :: gbl,     -- Info about things defined at the top level
                             -- of the module being compiled

        env_lcl  :: lcl      -- Nested stuff; changes as we go into
    }

instance ContainsDynFlags (Env gbl lcl) where
    extractDynFlags env = hsc_dflags (env_top env)

instance ContainsHooks (Env gbl lcl) where
    extractHooks env = hsc_hooks (env_top env)

instance ContainsLogger (Env gbl lcl) where
    extractLogger env = hsc_logger (env_top env)

instance ContainsModule gbl => ContainsModule (Env gbl lcl) where
    extractModule env = extractModule (env_gbl env)

{-
************************************************************************
*                                                                      *
*                            RewriteEnv
*                     The rewriting environment
*                                                                      *
************************************************************************
-}

-- | A 'RewriteEnv' carries the necessary context for performing rewrites
-- (i.e. type family reductions and following filled-in metavariables)
-- in the solver.
data RewriteEnv
  = RE { re_loc     :: !CtLoc
       -- ^ In which context are we rewriting?
       --
       -- Type-checking plugins might want to use this location information
       -- when emitting new Wanted constraints when rewriting type family
       -- applications. This ensures that such Wanted constraints will,
       -- when unsolved, give rise to error messages with the
       -- correct source location.

       -- Within GHC, we use this field to keep track of reduction depth.
       -- See Note [Rewriter CtLoc] in GHC.Tc.Solver.Rewrite.
       , re_flavour :: !CtFlavour
       , re_eq_rel  :: !EqRel
          -- ^ At what role are we rewriting?
          -- See Note [Rewriter EqRels] in GHC.Tc.Solver.Rewrite

       , re_rewriters :: !(TcRef RewriterSet)  -- ^ See Note [Wanteds rewrite Wanteds]
       }
-- RewriteEnv is mostly used in @GHC.Tc.Solver.Rewrite@, but it is defined
-- here so that it can also be passed to rewriting plugins.
-- See the 'tcPluginRewrite' field of 'TcPlugin'.


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
        -- See Note [Tying the knot] in GHC.IfaceToCore
        if_rec_types :: (KnotVars (IfG TypeEnv))
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
        if_mod :: !Module,

        -- Whether or not the IfaceDecl came from a boot
        -- file or not; we'll use this to choose between
        -- NoUnfolding and BootUnfolding
        if_boot :: IsBootInterface,

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
        -- of the time it's @Nothing@.  See Note [Resolving never-exported Names]
        -- in GHC.IfaceToCore.
        if_implicits_env :: Maybe TypeEnv,

        if_tv_env  :: FastStringEnv TyVar,     -- Nested tyvar bindings
        if_id_env  :: FastStringEnv Id         -- Nested id binding
    }

{-
************************************************************************
*                                                                      *
                Global typechecker environment
*                                                                      *
************************************************************************
-}

-- | 'FrontendResult' describes the result of running the frontend of a Haskell
-- module. Currently one always gets a 'FrontendTypecheck', since running the
-- frontend involves typechecking a program. hs-sig merges are not handled here.
--
-- This data type really should be in GHC.Driver.Env, but it needs
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
--        A corollary of this is that the following invariant holds at any point
--        past desugaring,
--
--            if I have a Module, this_mod, in hand representing the module
--            currently being compiled,
--            then moduleUnit this_mod == thisPackage dflags
--
--      - For any code involving Names, we want semantic modules.
--        See lookupIfaceTop in GHC.Iface.Env, mkIface and addFingerprints
--        in GHC.Iface.{Make,Recomp}, and tcLookupGlobal in GHC.Tc.Utils.Env
--
--      - When reading interfaces, we want the identity module to
--        identify the specific interface we want (such interfaces
--        should never be loaded into the EPS).  However, if a
--        hole module <A> is requested, we look for A.hi
--        in the home library we are compiling.  (See GHC.Iface.Load.)
--        Similarly, in GHC.Rename.Names we check for self-imports using
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
        tcg_default         :: DefaultEnv,     -- ^ All class defaults in scope in the module
        tcg_default_exports :: DefaultEnv,     -- ^ All class defaults exported from the module

        tcg_fix_env :: FixityEnv,      -- ^ Just for things in this module

        tcg_type_env :: TypeEnv,
          -- ^ Global type env for the module we are compiling now.  All
          -- TyCons and Classes (for this module) end up in here right away,
          -- along with their derived constructors, selectors.
          --
          -- (Ids defined in this module start in the local envt, though they
          --  move to the global envt during zonking)
          --
          -- NB: for what "things in this module" means, see
          -- Note [The interactive package] in "GHC.Runtime.Context"

        tcg_type_env_var :: KnotVars (IORef TypeEnv),
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
          --      (mkIfaceTc, as well as in "GHC.Driver.Main")
          --    - To create the Dependencies field in interface (mkDependencies)


          -- This field tracks the user-written imports of a module, so they can be
          -- recorded in an interface file in order to reconstruct the top-level environment
          -- if necessary for GHCi.
        tcg_import_decls :: ![ImportUserSpec],

          -- These three fields track unused bindings and imports
          -- See Note [Tracking unused binding and imports]
        tcg_dus       :: DefUses,
        tcg_used_gres :: TcRef [GlobalRdrElt],
          -- ^ INVARIANT: all these GREs were imported; that is,
          -- they all have a non-empty gre_imp field.
        tcg_keep      :: TcRef NameSet,

        tcg_th_used :: TcRef Bool,
          -- ^ @True@ \<=> Template Haskell syntax used.
          --
          -- We need this so that we can generate a dependency on the
          -- Template Haskell package, because the desugarer is going
          -- to emit loads of references to TH symbols.  The reference
          -- is implicit rather than explicit, so we have to zap a
          -- mutable variable.

        tcg_th_needed_deps :: TcRef ([Linkable], PkgsLoaded),
          -- ^ The set of runtime dependencies required by this module
          -- See Note [Object File Dependencies]

        tcg_dfun_n  :: TcRef OccSet,
          -- ^ Allows us to choose unique DFun names.

        tcg_zany_n :: TcRef Integer,
          -- ^ A source of unique identities for ZonkAny instances
          -- See Note [Any types] in GHC.Builtin.Types, wrinkle (Any4)

        tcg_merged :: [(Module, Fingerprint)],
          -- ^ The requirements we merged with; we always have to recompile
          -- if any of these changed.

        -- The next fields accumulate the payload of the module
        -- The binds, rules and foreign-decl fields are collected
        -- initially in un-zonked form and are finally zonked in tcRnSrcDecls

        tcg_rn_exports :: Maybe [(LIE GhcRn, Avails)],
                -- Nothing <=> no explicit export list
                -- Is always Nothing if we don't want to retain renamed
                -- exports.
                -- If present contains each renamed export list item
                -- together with its exported names.

        tcg_rn_imports :: [LImportDecl GhcRn],
                -- Keep the renamed imports regardless.  They are not
                -- voluminous and are needed if you want to report unused imports

        tcg_rn_decls :: Maybe (HsGroup GhcRn),
          -- ^ Renamed decls, maybe.  @Nothing@ \<=> Don't retain renamed
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

        tcg_th_docs   :: TcRef THDocs,
        -- ^ Docs added in Template Haskell via @putDoc@.

        tcg_ev_binds  :: Bag EvBind,        -- Top-level evidence bindings

        -- Things defined in this module, or (in GHCi)
        -- in the declarations for a single GHCi command.
        -- For the latter, see Note [The interactive package] in
        -- GHC.Runtime.Context
        tcg_tr_module :: Maybe Id,   -- Id for $trModule :: GHC.Unit.Module
                                             -- for which every module has a top-level defn
                                             -- except in GHCi in which case we have Nothing
        tcg_binds     :: LHsBinds GhcTc,     -- Value bindings in this module
        tcg_sigs      :: NameSet,            -- ...Top-level names that *lack* a signature
        tcg_imp_specs :: [LTcSpecPrag],      -- ...SPECIALISE prags for imported Ids
        tcg_warns     :: (Warnings GhcRn), -- ...Warnings and deprecations
        tcg_anns      :: [Annotation],       -- ...Annotations
        tcg_tcs       :: [TyCon],            -- ...TyCons and Classes
        tcg_ksigs     :: NameSet,            -- ...Top-level TyCon names that *lack* a signature
        tcg_insts     :: [ClsInst],          -- ...Instances
        tcg_fam_insts :: [FamInst],          -- ...Family instances
        tcg_rules     :: [LRuleDecl GhcTc],  -- ...Rules
        tcg_fords     :: [LForeignDecl GhcTc], -- ...Foreign import & exports
        tcg_patsyns   :: [PatSyn],            -- ...Pattern synonyms

        tcg_hdr_info   :: (Maybe (LHsDoc GhcRn), Maybe (XRec GhcRn ModuleName)),
        -- ^ Maybe Haddock header docs and Maybe located module name

        tcg_hpc       :: !AnyHpcUsage,       -- ^ @True@ if any part of the
                                             --  prog uses hpc instrumentation.
           -- NB. BangPattern is to fix a leak, see #15111

        tcg_self_boot :: SelfBootInfo,       -- ^ Whether this module has a
                                             -- corresponding hi-boot file

        tcg_main      :: Maybe Name,         -- ^ The Name of the main
                                             -- function, if this module is
                                             -- the main module.

        tcg_safe_infer :: TcRef Bool,
        -- ^ Has the typechecker inferred this module as -XSafe (Safe Haskell)?
        -- See Note [Safe Haskell Overlapping Instances Implementation],
        -- although this is used for more than just that failure case.

        tcg_safe_infer_reasons :: TcRef (Messages TcRnMessage),
        -- ^ Unreported reasons why tcg_safe_infer is False.
        -- INVARIANT: If this Messages is non-empty, then tcg_safe_infer is False.
        -- It may be that tcg_safe_infer is False but this is empty, if no reasons
        -- are supplied (#19714), or if those reasons have already been
        -- reported by GHC.Driver.Main.markUnsafeInfer

        tcg_tc_plugin_solvers :: [TcPluginSolver],
        -- ^ A list of user-defined type-checking plugins for constraint solving.

        tcg_tc_plugin_rewriters :: UniqFM TyCon [TcPluginRewriter],
        -- ^ A collection of all the user-defined type-checking plugins for rewriting
        -- type family applications, collated by their type family 'TyCon's.

        tcg_defaulting_plugins :: [FillDefaulting],
        -- ^ A list of user-defined plugins for type defaulting plugins.

        tcg_hf_plugins :: [HoleFitPlugin],
        -- ^ A list of user-defined plugins for hole fit suggestions.

        tcg_top_loc :: RealSrcSpan,
        -- ^ The RealSrcSpan this module came from

        tcg_static_wc :: TcRef WantedConstraints,
          -- ^ Wanted constraints of static forms.
        -- See Note [Constraints in static forms].
        tcg_complete_matches :: !CompleteMatches,

        -- ^ Tracking indices for cost centre annotations
        tcg_cc_st   :: TcRef CostCentreState,

        tcg_next_wrapper_num :: TcRef (ModuleEnv Int)
        -- ^ See Note [Generating fresh names for FFI wrappers]
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

data SelfBootInfo
  = NoSelfBoot    -- No corresponding hi-boot file
  | SelfBoot
       { sb_mds :: ModDetails }  -- There was a hi-boot file

bootExports :: SelfBootInfo -> NameSet
bootExports boot =
  case boot of
    NoSelfBoot -> emptyNameSet
    SelfBoot { sb_mds = mds} ->
      let exports = md_exports mds
      in availsToNameSet exports



{- Note [Tracking unused binding and imports]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We gather three sorts of usage information

 * tcg_dus :: DefUses (defs/uses)
      Records what is defined in this module and what is used.

      Records *defined* Names (local, top-level)
          and *used*    Names (local or imported)

      Used (a) to report "defined but not used"
               (see GHC.Rename.Names.reportUnusedNames)
           (b) to generate version-tracking usage info in interface
               files (see GHC.Iface.Make.mkUsedNames)
   This usage info is mainly gathered by the renamer's
   gathering of free-variables

 * tcg_used_gres :: TcRef [GlobalRdrElt]
      Records occurrences of imported entities.

      Used only to report unused import declarations

      Records each *occurrence* an *imported* (not locally-defined) entity.
      The occurrence is recorded by keeping a GlobalRdrElt for it.
      These is not the GRE that is in the GlobalRdrEnv; rather it
      is recorded *after* the filtering done by pickGREs.  So it reflect
      /how that occurrence is in scope/.   See Note [GRE filtering] in
      RdrName.

  * tcg_keep :: TcRef NameSet
      Records names of the type constructors, data constructors, and Ids that
      are used by the constraint solver.

      The typechecker may use find that some imported or
      locally-defined things are used, even though they
      do not appear to be mentioned in the source code:

      (a) The to/from functions for generic data types

      (b) Top-level variables appearing free in the RHS of an
          orphan rule

      (c) Top-level variables appearing free in a TH bracket
          See Note [Keeping things alive for Template Haskell]
          in GHC.Rename.Splice

      (d) The data constructor of a newtype that is used
          to solve a Coercible instance (e.g. #10347). Example
              module T10347 (N, mkN) where
                import Data.Coerce
                newtype N a = MkN Int
                mkN :: Int -> N a
                mkN = coerce

          Then we wish to record `MkN` as used, since it is (morally)
          used to perform the coercion in `mkN`. To do so, the
          Coercible solver updates tcg_keep's TcRef whenever it
          encounters a use of `coerce` that crosses newtype boundaries.

      (e) Record fields that are used to solve HasField constraints
          (see Note [Unused name reporting and HasField] in GHC.Tc.Instance.Class)

      The tcg_keep field is used in two distinct ways:

      * Desugar.addExportFlagsAndRules.  Where things like (a-c) are locally
        defined, we should give them an Exported flag, so that the
        simplifier does not discard them as dead code, and so that they are
        exposed in the interface file (but not to export to the user).

      * GHC.Rename.Names.reportUnusedNames.  Where newtype data constructors
        like (d) are imported, we don't want to report them as unused.
-}


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


-- | Get target platform
getPlatform :: TcRnIf a b Platform
getPlatform = targetPlatform <$> getDynFlags


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

{-
************************************************************************
*                                                                      *
        Operations over ImportAvails
*                                                                      *
************************************************************************
-}


mkModDeps :: Set (UnitId, ModuleNameWithIsBoot)
          -> InstalledModuleEnv ModuleNameWithIsBoot
mkModDeps deps = S.foldl' add emptyInstalledModuleEnv deps
  where
    add env (uid, elt) = extendInstalledModuleEnv env (mkModule uid (gwib_mod elt)) elt

plusModDeps :: InstalledModuleEnv ModuleNameWithIsBoot
            -> InstalledModuleEnv ModuleNameWithIsBoot
            -> InstalledModuleEnv ModuleNameWithIsBoot
plusModDeps = plusInstalledModuleEnv plus_mod_dep
  where
    plus_mod_dep r1@(GWIB { gwib_mod = m1, gwib_isBoot = boot1 })
                 r2@(GWIB {gwib_mod = m2, gwib_isBoot = boot2})
      | assertPpr (m1 == m2) ((ppr m1 <+> ppr m2) $$ (ppr (boot1 == IsBoot) <+> ppr (boot2 == IsBoot)))
        boot1 == IsBoot = r2
      | otherwise = r1
      -- If either side can "see" a non-hi-boot interface, use that
      -- Reusing existing tuples saves 10% of allocations on test
      -- perf/compiler/MultiLayerModules

emptyImportAvails :: ImportAvails
emptyImportAvails = ImportAvails { imp_mods          = M.empty,
                                   imp_direct_dep_mods = emptyInstalledModuleEnv,
                                   imp_dep_direct_pkgs = S.empty,
                                   imp_sig_mods      = [],
                                   imp_trust_pkgs    = S.empty,
                                   imp_trust_own_pkg = False,
                                   imp_boot_mods   = emptyInstalledModuleEnv,
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
                  imp_direct_dep_mods = ddmods1,
                  imp_dep_direct_pkgs = ddpkgs1,
                  imp_boot_mods = srs1,
                  imp_sig_mods = sig_mods1,
                  imp_trust_pkgs = tpkgs1, imp_trust_own_pkg = tself1,
                  imp_orphs = orphs1, imp_finsts = finsts1 })
  (ImportAvails { imp_mods = mods2,
                  imp_direct_dep_mods = ddmods2,
                  imp_dep_direct_pkgs = ddpkgs2,
                  imp_boot_mods = srcs2,
                  imp_sig_mods = sig_mods2,
                  imp_trust_pkgs = tpkgs2, imp_trust_own_pkg = tself2,
                  imp_orphs = orphs2, imp_finsts = finsts2 })
  = ImportAvails { imp_mods          = M.unionWith (++) mods1 mods2,
                   imp_direct_dep_mods = ddmods1 `plusModDeps` ddmods2,
                   imp_dep_direct_pkgs      = ddpkgs1 `S.union` ddpkgs2,
                   imp_trust_pkgs    = tpkgs1 `S.union` tpkgs2,
                   imp_trust_own_pkg = tself1 || tself2,
                   imp_boot_mods   = srs1 `plusModDeps` srcs2,
                   imp_sig_mods      = unionListsOrd sig_mods1 sig_mods2,
                   imp_orphs         = unionListsOrd orphs1 orphs2,
                   imp_finsts        = unionListsOrd finsts1 finsts2 }


{-
Constraint Solver Plugins
-------------------------
-}

-- | The @solve@ function of a type-checking plugin takes in Given
-- and Wanted constraints, and should return a 'TcPluginSolveResult'
-- indicating which Wanted constraints it could solve, or whether any are
-- insoluble.
type TcPluginSolver = EvBindsVar
                   -> [Ct] -- ^ Givens
                   -> [Ct] -- ^ Wanteds
                   -> TcPluginM TcPluginSolveResult

-- | For rewriting type family applications, a type-checking plugin provides
-- a function of this type for each type family 'TyCon'.
--
-- The function is provided with the current set of Given constraints, together
-- with the arguments to the type family.
-- The type family application will always be fully saturated.
type TcPluginRewriter
  =  RewriteEnv -- ^ Rewriter environment
  -> [Ct]       -- ^ Givens
  -> [TcType]   -- ^ type family arguments
  -> TcPluginM TcPluginRewriteResult

-- | 'TcPluginM' is the monad in which type-checking plugins operate.
newtype TcPluginM a = TcPluginM { runTcPluginM :: TcM a }
  deriving newtype (Functor, Applicative, Monad, MonadFail)

-- | This function provides an escape for direct access to
-- the 'TcM` monad.  It should not be used lightly, and
-- the provided 'TcPluginM' API should be favoured instead.
unsafeTcPluginTcM :: TcM a -> TcPluginM a
unsafeTcPluginTcM = TcPluginM

data TcPlugin = forall s. TcPlugin
  { tcPluginInit :: TcPluginM s
    -- ^ Initialize plugin, when entering type-checker.

  , tcPluginSolve :: s -> TcPluginSolver
    -- ^ Solve some constraints.
    --
    -- This function will be invoked at two points in the constraint solving
    -- process: once to simplify Given constraints, and once to solve
    -- Wanted constraints. In the first case (and only in the first case),
    -- no Wanted constraints will be passed to the plugin.
    --
    -- The plugin can either return a contradiction,
    -- or specify that it has solved some constraints (with evidence),
    -- and possibly emit additional constraints. These returned constraints
    -- must be Givens in the first case, and Wanteds in the second.
    --
    -- Use @ \\ _ _ _ _ -> pure $ TcPluginOk [] [] @ if your plugin
    -- does not provide this functionality.

  , tcPluginRewrite :: s -> UniqFM TyCon TcPluginRewriter
    -- ^ Rewrite saturated type family applications.
    --
    -- The plugin is expected to supply a mapping from type family names to
    -- rewriting functions. For each type family 'TyCon', the plugin should
    -- provide a function which takes in the given constraints and arguments
    -- of a saturated type family application, and return a possible rewriting.
    -- See 'TcPluginRewriter' for the expected shape of such a function.
    --
    -- Use @ \\ _ -> emptyUFM @ if your plugin does not provide this functionality.

  , tcPluginStop :: s -> TcPluginM ()
   -- ^ Clean up after the plugin, when exiting the type-checker.
  }

-- | The plugin found a contradiction.
-- The returned constraints are removed from the inert set,
-- and recorded as insoluble.
--
-- The returned list of constraints should never be empty.
pattern TcPluginContradiction :: [Ct] -> TcPluginSolveResult
pattern TcPluginContradiction insols
  = TcPluginSolveResult
  { tcPluginInsolubleCts = insols
  , tcPluginSolvedCts    = []
  , tcPluginNewCts       = [] }

-- | The plugin has not found any contradictions,
--
-- The first field is for constraints that were solved.
-- The second field contains new work, that should be processed by
-- the constraint solver.
pattern TcPluginOk :: [(EvTerm, Ct)] -> [Ct] -> TcPluginSolveResult
pattern TcPluginOk solved new
  = TcPluginSolveResult
  { tcPluginInsolubleCts = []
  , tcPluginSolvedCts    = solved
  , tcPluginNewCts       = new }

-- | Result of running a solver plugin.
data TcPluginSolveResult
  = TcPluginSolveResult
  { -- | Insoluble constraints found by the plugin.
    --
    -- These constraints will be added to the inert set,
    -- and reported as insoluble to the user.
    tcPluginInsolubleCts :: [Ct]
    -- | Solved constraints, together with their evidence.
    --
    -- These are removed from the inert set, and the
    -- evidence for them is recorded.
  , tcPluginSolvedCts :: [(EvTerm, Ct)]
    -- | New constraints that the plugin wishes to emit.
    --
    -- These will be added to the work list.
  , tcPluginNewCts :: [Ct]
  }

data TcPluginRewriteResult
  =
  -- | The plugin does not rewrite the type family application.
    TcPluginNoRewrite

  -- | The plugin rewrites the type family application
  -- providing a rewriting together with evidence: a 'Reduction',
  -- which contains the rewritten type together with a 'Coercion'
  -- whose right-hand-side type is the rewritten type.
  --
  -- The plugin can also emit additional Wanted constraints.
  | TcPluginRewriteTo
    { tcPluginReduction    :: !Reduction
    , tcRewriterNewWanteds :: [Ct]
    }

-- | A collection of candidate default types for sets of type variables.
data DefaultingProposal
  = DefaultingProposal
    { deProposals :: [[(TcTyVar, Type)]]
      -- ^ The type variable assignments to try.
    , deProposalCts :: [Ct]
      -- ^ The constraints against which defaults are checked.
    }

instance Outputable DefaultingProposal where
  ppr p = text "DefaultingProposal"
          <+> ppr (deProposals p)
          <+> ppr (deProposalCts p)

type FillDefaulting
  = WantedConstraints
      -- Zonked constraints containing the unfilled metavariables that
      -- can be defaulted. See wrinkle (DP1) of Note [Defaulting plugins]
      -- in GHC.Tc.Solver
  -> TcPluginM [DefaultingProposal]

-- | A plugin for controlling defaulting.
data DefaultingPlugin = forall s. DefaultingPlugin
  { dePluginInit :: TcPluginM s
    -- ^ Initialize plugin, when entering type-checker.
  , dePluginRun :: s -> FillDefaulting
    -- ^ Default some types
  , dePluginStop :: s -> TcPluginM ()
   -- ^ Clean up after the plugin, when exiting the type-checker.
  }

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

getRoleAnnots :: [Name] -> RoleAnnotEnv -> [LRoleAnnotDecl GhcRn]
getRoleAnnots bndrs role_env
  = mapMaybe (lookupRoleAnnot role_env) bndrs

{- *********************************************************************
*                                                                      *
                  Linting a TcGblEnv
*                                                                      *
********************************************************************* -}

-- | Check the 'TcGblEnv' for consistency. Currently, only checks
-- axioms, but should check other aspects, too.
lintGblEnv :: Logger -> DynFlags -> TcGblEnv -> TcM ()
lintGblEnv logger dflags tcg_env =
  -- TODO empty list means no extra in scope from GHCi, is this correct?
  liftIO $ lintAxioms logger (initLintConfig dflags []) (text "TcGblEnv axioms") axioms
  where
    axioms = typeEnvCoAxioms (tcg_type_env tcg_env)

-- | This is a mirror of Template Haskell's DocLoc, but the TH names are
-- resolved to GHC names.
data DocLoc = DeclDoc Name
            | ArgDoc Name Int
            | InstDoc Name
            | ModuleDoc
  deriving (Eq, Ord)

-- | The current collection of docs that Template Haskell has built up via
-- putDoc.
type THDocs = Map DocLoc (HsDoc GhcRn)
