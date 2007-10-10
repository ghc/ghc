
% (c) The University of Glasgow 2006
% (c) The GRASP Project, Glasgow University, 1992-2002
%
\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module TcRnTypes(
	TcRnIf, TcRn, TcM, RnM,	IfM, IfL, IfG, -- The monad is opaque outside this module
	TcRef,

	-- The environment types
	Env(..), 
	TcGblEnv(..), TcLclEnv(..), 
	IfGblEnv(..), IfLclEnv(..), 

	-- Ranamer types
	ErrCtxt, RecFieldEnv,
	ImportAvails(..), emptyImportAvails, plusImportAvails, 
	WhereFrom(..), mkModDeps,

	-- Typechecker types
	TcTyThing(..), pprTcTyThingCategory, RefinementVisibility(..),

	-- Template Haskell
	ThStage(..), topStage, topSpliceStage,
	ThLevel, impLevel, topLevel,

	-- Arrows
	ArrowCtxt(NoArrowCtxt), newArrowScope, escapeArrowScope,

	-- Insts
	Inst(..), InstOrigin(..), InstLoc(..), 
	pprInstLoc, pprInstArising, instLocSpan, instLocOrigin,
	LIE, emptyLIE, unitLIE, plusLIE, consLIE, instLoc, instSpan,
	plusLIEs, mkLIE, isEmptyLIE, lieToList, listToLIE,

	-- Misc other types
	TcId, TcIdSet, TcDictBinds,
	
  ) where

#include "HsVersions.h"

import HsSyn hiding (LIE)
import HscTypes
import Packages
import Type
import Coercion
import TcType
import TcGadt
import InstEnv
import FamInstEnv
import IOEnv
import RdrName
import Name
import NameEnv
import NameSet
import Var
import VarEnv
import Module
import UniqFM
import SrcLoc
import VarSet
import ErrUtils
import UniqSupply
import BasicTypes
import Util
import Bag
import Outputable
import ListSetOps
import FiniteMap

import Data.Maybe
import Data.List
\end{code}


%************************************************************************
%*									*
	       Standard monad definition for TcRn
    All the combinators for the monad can be found in TcRnMonad
%*									*
%************************************************************************

The monad itself has to be defined here, because it is mentioned by ErrCtxt

\begin{code}
type TcRef a 	 = IORef a
type TcId    	 = Id 			-- Type may be a TcType
type TcIdSet 	 = IdSet
type TcDictBinds = DictBinds TcId	-- Bag of dictionary bindings

type TcRnIf a b c = IOEnv (Env a b) c
type IfM lcl a  = TcRnIf IfGblEnv lcl a		-- Iface stuff

type IfG a  = IfM () a				-- Top level
type IfL a  = IfM IfLclEnv a			-- Nested
type TcRn a = TcRnIf TcGblEnv TcLclEnv a
type RnM  a = TcRn a		-- Historical
type TcM  a = TcRn a		-- Historical
\end{code}


%************************************************************************
%*									*
		The main environment types
%*									*
%************************************************************************

\begin{code}
data Env gbl lcl	-- Changes as we move into an expression
  = Env {
	env_top	 :: HscEnv,	-- Top-level stuff that never changes
				-- Includes all info about imported things

	env_us   :: {-# UNPACK #-} !(IORef UniqSupply),	
				-- Unique supply for local varibles

	env_gbl  :: gbl,	-- Info about things defined at the top level
				-- of the module being compiled

	env_lcl  :: lcl	 	-- Nested stuff; changes as we go into 
    }

-- TcGblEnv describes the top-level of the module at the 
-- point at which the typechecker is finished work.
-- It is this structure that is handed on to the desugarer

data TcGblEnv
  = TcGblEnv {
	tcg_mod     :: Module,		-- Module being compiled
	tcg_src     :: HscSource,	-- What kind of module 
					-- (regular Haskell, hs-boot, ext-core)

	tcg_rdr_env :: GlobalRdrEnv,	-- Top level envt; used during renaming
	tcg_default :: Maybe [Type],	-- Types used for defaulting
					-- Nothing => no 'default' decl

	tcg_fix_env   :: FixityEnv,	-- Just for things in this module
	tcg_field_env :: RecFieldEnv,	-- Just for things in this module

	tcg_type_env :: TypeEnv,	-- Global type env for the module we are compiling now
		-- All TyCons and Classes (for this module) end up in here right away,
		-- along with their derived constructors, selectors.
		--
		-- (Ids defined in this module start in the local envt, 
		--  though they move to the global envt during zonking)

	tcg_type_env_var :: TcRef TypeEnv,
		-- Used only to initialise the interface-file
		-- typechecker in initIfaceTcRn, so that it can see stuff
		-- bound in this module when dealing with hi-boot recursions
		-- Updated at intervals (e.g. after dealing with types and classes)
	
	tcg_inst_env     :: InstEnv,	-- Instance envt for *home-package* 
					-- modules; Includes the dfuns in 
					-- tcg_insts
	tcg_fam_inst_env :: FamInstEnv,	-- Ditto for family instances

		-- Now a bunch of things about this module that are simply 
		-- accumulated, but never consulted until the end.  
		-- Nevertheless, it's convenient to accumulate them along 
		-- with the rest of the info from this module.
	tcg_exports :: [AvailInfo],	-- What is exported
	tcg_imports :: ImportAvails,	-- Information about what was imported 
					--    from where, including things bound
					--    in this module

	tcg_dus :: DefUses,  	-- What is defined in this module and what is used.
				-- The latter is used to generate 
				--	(a) version tracking; no need to recompile if these
				--		things have not changed version stamp
				-- 	(b) unused-import info

	tcg_keep :: TcRef NameSet,	-- Locally-defined top-level names to keep alive
		-- "Keep alive" means give them an Exported flag, so
		-- that the simplifier does not discard them as dead 
		-- code, and so that they are exposed in the interface file
		-- (but not to export to the user).
		--
		-- Some things, like dict-fun Ids and default-method Ids are 
		-- "born" with the Exported flag on, for exactly the above reason,
		-- but some we only discover as we go.  Specifically:
		--	* The to/from functions for generic data types
		--	* Top-level variables appearing free in the RHS of an orphan rule
		--	* Top-level variables appearing free in a TH bracket

	tcg_inst_uses :: TcRef NameSet,	-- Home-package Dfuns actually used 
		-- Used to generate version dependencies
		-- This records usages, rather like tcg_dus, but it has to
		-- be a mutable variable so it can be augmented 
		-- when we look up an instance.  These uses of dfuns are
		-- rather like the free variables of the program, but
		-- are implicit instead of explicit.

	tcg_th_used :: TcRef Bool,	-- True <=> Template Haskell syntax used
		-- We need this so that we can generate a dependency on the
		-- Template Haskell package, becuase the desugarer is going to
		-- emit loads of references to TH symbols.  It's rather like 
		-- tcg_inst_uses; the reference is implicit rather than explicit,
		-- so we have to zap a mutable variable.

	tcg_dfun_n  :: TcRef Int,	-- Allows us to number off the names of DFuns
		-- It's convenient to allocate an External Name for a DFun, with
		-- a permanently-fixed unique, just like other top-level functions
		-- defined in this module.  But that means we need a canonical 
		-- occurrence name, distinct from all other dfuns in this module,
		-- and this name supply serves that purpose (df1, df2, etc).

		-- The next fields accumulate the payload of the module
		-- The binds, rules and foreign-decl fiels are collected
		-- initially in un-zonked form and are finally zonked in tcRnSrcDecls

		-- The next fields accumulate the payload of the
		-- module The binds, rules and foreign-decl fiels are
		-- collected initially in un-zonked form and are
		-- finally zonked in tcRnSrcDecls

        tcg_rn_imports :: Maybe [LImportDecl Name],
        tcg_rn_exports :: Maybe [Located (IE Name)],
	tcg_rn_decls :: Maybe (HsGroup Name),	-- renamed decls, maybe
		-- Nothing <=> Don't retain renamed decls

	tcg_binds     :: LHsBinds Id,	    -- Value bindings in this module
	tcg_deprecs   :: Deprecations,	    -- ...Deprecations 
	tcg_insts     :: [Instance],	    -- ...Instances
	tcg_fam_insts :: [FamInst],	    -- ...Family instances
	tcg_rules     :: [LRuleDecl Id],    -- ...Rules
	tcg_fords     :: [LForeignDecl Id], -- ...Foreign import & exports

	tcg_doc :: Maybe (HsDoc Name), -- Maybe Haddock documentation
        tcg_hmi :: HaddockModInfo Name, -- Haddock module information
        tcg_hpc :: AnyHpcUsage -- True if any part of the prog uses hpc instrumentation.
    }

type RecFieldEnv = NameEnv [Name]	-- Maps a constructor name *in this module*
					-- to the fields for that constructor
	-- This is used when dealing with ".." notation in record 
	-- construction and pattern matching.
	-- The FieldEnv deals *only* with constructors defined in
	-- *thie* module.  For imported modules, we get the same info
	-- from the TypeEnv
\end{code}

%************************************************************************
%*									*
		The interface environments
  	      Used when dealing with IfaceDecls
%*									*
%************************************************************************

\begin{code}
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
		--	.hi file, or GHCi state, or ext core
		-- plus which bit is currently being examined

	if_tv_env  :: UniqFM TyVar,	-- Nested tyvar bindings
	if_id_env  :: UniqFM Id		-- Nested id binding
    }
\end{code}


%************************************************************************
%*									*
		The local typechecker environment
%*									*
%************************************************************************

The Global-Env/Local-Env story
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

\begin{code}
data TcLclEnv		-- Changes as we move inside an expression
			-- Discarded after typecheck/rename; not passed on to desugarer
  = TcLclEnv {
	tcl_loc  :: SrcSpan,		-- Source span
	tcl_ctxt :: ErrCtxt,		-- Error context
	tcl_errs :: TcRef Messages,	-- Place to accumulate errors

	tcl_th_ctxt    :: ThStage,	-- Template Haskell context
	tcl_arrow_ctxt :: ArrowCtxt,	-- Arrow-notation context

	tcl_rdr :: LocalRdrEnv,		-- Local name envt
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

	tcl_env  :: NameEnv TcTyThing,  -- The local type environment: Ids and TyVars
					-- defined in this module
					
	tcl_tyvars :: TcRef TcTyVarSet,	-- The "global tyvars"
			-- Namely, the in-scope TyVars bound in tcl_env, 
			-- plus the tyvars mentioned in the types of Ids bound in tcl_lenv
			-- Why mutable? see notes with tcGetGlobalTyVars

	tcl_lie   :: TcRef LIE		-- Place to accumulate type constraints
    }


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

---------------------------
-- Template Haskell levels 
---------------------------

type ThLevel = Int	
	-- Indicates how many levels of brackets we are inside
	-- 	(always >= 0)
	-- Incremented when going inside a bracket,
	-- decremented when going inside a splice
	-- NB: ThLevel is one greater than the 'n' in Fig 2 of the
	--     original "Template meta-programmign for Haskell" paper

impLevel, topLevel :: ThLevel
topLevel = 1	-- Things defined at top level of this module
impLevel = 0	-- Imported things; they can be used inside a top level splice
--
-- For example: 
--	f = ...
--	g1 = $(map ...)		is OK
--	g2 = $(f ...)		is not OK; because we havn't compiled f yet


data ThStage
  = Comp   				-- Ordinary compiling, at level topLevel
  | Splice ThLevel 			-- Inside a splice
  | Brack  ThLevel 			-- Inside brackets; 
	   (TcRef [PendingSplice])	--   accumulate pending splices here
	   (TcRef LIE)			--   and type constraints here
topStage, topSpliceStage :: ThStage
topStage       = Comp
topSpliceStage = Splice (topLevel - 1)	-- Stage for the body of a top-level splice

---------------------------
-- Arrow-notation context
---------------------------

{-
In arrow notation, a variable bound by a proc (or enclosed let/kappa)
is not in scope to the left of an arrow tail (-<) or the head of (|..|).
For example

	proc x -> (e1 -< e2)

Here, x is not in scope in e1, but it is in scope in e2.  This can get
a bit complicated:

	let x = 3 in
	proc y -> (proc z -> e1) -< e2

Here, x and z are in scope in e1, but y is not.  We implement this by
recording the environment when passing a proc (using newArrowScope),
and returning to that (using escapeArrowScope) on the left of -< and the
head of (|..|).
-}

data ArrowCtxt
  = NoArrowCtxt
  | ArrowCtxt (Env TcGblEnv TcLclEnv)

-- Record the current environment (outside a proc)
newArrowScope :: TcM a -> TcM a
newArrowScope
  = updEnv $ \env ->
	env { env_lcl = (env_lcl env) { tcl_arrow_ctxt = ArrowCtxt env } }

-- Return to the stored environment (from the enclosing proc)
escapeArrowScope :: TcM a -> TcM a
escapeArrowScope
  = updEnv $ \ env -> case tcl_arrow_ctxt (env_lcl env) of
	NoArrowCtxt -> env
	ArrowCtxt env' -> env'

---------------------------
-- TcTyThing
---------------------------

data TcTyThing
  = AGlobal TyThing		-- Used only in the return type of a lookup

  | ATcId   {		-- Ids defined in this module; may not be fully zonked
	tct_id :: TcId,		
	tct_co :: RefinementVisibility,	-- Previously: Maybe HsWrapper
					-- Nothing <=>	Do not apply a GADT type refinement
					--		I am wobbly, or have no free
					--		type variables
					-- Just co <=>  Apply any type refinement to me,
					--		and record it in the coercion
	tct_type  :: TcType,	-- Type of (coercion applied to id)
	tct_level :: ThLevel }

  | ATyVar  Name TcType		-- The type to which the lexically scoped type vaiable
				-- is currently refined. We only need the Name
				-- for error-message purposes

  | AThing  TcKind 		-- Used temporarily, during kind checking, for the
				--	tycons and clases in this recursive group

data RefinementVisibility
  = Unrefineable			-- Do not apply a GADT refinement
					-- I have no free variables	

  | Rigid HsWrapper			-- Apply any refinement to me
					-- and record it in the coercion

  | Wobbly				-- Do not apply a GADT refinement
					-- I am wobbly

  | WobblyInvisible			-- Wobbly type, not available inside current
					-- GADT refinement

instance Outputable TcTyThing where	-- Debugging only
   ppr (AGlobal g)      = pprTyThing g
   ppr elt@(ATcId {})   = text "Identifier" <> 
			  ifPprDebug (brackets (ppr (tct_id elt) <> dcolon <> ppr (tct_type elt) <> comma
				 <+> ppr (tct_level elt) <+> ppr (tct_co elt)))
   ppr (ATyVar tv _)    = text "Type variable" <+> quotes (ppr tv)
   ppr (AThing k)       = text "AThing" <+> ppr k

pprTcTyThingCategory :: TcTyThing -> SDoc
pprTcTyThingCategory (AGlobal thing) = pprTyThingCategory thing
pprTcTyThingCategory (ATyVar {})     = ptext SLIT("Type variable")
pprTcTyThingCategory (ATcId {})      = ptext SLIT("Local identifier")
pprTcTyThingCategory (AThing {})     = ptext SLIT("Kinded thing")

instance Outputable RefinementVisibility where
    ppr Unrefineable	      = ptext SLIT("unrefineable")
    ppr (Rigid co)	      = ptext SLIT("rigid") <+> ppr co
    ppr	Wobbly		      = ptext SLIT("wobbly")
    ppr WobblyInvisible	      = ptext SLIT("wobbly-invisible")

\end{code}

\begin{code}
type ErrCtxt = [TidyEnv -> TcM (TidyEnv, Message)]	
			-- Innermost first.  Monadic so that we have a chance
			-- to deal with bound type variables just before error
			-- message construction
\end{code}


%************************************************************************
%*									*
	Operations over ImportAvails
%*									*
%************************************************************************

ImportAvails summarises what was imported from where, irrespective
of whether the imported things are actually used or not
It is used 	* when processing the export list
		* when constructing usage info for the inteface file
		* to identify the list of directly imported modules
			for initialisation purposes and
			for optimsed overlap checking of family instances
		* when figuring out what things are really unused

\begin{code}
data ImportAvails 
   = ImportAvails {
	imp_mods :: ModuleEnv (Module, [(ModuleName, Bool, SrcSpan)]),
		-- Domain is all directly-imported modules
        -- The ModuleName is what the module was imported as, e.g. in
        --     import Foo as Bar
        -- it is Bar.
		-- Bool means:
		--   True => import was "import Foo ()"
		--   False  => import was some other form
		--
		-- We need the Module in the range because we can't get
		-- 	the keys of a ModuleEnv
		-- Used 
		--   (a) to help construct the usage information in 
		--       the interface file; if we import somethign we
		--       need to recompile if the export version changes
		--   (b) to specify what child modules to initialise
                --
                -- We need a full ModuleEnv rather than a ModuleNameEnv
                -- here, because we might be importing modules of the
                -- same name from different packages. (currently not the case,
                -- but might be in the future).

	imp_dep_mods :: ModuleNameEnv (ModuleName, IsBootInterface),
		-- Home-package modules needed by the module being compiled
		--
		-- It doesn't matter whether any of these dependencies
		-- are actually *used* when compiling the module; they
		-- are listed if they are below it at all.  For
		-- example, suppose M imports A which imports X.  Then
		-- compiling M might not need to consult X.hi, but X
		-- is still listed in M's dependencies.

	imp_dep_pkgs :: [PackageId],
		-- Packages needed by the module being compiled, whether
		-- directly, or via other modules in this package, or via
		-- modules imported from other packages.

 	imp_orphs :: [Module],
		-- Orphan modules below us in the import tree (and maybe
		-- including us for imported modules) 

 	imp_finsts :: [Module]
		-- Family instance modules below us in the import tree  (and
		-- maybe including us for imported modules)
      }

mkModDeps :: [(ModuleName, IsBootInterface)]
	  -> ModuleNameEnv (ModuleName, IsBootInterface)
mkModDeps deps = foldl add emptyUFM deps
	       where
		 add env elt@(m,_) = addToUFM env m elt

emptyImportAvails :: ImportAvails
emptyImportAvails = ImportAvails { imp_mods   	= emptyModuleEnv,
				   imp_dep_mods = emptyUFM,
				   imp_dep_pkgs = [],
				   imp_orphs    = [],
				   imp_finsts   = [] }

plusImportAvails ::  ImportAvails ->  ImportAvails ->  ImportAvails
plusImportAvails
  (ImportAvails { imp_mods = mods1,
		  imp_dep_mods = dmods1, imp_dep_pkgs = dpkgs1, 
                  imp_orphs = orphs1, imp_finsts = finsts1 })
  (ImportAvails { imp_mods = mods2,
		  imp_dep_mods = dmods2, imp_dep_pkgs = dpkgs2,
                  imp_orphs = orphs2, imp_finsts = finsts2 })
  = ImportAvails { imp_mods     = plusModuleEnv_C plus_mod mods1 mods2,	
		   imp_dep_mods = plusUFM_C plus_mod_dep dmods1 dmods2,	
		   imp_dep_pkgs = dpkgs1 `unionLists` dpkgs2,
		   imp_orphs    = orphs1 `unionLists` orphs2,
		   imp_finsts   = finsts1 `unionLists` finsts2 }
  where
    plus_mod (m1, xs1) (_, xs2) = (m1, xs1 ++ xs2)
    plus_mod_dep (m1, boot1) (m2, boot2) 
	= WARN( not (m1 == m2), (ppr m1 <+> ppr m2) $$ (ppr boot1 <+> ppr boot2) )
		-- Check mod-names match
	  (m1, boot1 && boot2)	-- If either side can "see" a non-hi-boot interface, use that
\end{code}

%************************************************************************
%*									*
\subsection{Where from}
%*									*
%************************************************************************

The @WhereFrom@ type controls where the renamer looks for an interface file

\begin{code}
data WhereFrom 
  = ImportByUser IsBootInterface	-- Ordinary user import (perhaps {-# SOURCE #-})
  | ImportBySystem			-- Non user import.

instance Outputable WhereFrom where
  ppr (ImportByUser is_boot) | is_boot     = ptext SLIT("{- SOURCE -}")
			     | otherwise   = empty
  ppr ImportBySystem     		   = ptext SLIT("{- SYSTEM -}")
\end{code}


%************************************************************************
%*									*
\subsection[Inst-types]{@Inst@ types}
%*									*
v%************************************************************************

An @Inst@ is either a dictionary, an instance of an overloaded
literal, or an instance of an overloaded value.  We call the latter a
``method'' even though it may not correspond to a class operation.
For example, we might have an instance of the @double@ function at
type Int, represented by

	Method 34 doubleId [Int] origin

In addition to the basic Haskell variants of 'Inst's, they can now also
represent implication constraints 'forall tvs. (reft, given) => wanted'
and equality constraints 'co :: ty1 ~ ty2'.

NB: Equalities occur in two flavours:

  (1) Dict {tci_pred = EqPred ty1 ty2}
  (2) EqInst {tci_left = ty1, tci_right = ty2, tci_co = coe}

The former arises from equalities in contexts, whereas the latter is used
whenever the type checker introduces an equality (e.g., during deferring
unification).

I am not convinced that this duplication is necessary or useful! -=chak

\begin{code}
data Inst
  = Dict {
	tci_name :: Name,
	tci_pred :: TcPredType,
	tci_loc  :: InstLoc 
    }

  | ImplicInst {	-- An implication constraint
			-- forall tvs. (reft, given) => wanted
	tci_name   :: Name,
	tci_tyvars :: [TcTyVar],    -- Quantified type variables
				    -- Includes coercion variables
				    --   mentioned in tci_reft
	tci_reft   :: Refinement,
	tci_given  :: [Inst],	    -- Only Dicts
				    --   (no Methods, LitInsts, ImplicInsts)
	tci_wanted :: [Inst],	    -- Only Dicts and ImplicInsts
				    --   (no Methods or LitInsts)

	tci_loc    :: InstLoc
    }
	-- NB: the tci_given are not necessarily rigid,
	--     although they will be if the tci_reft is non-trivial
	-- NB: the tci_reft is already applied to tci_given and tci_wanted

  | Method {
	tci_id :: TcId,		-- The Id for the Inst

	tci_oid :: TcId,	-- The overloaded function
		-- This function will be a global, local, or ClassOpId;
		--   inside instance decls (only) it can also be an InstId!
		-- The id needn't be completely polymorphic.
		-- You'll probably find its name (for documentation purposes)
		--	  inside the InstOrigin

	tci_tys :: [TcType],	-- The types to which its polymorphic tyvars
				--	should be instantiated.
				-- These types must saturate the Id's foralls.

	tci_theta :: TcThetaType,	
			-- The (types of the) dictionaries to which the function
			-- must be applied to get the method

	tci_loc :: InstLoc 
    }
	-- INVARIANT 1: in (Method m f tys theta tau loc)
	--	type of m = type of (f tys dicts(from theta))

	-- INVARIANT 2: type of m must not be of form (Pred -> Tau)
	--   Reason: two methods are considered equal if the 
	--   	     base Id matches, and the instantiating types
	--	     match.  The TcThetaType should then match too.
	--   This only bites in the call to tcInstClassOp in TcClassDcl.mkMethodBind

  | LitInst {
	tci_name :: Name,
	tci_lit  :: HsOverLit Name,	-- The literal from the occurrence site
			-- INVARIANT: never a rebindable-syntax literal
			-- Reason: tcSyntaxName does unification, and we
			--	   don't want to deal with that during tcSimplify,
			--	   when resolving LitInsts

	tci_ty :: TcType,	-- The type at which the literal is used
	tci_loc :: InstLoc
    }

  | EqInst {			  -- delayed unification of the form 
				  --  	co :: ty1 ~ ty2
	tci_left  :: TcType,      -- ty1    -- both types are...
	tci_right :: TcType,      -- ty2    -- ...free of boxes
	tci_co    :: Either    	  -- co
			TcTyVar	  --  - a wanted equation, with a hole, to be 
				  --    filled with a witness for the equality;
                                  --    for equation arising from deferring
                                  --    unification, 'ty1' is the actual and
                                  --    'ty2' the expected type
			Coercion, --  - a given equation, with a coercion
				  --	witnessing the equality;
				  --    a coercion that originates from a
				  --    signature or a GADT is a CoVar, but
                                  --    after normalisation of coercions, they
				  --    can be arbitrary Coercions involving
                                  --    constructors and pseudo-constructors 
                                  --    like sym and trans.
	tci_loc   :: InstLoc,

	tci_name  :: Name	-- Debugging help only: this makes it easier to
				-- follow where a constraint is used in a morass
				-- of trace messages!  Unlike other Insts, it has
				-- no semantic significance whatsoever.
    }
\end{code}

@Insts@ are ordered by their class/type info, rather than by their
unique.  This allows the context-reduction mechanism to use standard finite
maps to do their stuff.  It's horrible that this code is here, rather
than with the Avails handling stuff in TcSimplify

\begin{code}
instance Ord Inst where
  compare = cmpInst

instance Eq Inst where
  (==) i1 i2 = case i1 `cmpInst` i2 of
	         EQ    -> True
		 other -> False

cmpInst d1@(Dict {}) 	d2@(Dict {})	= tci_pred d1 `tcCmpPred` tci_pred d2
cmpInst (Dict {})	other 		= LT

cmpInst (Method {}) 	(Dict {})	= GT
cmpInst m1@(Method {}) 	m2@(Method {})	= (tci_oid m1 `compare` tci_oid m2) `thenCmp`
					  (tci_tys m1 `tcCmpTypes` tci_tys m2)
cmpInst (Method {})  	other		= LT

cmpInst (LitInst {})	(Dict {}) 	= GT
cmpInst (LitInst {})	(Method {})	= GT
cmpInst l1@(LitInst {})	l2@(LitInst {})	= (tci_lit l1 `compare` tci_lit l2) `thenCmp`
					  (tci_ty l1 `tcCmpType` tci_ty l2)
cmpInst (LitInst {})  	other		= LT

	-- Implication constraints are compared by *name*
	-- not by type; that is, we make no attempt to do CSE on them
cmpInst (ImplicInst {})    (Dict {})	      = GT
cmpInst (ImplicInst {})    (Method {})	      = GT
cmpInst (ImplicInst {})    (LitInst {})	      = GT
cmpInst i1@(ImplicInst {}) i2@(ImplicInst {}) = tci_name i1 `compare` tci_name i2
cmpInst (ImplicInst {})    other	      = LT

	-- same for Equality constraints
cmpInst (EqInst {})    (Dict {})	      = GT
cmpInst (EqInst {})    (Method {})	      = GT
cmpInst (EqInst {})    (LitInst {})	      = GT
cmpInst (EqInst {})    (ImplicInst {})	      = GT
cmpInst i1@(EqInst {}) i2@(EqInst {})         = tci_name i1 `compare` tci_name i2
\end{code}


%************************************************************************
%*									*
\subsection[Inst-collections]{LIE: a collection of Insts}
%*									*
%************************************************************************

\begin{code}
-- FIXME: Rename this. It clashes with (Located (IE ...))
type LIE = Bag Inst

isEmptyLIE	  = isEmptyBag
emptyLIE          = emptyBag
unitLIE inst 	  = unitBag inst
mkLIE insts	  = listToBag insts
plusLIE lie1 lie2 = lie1 `unionBags` lie2
plusLIEs lies	  = unionManyBags lies
lieToList	  = bagToList
listToLIE	  = listToBag

consLIE inst lie  = lie `snocBag` inst
-- Putting the new Inst at the *end* of the bag is a half-hearted attempt
-- to ensure that we tend to report the *leftmost* type-constraint error
-- E.g. 	f :: [a]
--		f = [1,2,3]
-- we'd like to complain about the '1', not the '3'.
--
-- "Half-hearted" because the rest of the type checker makes no great
-- claims for retaining order in the constraint set.  Still, this 
-- seems to improve matters slightly.  Exampes: mdofail001, tcfail015
\end{code}


%************************************************************************
%*									*
\subsection[Inst-origin]{The @InstOrigin@ type}
%*									*
%************************************************************************

The @InstOrigin@ type gives information about where a dictionary came from.
This is important for decent error message reporting because dictionaries
don't appear in the original source code.  Doubtless this type will evolve...

It appears in TcMonad because there are a couple of error-message-generation
functions that deal with it.

\begin{code}
-------------------------------------------
data InstLoc = InstLoc InstOrigin SrcSpan ErrCtxt

instLoc :: Inst -> InstLoc
instLoc inst = tci_loc inst

instSpan :: Inst -> SrcSpan
instSpan wanted = instLocSpan (instLoc wanted)

instLocSpan :: InstLoc -> SrcSpan
instLocSpan (InstLoc _ s _) = s

instLocOrigin :: InstLoc -> InstOrigin
instLocOrigin (InstLoc o _ _) = o

pprInstArising :: Inst -> SDoc
pprInstArising loc = ptext SLIT("arising from") <+> pprInstLoc (tci_loc loc)

pprInstLoc :: InstLoc -> SDoc
pprInstLoc (InstLoc orig span _) = sep [ppr orig, text "at" <+> ppr span]

-------------------------------------------
data InstOrigin
  = SigOrigin SkolemInfo	-- Pattern, class decl, inst decl etc;
				-- Places that bind type variables and introduce
				-- available constraints

  | IPBindOrigin (IPName Name)	-- Binding site of an implicit parameter

	-------------------------------------------------------
	-- The rest are all occurrences: Insts that are 'wanted'
	-------------------------------------------------------
  | OccurrenceOf Name		-- Occurrence of an overloaded identifier

  | IPOccOrigin  (IPName Name)	-- Occurrence of an implicit parameter

  | LiteralOrigin (HsOverLit Name)	-- Occurrence of a literal

  | ArithSeqOrigin (ArithSeqInfo Name) -- [x..], [x..y] etc
  | PArrSeqOrigin  (ArithSeqInfo Name) -- [:x..y:] and [:x,y..z:]

  | InstSigOrigin	-- A dict occurrence arising from instantiating
			-- a polymorphic type during a subsumption check

  | RecordUpdOrigin
  | InstScOrigin	-- Typechecking superclasses of an instance declaration
  | DerivOrigin		-- Typechecking deriving
  | StandAloneDerivOrigin -- Typechecking stand-alone deriving
  | DefaultOrigin	-- Typechecking a default decl
  | DoOrigin		-- Arising from a do expression
  | ProcOrigin		-- Arising from a proc expression
  | ImplicOrigin SDoc	-- An implication constraint
  | EqOrigin		-- A type equality

instance Outputable InstOrigin where
    ppr (OccurrenceOf name)   = hsep [ptext SLIT("a use of"), quotes (ppr name)]
    ppr (IPOccOrigin name)    = hsep [ptext SLIT("a use of implicit parameter"), quotes (ppr name)]
    ppr (IPBindOrigin name)   = hsep [ptext SLIT("a binding for implicit parameter"), quotes (ppr name)]
    ppr RecordUpdOrigin       = ptext SLIT("a record update")
    ppr (LiteralOrigin lit)   = hsep [ptext SLIT("the literal"), quotes (ppr lit)]
    ppr (ArithSeqOrigin seq)  = hsep [ptext SLIT("the arithmetic sequence"), quotes (ppr seq)]
    ppr (PArrSeqOrigin seq)   = hsep [ptext SLIT("the parallel array sequence"), quotes (ppr seq)]
    ppr InstSigOrigin	      = ptext SLIT("instantiating a type signature")
    ppr InstScOrigin	      = ptext SLIT("the superclasses of an instance declaration")
    ppr DerivOrigin	      = ptext SLIT("the 'deriving' clause of a data type declaration")
    ppr StandAloneDerivOrigin = ptext SLIT("a 'deriving' declaration")
    ppr DefaultOrigin	      = ptext SLIT("a 'default' declaration")
    ppr DoOrigin	      = ptext SLIT("a do statement")
    ppr ProcOrigin	      = ptext SLIT("a proc expression")
    ppr (ImplicOrigin doc)    = doc
    ppr (SigOrigin info)      = pprSkolInfo info
    ppr EqOrigin	      = ptext SLIT("a type equality")

\end{code}
