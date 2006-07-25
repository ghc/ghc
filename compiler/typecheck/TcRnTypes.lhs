%
% (c) The GRASP Project, Glasgow University, 1992-2002
%
\begin{code}
module TcRnTypes(
	TcRnIf, TcRn, TcM, RnM,	IfM, IfL, IfG, -- The monad is opaque outside this module
	TcRef,

	-- The environment types
	Env(..), 
	TcGblEnv(..), TcLclEnv(..), 
	IfGblEnv(..), IfLclEnv(..), 

	-- Ranamer types
	ErrCtxt,
	ImportAvails(..), emptyImportAvails, plusImportAvails, 
	plusAvail, pruneAvails,  
	AvailEnv, emptyAvailEnv, unitAvailEnv, plusAvailEnv, 
	mkAvailEnv, lookupAvailEnv, lookupAvailEnv_maybe, availEnvElts, addAvail,
	WhereFrom(..), mkModDeps,

	-- Typechecker types
	TcTyThing(..), pprTcTyThingCategory, 
	GadtRefinement,

	-- Template Haskell
	ThStage(..), topStage, topSpliceStage,
	ThLevel, impLevel, topLevel,

	-- Arrows
	ArrowCtxt(NoArrowCtxt), newArrowScope, escapeArrowScope,

	-- Insts
	Inst(..), InstOrigin(..), InstLoc(..), pprInstLoc, 
	instLocSrcLoc, instLocSrcSpan,
	LIE, emptyLIE, unitLIE, plusLIE, consLIE, 
	plusLIEs, mkLIE, isEmptyLIE, lieToList, listToLIE,

	-- Misc other types
	TcId, TcIdSet, TcDictBinds
  ) where

#include "HsVersions.h"

import HsSyn		( PendingSplice, HsOverLit, LRuleDecl, LForeignDecl,
			  ArithSeqInfo, DictBinds, LHsBinds, LImportDecl, HsGroup,
                          IE )
import HscTypes		( FixityEnv,
			  HscEnv, TypeEnv, TyThing, 
			  GenAvailInfo(..), AvailInfo, HscSource(..),
			  availName, IsBootInterface, Deprecations )
import Packages		( PackageId )
import Type		( Type, pprTyThingCategory )
import TcType		( TcTyVarSet, TcType, TcThetaType, SkolemInfo, TvSubst,
			  TcPredType, TcKind, tcCmpPred, tcCmpType, tcCmpTypes, pprSkolInfo )
import InstEnv		( Instance, InstEnv )
import IOEnv
import RdrName		( GlobalRdrEnv, LocalRdrEnv )
import Name		( Name )
import NameEnv
import NameSet		( NameSet, unionNameSets, DefUses )
import Var		( Id, TyVar )
import VarEnv		( TidyEnv )
import Module
import UniqFM
import SrcLoc		( SrcSpan, SrcLoc, Located, srcSpanStart )
import VarSet		( IdSet )
import ErrUtils		( Messages, Message )
import UniqFM           ( UniqFM )
import UniqSupply	( UniqSupply )
import BasicTypes	( IPName )
import Util		( thenCmp )
import Bag
import Outputable
import Maybe		( mapMaybe )
import ListSetOps	( unionLists )
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

	env_lcl  :: lcl		-- Nested stuff; changes as we go into 
				-- an expression
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

	tcg_fix_env  :: FixityEnv,	-- Just for things in this module

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
	
	tcg_inst_env :: InstEnv,	-- Instance envt for *home-package* modules
					-- Includes the dfuns in tcg_insts
		-- Now a bunch of things about this module that are simply 
		-- accumulated, but never consulted until the end.  
		-- Nevertheless, it's convenient to accumulate them along 
		-- with the rest of the info from this module.
	tcg_exports :: NameSet,		-- What is exported
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

	tcg_binds   :: LHsBinds Id,		-- Value bindings in this module
	tcg_deprecs :: Deprecations,		-- ...Deprecations 
	tcg_insts   :: [Instance],		-- ...Instances
	tcg_rules   :: [LRuleDecl Id],		-- ...Rules
	tcg_fords   :: [LForeignDecl Id]	-- ...Foreign import & exports
    }
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

type GadtRefinement = TvSubst

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

  | ATcId   TcId 	 	-- Ids defined in this module; may not be fully zonked
	    ThLevel 
	    Bool		-- True <=> apply the type refinement to me

  | ATyVar  Name TcType		-- The type to which the lexically scoped type vaiable
				-- is currently refined. We only need the Name
				-- for error-message purposes

  | AThing  TcKind 		-- Used temporarily, during kind checking, for the
				--	tycons and clases in this recursive group

instance Outputable TcTyThing where	-- Debugging only
   ppr (AGlobal g)      = ppr g
   ppr (ATcId g tl rig) = text "Identifier" <> 
			  ifPprDebug (brackets (ppr g <> comma <> ppr tl <+> ppr rig))
   ppr (ATyVar tv _)    = text "Type variable" <+> quotes (ppr tv)
   ppr (AThing k)       = text "AThing" <+> ppr k

pprTcTyThingCategory :: TcTyThing -> SDoc
pprTcTyThingCategory (AGlobal thing) = pprTyThingCategory thing
pprTcTyThingCategory (ATyVar {})     = ptext SLIT("Type variable")
pprTcTyThingCategory (ATcId {})      = ptext SLIT("Local identifier")
pprTcTyThingCategory (AThing {})     = ptext SLIT("Kinded thing")
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
			for initialisation purposes
		* when figuring out what things are really unused

\begin{code}
data ImportAvails 
   = ImportAvails {
	imp_env :: ModuleNameEnv NameSet,
		-- All the things imported, classified by 
		-- the *module qualifier* for its import
		--   e.g.	 import List as Foo
		-- would add a binding Foo |-> ...stuff from List...
		-- to imp_env.
		-- 
		-- We need to classify them like this so that we can figure out 
		-- "module M" export specifiers in an export list 
		-- (see 1.4 Report Section 5.1.1).  Ultimately, we want to find 
		-- everything that is unambiguously in scope as 'M.x'
		-- and where plain 'x' is (perhaps ambiguously) in scope.
		-- So the starting point is all things that are in scope as 'M.x',
		-- which is what this field tells us.

	imp_mods :: ModuleEnv (Module, Bool, SrcSpan),
		-- Domain is all directly-imported modules
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

 	imp_orphs :: [Module]
		-- Orphan modules below us in the import tree
      }

mkModDeps :: [(ModuleName, IsBootInterface)]
	  -> ModuleNameEnv (ModuleName, IsBootInterface)
mkModDeps deps = foldl add emptyUFM deps
	       where
		 add env elt@(m,_) = addToUFM env m elt

emptyImportAvails :: ImportAvails
emptyImportAvails = ImportAvails { imp_env 	= emptyUFM, 
				   imp_mods   	= emptyModuleEnv,
				   imp_dep_mods = emptyUFM,
				   imp_dep_pkgs = [],
				   imp_orphs    = [] }

plusImportAvails ::  ImportAvails ->  ImportAvails ->  ImportAvails
plusImportAvails
  (ImportAvails { imp_env = env1, imp_mods = mods1,
		  imp_dep_mods = dmods1, imp_dep_pkgs = dpkgs1, imp_orphs = orphs1 })
  (ImportAvails { imp_env = env2, imp_mods = mods2,
		  imp_dep_mods = dmods2, imp_dep_pkgs = dpkgs2, imp_orphs = orphs2 })
  = ImportAvails { imp_env      = plusUFM_C unionNameSets env1 env2, 
		   imp_mods     = mods1  `plusModuleEnv` mods2,	
		   imp_dep_mods = plusUFM_C plus_mod_dep dmods1 dmods2,	
		   imp_dep_pkgs = dpkgs1 `unionLists` dpkgs2,
		   imp_orphs    = orphs1 `unionLists` orphs2 }
  where
    plus_mod_dep (m1, boot1) (m2, boot2) 
	= WARN( not (m1 == m2), (ppr m1 <+> ppr m2) $$ (ppr boot1 <+> ppr boot2) )
		-- Check mod-names match
	  (m1, boot1 && boot2)	-- If either side can "see" a non-hi-boot interface, use that
\end{code}

%************************************************************************
%*									*
	Avails, AvailEnv, etc
%*									*
v%************************************************************************

\begin{code}
plusAvail (Avail n1)	   (Avail n2)	    = Avail n1
plusAvail (AvailTC n1 ns1) (AvailTC n2 ns2) = AvailTC n2 (ns1 `unionLists` ns2)
-- Added SOF 4/97
#ifdef DEBUG
plusAvail a1 a2 = pprPanic "RnEnv.plusAvail" (hsep [ppr a1,ppr a2])
#endif

-------------------------
pruneAvails :: (Name -> Bool)	-- Keep if this is True
	    -> [AvailInfo]
	    -> [AvailInfo]
pruneAvails keep avails
  = mapMaybe del avails
  where
    del :: AvailInfo -> Maybe AvailInfo	-- Nothing => nothing left!
    del (Avail n) | keep n    = Just (Avail n)
    	          | otherwise = Nothing
    del (AvailTC n ns) | null ns'  = Nothing
		       | otherwise = Just (AvailTC n ns')
		       where
		         ns' = filter keep ns
\end{code}

---------------------------------------
	AvailEnv and friends
---------------------------------------

\begin{code}
type AvailEnv = NameEnv AvailInfo	-- Maps a Name to the AvailInfo that contains it

emptyAvailEnv :: AvailEnv
emptyAvailEnv = emptyNameEnv

unitAvailEnv :: AvailInfo -> AvailEnv
unitAvailEnv a = unitNameEnv (availName a) a

plusAvailEnv :: AvailEnv -> AvailEnv -> AvailEnv
plusAvailEnv = plusNameEnv_C plusAvail

lookupAvailEnv_maybe :: AvailEnv -> Name -> Maybe AvailInfo
lookupAvailEnv_maybe = lookupNameEnv

lookupAvailEnv :: AvailEnv -> Name -> AvailInfo
lookupAvailEnv env n = case lookupNameEnv env n of
			 Just avail -> avail
			 Nothing    -> pprPanic "lookupAvailEnv" (ppr n)

availEnvElts = nameEnvElts

addAvail :: AvailEnv -> AvailInfo -> AvailEnv
addAvail avails avail = extendNameEnv_C plusAvail avails (availName avail) avail

mkAvailEnv :: [AvailInfo] -> AvailEnv
	-- 'avails' may have several items with the same availName
	-- E.g  import Ix( Ix(..), index )
	-- will give Ix(Ix,index,range) and Ix(index)
	-- We want to combine these; addAvail does that
mkAvailEnv avails = foldl addAvail emptyAvailEnv avails
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

\begin{code}
data Inst
  = Dict
	Name
	TcPredType
	InstLoc

  | Method
	Id

	TcId	-- The overloaded function
			-- This function will be a global, local, or ClassOpId;
			--   inside instance decls (only) it can also be an InstId!
			-- The id needn't be completely polymorphic.
			-- You'll probably find its name (for documentation purposes)
			--	  inside the InstOrigin

	[TcType]	-- The types to which its polymorphic tyvars
			--	should be instantiated.
			-- These types must saturate the Id's foralls.

	TcThetaType	-- The (types of the) dictionaries to which the function
			-- must be applied to get the method

	InstLoc

	-- INVARIANT 1: in (Method u f tys theta tau loc)
	--	type of (f tys dicts(from theta)) = tau

	-- INVARIANT 2: tau must not be of form (Pred -> Tau)
	--   Reason: two methods are considered equal if the 
	--   	     base Id matches, and the instantiating types
	--	     match.  The TcThetaType should then match too.
	--   This only bites in the call to tcInstClassOp in TcClassDcl.mkMethodBind

  | LitInst
	Name
	(HsOverLit Name)	-- The literal from the occurrence site
				-- INVARIANT: never a rebindable-syntax literal
				-- Reason: tcSyntaxName does unification, and we
				--	   don't want to deal with that during tcSimplify,
				--	   when resolving LitInsts
	TcType		-- The type at which the literal is used
	InstLoc
\end{code}

@Insts@ are ordered by their class/type info, rather than by their
unique.  This allows the context-reduction mechanism to use standard finite
maps to do their stuff.

\begin{code}
instance Ord Inst where
  compare = cmpInst

instance Eq Inst where
  (==) i1 i2 = case i1 `cmpInst` i2 of
	         EQ    -> True
		 other -> False

cmpInst (Dict _ pred1 _)     	(Dict _ pred2 _)	= pred1 `tcCmpPred` pred2
cmpInst (Dict _ _ _)	     	other 		    	= LT

cmpInst (Method _ _ _ _ _) 	(Dict _ _ _)	  	= GT
cmpInst (Method _ id1 tys1 _ _) (Method _ id2 tys2 _ _) = (id1 `compare` id2) `thenCmp` (tys1 `tcCmpTypes` tys2)
cmpInst (Method _ _ _ _ _)      other			= LT

cmpInst (LitInst _ _ _ _)	(Dict _ _ _) 		= GT
cmpInst (LitInst _ _ _ _)	(Method _ _ _ _ _)	= GT
cmpInst (LitInst _ lit1 ty1 _)	(LitInst _ lit2 ty2 _)  = (lit1 `compare` lit2) `thenCmp` (ty1 `tcCmpType` ty2)
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
consLIE inst lie  = inst `consBag` lie
plusLIEs lies	  = unionManyBags lies
lieToList	  = bagToList
listToLIE	  = listToBag
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
data InstLoc = InstLoc InstOrigin SrcSpan ErrCtxt

instLocSrcLoc :: InstLoc -> SrcLoc
instLocSrcLoc (InstLoc _ src_span _) = srcSpanStart src_span

instLocSrcSpan :: InstLoc -> SrcSpan
instLocSrcSpan (InstLoc _ src_span _) = src_span

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
  | DefaultOrigin	-- Typechecking a default decl
  | DoOrigin		-- Arising from a do expression
  | ProcOrigin		-- Arising from a proc expression
\end{code}

\begin{code}
pprInstLoc :: InstLoc -> SDoc
pprInstLoc (InstLoc orig locn _)
  = hsep [text "arising from", pp_orig orig, text "at", ppr locn]
  where
    pp_orig (OccurrenceOf name)  = hsep [ptext SLIT("use of"), quotes (ppr name)]
    pp_orig (IPOccOrigin name)   = hsep [ptext SLIT("use of implicit parameter"), quotes (ppr name)]
    pp_orig (IPBindOrigin name)  = hsep [ptext SLIT("binding for implicit parameter"), quotes (ppr name)]
    pp_orig RecordUpdOrigin 	 = ptext SLIT("a record update")
    pp_orig (LiteralOrigin lit)	 = hsep [ptext SLIT("the literal"), quotes (ppr lit)]
    pp_orig (ArithSeqOrigin seq) = hsep [ptext SLIT("the arithmetic sequence"), quotes (ppr seq)]
    pp_orig (PArrSeqOrigin seq)	 = hsep [ptext SLIT("the parallel array sequence"), quotes (ppr seq)]
    pp_orig InstSigOrigin	 = ptext SLIT("instantiating a type signature")
    pp_orig InstScOrigin	 = ptext SLIT("the superclasses of an instance declaration")
    pp_orig DerivOrigin	 	 = ptext SLIT("the 'deriving' clause of a data type declaration")
    pp_orig DefaultOrigin	 = ptext SLIT("a 'default' declaration")
    pp_orig DoOrigin		 = ptext SLIT("a do statement")
    pp_orig ProcOrigin		 = ptext SLIT("a proc expression")
    pp_orig (SigOrigin info) 	 = pprSkolInfo info
\end{code}
