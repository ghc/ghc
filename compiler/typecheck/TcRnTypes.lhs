
% (c) The University of Glasgow 2006
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
	ErrCtxt, RecFieldEnv(..),
	ImportAvails(..), emptyImportAvails, plusImportAvails, 
	WhereFrom(..), mkModDeps,

	-- Typechecker types
	TcTypeEnv, TcTyThing(..), pprTcTyThingCategory, 

	-- Template Haskell
	ThStage(..), topStage, topAnnStage, topSpliceStage,
	ThLevel, impLevel, outerLevel, thLevel,

	-- Arrows
	ArrowCtxt(NoArrowCtxt), newArrowScope, escapeArrowScope,

	-- Constraints
        Untouchables,
  	WantedConstraints, emptyWanteds, andWanteds, extendWanteds,
	WantedConstraint(..), WantedEvVar(..), wantedEvVarLoc, 
        wantedEvVarToVar, wantedEvVarPred, splitWanteds,

	evVarsToWanteds,
	Implication(..), 
        CtLoc(..), ctLocSpan, ctLocOrigin, setCtLocOrigin,
	CtOrigin(..), EqOrigin(..), 
	WantedLoc, GivenLoc,

	SkolemInfo(..),

	-- Pretty printing
	pprEvVarTheta, pprWantedsWithLocs, pprWantedWithLoc, 
	pprEvVars, pprEvVarWithType,
	pprArising, pprArisingAt,

	-- Misc other types
	TcId, TcIdSet, TcTyVarBind(..), TcTyVarBinds
	
  ) where

#include "HsVersions.h"

import HsSyn
import HscTypes
import Type
import TcType
import Annotations
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
import Bag
import Outputable
import ListSetOps
import FastString
import StaticFlags( opt_ErrorSpans )

import Data.Set (Set)
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
type TcId    	 = Id 			-- Type may be a TcType  DV: WHAT??????????
type TcIdSet 	 = IdSet


type TcRnIf a b c = IOEnv (Env a b) c
type IfM lcl a  = TcRnIf IfGblEnv lcl a		-- Iface stuff

type IfG a  = IfM () a				-- Top level
type IfL a  = IfM IfLclEnv a			-- Nested
type TcRn a = TcRnIf TcGblEnv TcLclEnv a
type RnM  a = TcRn a		-- Historical
type TcM  a = TcRn a		-- Historical
\end{code}

Representation of type bindings to uninstantiated meta variables used during
constraint solving.

\begin{code}
data TcTyVarBind = TcTyVarBind TcTyVar TcType

type TcTyVarBinds = Bag TcTyVarBind

instance Outputable TcTyVarBind where
  ppr (TcTyVarBind tv ty) = ppr tv <+> text ":=" <+> ppr ty
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
	tcg_mod     :: Module,         -- ^ Module being compiled
	tcg_src     :: HscSource,
          -- ^ What kind of module (regular Haskell, hs-boot, ext-core)

	tcg_rdr_env :: GlobalRdrEnv,   -- ^ Top level envt; used during renaming
	tcg_default :: Maybe [Type],
          -- ^ Types used for defaulting. @Nothing@ => no @default@ decl

	tcg_fix_env   :: FixityEnv,	-- ^ Just for things in this module
	tcg_field_env :: RecFieldEnv,	-- ^ Just for things in this module

	tcg_type_env :: TypeEnv,
          -- ^ Global type env for the module we are compiling now.  All
	  -- TyCons and Classes (for this module) end up in here right away,
	  -- along with their derived constructors, selectors.
	  --
	  -- (Ids defined in this module start in the local envt, though they
	  --  move to the global envt during zonking)

	tcg_type_env_var :: TcRef TypeEnv,
		-- Used only to initialise the interface-file
		-- typechecker in initIfaceTcRn, so that it can see stuff
		-- bound in this module when dealing with hi-boot recursions
		-- Updated at intervals (e.g. after dealing with types and classes)
	
	tcg_inst_env     :: InstEnv,
          -- ^ Instance envt for /home-package/ modules; Includes the dfuns in
	  -- tcg_insts
	tcg_fam_inst_env :: FamInstEnv,	-- ^ Ditto for family instances

		-- Now a bunch of things about this module that are simply 
		-- accumulated, but never consulted until the end.  
		-- Nevertheless, it's convenient to accumulate them along 
		-- with the rest of the info from this module.
	tcg_exports :: [AvailInfo],	-- ^ What is exported
	tcg_imports :: ImportAvails,
          -- ^ Information about what was imported from where, including
	  -- things bound in this module.

	tcg_dus :: DefUses,
          -- ^ What is defined in this module and what is used.
          -- The latter is used to generate
          --
          --  (a) version tracking; no need to recompile if these things have
          --      not changed version stamp
          --
          --  (b) unused-import info

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

	tcg_inst_uses :: TcRef NameSet,
          -- ^ Home-package Dfuns actually used.
          --
          -- Used to generate version dependencies This records usages, rather
          -- like tcg_dus, but it has to be a mutable variable so it can be
          -- augmented when we look up an instance.  These uses of dfuns are
          -- rather like the free variables of the program, but are implicit
          -- instead of explicit.

	tcg_th_used :: TcRef Bool,
          -- ^ @True@ <=> Template Haskell syntax used.
          --
          -- We need this so that we can generate a dependency on the Template
          -- Haskell package, becuase the desugarer is going to emit loads of
          -- references to TH symbols.  It's rather like tcg_inst_uses; the
          -- reference is implicit rather than explicit, so we have to zap a
          -- mutable variable.

	tcg_dfun_n  :: TcRef OccSet,
          -- ^ Allows us to choose unique DFun names.

	-- The next fields accumulate the payload of the module
	-- The binds, rules and foreign-decl fiels are collected
	-- initially in un-zonked form and are finally zonked in tcRnSrcDecls

        tcg_rn_exports :: Maybe [Located (IE Name)],
        tcg_rn_imports :: [LImportDecl Name],
		-- Keep the renamed imports regardless.  They are not 
		-- voluminous and are needed if you want to report unused imports

        tcg_used_rdrnames :: TcRef (Set RdrName),
		-- The set of used *imported* (not locally-defined) RdrNames
		-- Used only to report unused import declarations

	tcg_rn_decls :: Maybe (HsGroup Name),
          -- ^ Renamed decls, maybe.  @Nothing@ <=> Don't retain renamed
          -- decls.

        tcg_ev_binds  :: Bag EvBind,	    -- Top-level evidence bindings
	tcg_binds     :: LHsBinds Id,	    -- Value bindings in this module
        tcg_sigs      :: NameSet, 	    -- ...Top-level names that *lack* a signature
	tcg_warns     :: Warnings,	    -- ...Warnings and deprecations
	tcg_anns      :: [Annotation],      -- ...Annotations
	tcg_insts     :: [Instance],	    -- ...Instances
	tcg_fam_insts :: [FamInst],	    -- ...Family instances
	tcg_rules     :: [LRuleDecl Id],    -- ...Rules
	tcg_fords     :: [LForeignDecl Id], -- ...Foreign import & exports

	tcg_doc_hdr   :: Maybe LHsDocString, -- ^ Maybe Haddock header docs
        tcg_hpc       :: AnyHpcUsage,        -- ^ @True@ if any part of the
                                             --  prog uses hpc instrumentation.

        tcg_main      :: Maybe Name          -- ^ The Name of the main
                                             -- function, if this module is
                                             -- the main module.
    }

data RecFieldEnv 
  = RecFields (NameEnv [Name])	-- Maps a constructor name *in this module*
				-- to the fields for that constructor
	      NameSet		-- Set of all fields declared *in this module*;
				-- used to suppress name-shadowing complaints
				-- when using record wild cards
				-- E.g.  let fld = e in C {..}
	-- This is used when dealing with ".." notation in record 
	-- construction and pattern matching.
	-- The FieldEnv deals *only* with constructors defined in *this*
	-- module.  For imported modules, we get the same info from the
	-- TypeEnv
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
	tcl_ctxt :: [ErrCtxt],		-- Error context, innermost on top
	tcl_errs :: TcRef Messages,	-- Place to accumulate errors

	tcl_th_ctxt    :: ThStage,	      -- Template Haskell context
	tcl_arrow_ctxt :: ArrowCtxt,	      -- Arrow-notation context

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

	tcl_env  :: TcTypeEnv,    -- The local type environment: Ids and
			          -- TyVars defined in this module
					
	tcl_tyvars :: TcRef TcTyVarSet,	-- The "global tyvars"
			-- Namely, the in-scope TyVars bound in tcl_env, 
			-- plus the tyvars mentioned in the types of Ids bound
			-- in tcl_lenv. 
                        -- Why mutable? see notes with tcGetGlobalTyVars

	tcl_lie   :: TcRef WantedConstraints,    -- Place to accumulate type constraints
	tcl_untch :: Untouchables		 -- Untouchables
    }

type TcTypeEnv = NameEnv TcTyThing


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
-- Template Haskell stages and levels 
---------------------------

data ThStage	-- See Note [Template Haskell state diagram] in TcSplice
  = Splice	-- Top-level splicing
		-- This code will be run *at compile time*;
		--   the result replaces the splice
		-- Binding level = 0
 
  | Comp   	-- Ordinary Haskell code
		-- Binding level = 1

  | Brack  			-- Inside brackets 
      ThStage 			--   Binding level = level(stage) + 1
      (TcRef [PendingSplice])	--   Accumulate pending splices here
      (TcRef WantedConstraints)	--     and type constraints here

topStage, topAnnStage, topSpliceStage :: ThStage
topStage       = Comp
topAnnStage    = Splice
topSpliceStage = Splice

instance Outputable ThStage where
   ppr Splice        = text "Splice"
   ppr Comp	     = text "Comp"
   ppr (Brack s _ _) = text "Brack" <> parens (ppr s)

type ThLevel = Int	
        -- See Note [Template Haskell levels] in TcSplice
	-- Incremented when going inside a bracket,
	-- decremented when going inside a splice
	-- NB: ThLevel is one greater than the 'n' in Fig 2 of the
	--     original "Template meta-programming for Haskell" paper

impLevel, outerLevel :: ThLevel
impLevel = 0	-- Imported things; they can be used inside a top level splice
outerLevel = 1	-- Things defined outside brackets
-- NB: Things at level 0 are not *necessarily* imported.
--	eg  $( \b -> ... )   here b is bound at level 0
--
-- For example: 
--	f = ...
--	g1 = $(map ...)		is OK
--	g2 = $(f ...)		is not OK; because we havn't compiled f yet

thLevel :: ThStage -> ThLevel
thLevel Splice        = 0
thLevel Comp          = 1
thLevel (Brack s _ _) = thLevel s + 1

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
	tct_id    :: TcId,		
	tct_level :: ThLevel }

  | ATyVar  Name TcType		-- The type to which the lexically scoped type vaiable
				-- is currently refined. We only need the Name
				-- for error-message purposes; it is the corresponding
				-- Name in the domain of the envt

  | AThing  TcKind 		-- Used temporarily, during kind checking, for the
				--	tycons and clases in this recursive group

instance Outputable TcTyThing where	-- Debugging only
   ppr (AGlobal g)      = pprTyThing g
   ppr elt@(ATcId {})   = text "Identifier" <> 
			  brackets (ppr (tct_id elt) <> dcolon 
                                 <> ppr (varType (tct_id elt)) <> comma
				 <+> ppr (tct_level elt))
   ppr (ATyVar tv _)    = text "Type variable" <+> quotes (ppr tv)
   ppr (AThing k)       = text "AThing" <+> ppr k

pprTcTyThingCategory :: TcTyThing -> SDoc
pprTcTyThingCategory (AGlobal thing) = pprTyThingCategory thing
pprTcTyThingCategory (ATyVar {})     = ptext (sLit "Type variable")
pprTcTyThingCategory (ATcId {})      = ptext (sLit "Local identifier")
pprTcTyThingCategory (AThing {})     = ptext (sLit "Kinded thing")
\end{code}

\begin{code}
type ErrCtxt = (Bool, TidyEnv -> TcM (TidyEnv, Message))
	-- Monadic so that we have a chance
	-- to deal with bound type variables just before error
	-- message construction

	-- Bool:  True <=> this is a landmark context; do not
	--		   discard it when trimming for display
\end{code}


%************************************************************************
%*									*
	Operations over ImportAvails
%*									*
%************************************************************************

\begin{code}
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
	imp_mods :: ModuleEnv [(ModuleName, Bool, SrcSpan)],
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
          --       file; if we import somethign we need to recompile if the
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

	imp_dep_pkgs :: [PackageId],
          -- ^ Packages needed by the module being compiled, whether directly,
          -- or via other modules in this package, or via modules imported
          -- from other packages.

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
  = ImportAvails { imp_mods     = plusModuleEnv_C (++) mods1 mods2,	
		   imp_dep_mods = plusUFM_C plus_mod_dep dmods1 dmods2,	
		   imp_dep_pkgs = dpkgs1 `unionLists` dpkgs2,
		   imp_orphs    = orphs1 `unionLists` orphs2,
		   imp_finsts   = finsts1 `unionLists` finsts2 }
  where
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
  ppr (ImportByUser is_boot) | is_boot     = ptext (sLit "{- SOURCE -}")
			     | otherwise   = empty
  ppr ImportBySystem     		   = ptext (sLit "{- SYSTEM -}")
\end{code}


%************************************************************************
%*									*
		Wanted constraints

     These are forced to be in TcRnTypes because
     	   TcLclEnv mentions WantedConstraints
	   WantedConstraint mentions CtLoc
	   CtLoc mentions ErrCtxt
	   ErrCtxt mentions TcM
%*									*
v%************************************************************************

\begin{code}
type Untouchables = TcTyVarSet	-- All MetaTyVars

type WantedConstraints = Bag WantedConstraint

data WantedConstraint
  = WcEvVar  WantedEvVar
  | WcImplic Implication
  -- ToDo: add literals, methods

-- EvVar defined in module Var.lhs: 
-- Evidence variables include all *quantifiable* constraints
--   dictionaries
--   implicit parameters
--   coercion variables

data WantedEvVar   -- The sort of constraint over which one can lambda-abstract
   = WantedEvVar 
         EvVar 	     -- The variable itself; make a binding for it please
         WantedLoc   -- How the constraint arose in the first place
	 	     -- (used for error messages only)

type WantedLoc = CtLoc CtOrigin
type GivenLoc  = CtLoc SkolemInfo

data Implication
  = Implic {  
      ic_untch :: Untouchables, -- Untouchables: unification variables
                                  -- free in the environment
      ic_env   :: TcTypeEnv,    -- The type environment
      		      	      	  -- Used only when generating error messages
	  -- Generally, ic_untch is a superset of tvsof(ic_env)
	  -- However, we don't zonk ic_env when zonking the Implication
	  -- Instead we do that when generating a skolem-escape error message

      ic_skols  :: TcTyVarSet,   -- Introduced skolems 
      		   	         -- See Note [Skolems in an implication]

      ic_scoped :: [TcTyVar],    -- List of scoped variables to be unified 
                                 -- bijectively to a subset of ic_tyvars
				 -- Note [Scoped pattern variable]

      ic_given  :: [EvVar],      -- Given evidence variables
      		   		 --   (order does not matter)

      ic_wanted :: WantedConstraints,    -- Wanted constraints

      ic_binds  :: EvBindsVar,   -- Points to the place to fill in the
      		      	    	 -- abstraction and bindings

      ic_loc   :: GivenLoc }

evVarsToWanteds :: WantedLoc -> [EvVar] -> WantedConstraints
evVarsToWanteds loc evs = listToBag [WcEvVar (WantedEvVar ev loc) | ev <- evs]

wantedEvVarLoc :: WantedEvVar -> WantedLoc 
wantedEvVarLoc (WantedEvVar _ loc) = loc 

wantedEvVarToVar :: WantedEvVar -> EvVar 
wantedEvVarToVar (WantedEvVar ev _) = ev 

wantedEvVarPred :: WantedEvVar -> PredType 
wantedEvVarPred (WantedEvVar ev _)  = evVarPred ev 

splitWanteds :: WantedConstraints -> (Bag WantedEvVar, Bag Implication)
splitWanteds wanted = partitionBagWith pick wanted
  where
    pick (WcEvVar v)  = Left v
    pick (WcImplic i) = Right i
\end{code}

Note [Skolems in an implication]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The skolems in an implication are not there to perform a skolem escape
check.  That happens because all the environment variables are in the
untouchables, and therefore cannot be unified with anything at all,
let alone the skolems.

Instead, ic_skols is used only when considering floating a constraint
outside the implication in TcSimplify.floatEqualities or 
TcSimplify.approximateImplications

Note [Scoped pattern variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   data T where K :: forall a,b. a -> b -> T

   ...(case x of K (p::c) (q::d) -> ...)...

We create fresh MetaTvs for c,d, and later check that they are
bound bijectively to the skolems we created for a,b.  So the
implication constraint looks like
	    ic_skols  = {a',b'}  -- Skolem tvs created from a,b
	    ic_scoped = {c',d'}  -- Meta tvs created from c,d

\begin{code}
emptyWanteds :: WantedConstraints
emptyWanteds = emptyBag

andWanteds :: WantedConstraints -> WantedConstraints -> WantedConstraints
andWanteds = unionBags

extendWanteds :: WantedConstraints -> WantedConstraint -> WantedConstraints
extendWanteds = snocBag
\end{code}
 
\begin{code}
pprEvVars :: [EvVar] -> SDoc	-- Print with their types
pprEvVars ev_vars = vcat (map pprEvVarWithType ev_vars)

pprEvVarTheta :: [EvVar] -> SDoc
pprEvVarTheta ev_vars = pprTheta (map evVarPred ev_vars)
                              
pprEvVarWithType :: EvVar -> SDoc
pprEvVarWithType v = ppr v <+> dcolon <+> pprPred (evVarPred v)

pprWantedsWithLocs :: Bag WantedConstraint -> SDoc
pprWantedsWithLocs = foldrBag (($$) . pprWantedWithLoc) empty 

pprWantedWithLoc :: WantedConstraint -> SDoc
pprWantedWithLoc (WcImplic i) = ppr i
pprWantedWithLoc (WcEvVar v)  = pprWantedEvVarWithLoc v

instance Outputable WantedConstraint where
  ppr (WcEvVar v)  = ppr v
  ppr (WcImplic i) = ppr i

-- Adding -ferror-spans makes the output more voluminous
instance Outputable WantedEvVar where
  ppr wev | opt_ErrorSpans = pprWantedEvVarWithLoc wev
          | otherwise      = pprWantedEvVar wev

pprWantedEvVarWithLoc, pprWantedEvVar :: WantedEvVar -> SDoc
pprWantedEvVarWithLoc (WantedEvVar v loc) = hang (pprEvVarWithType v) 
                                               2 (pprArisingAt loc) 
pprWantedEvVar        (WantedEvVar v _)   = pprEvVarWithType v

instance Outputable Implication where
  ppr (Implic { ic_untch = untch, ic_skols = skols, ic_given = given
              , ic_wanted = wanted, ic_binds = binds, ic_loc = loc })
   = ptext (sLit "Implic") <+> braces 
     (sep [ ptext (sLit "Untouchables = ") <+> ppr untch
          , ptext (sLit "Skolems = ") <+> ppr skols
          , ptext (sLit "Given = ") <+> pprEvVars given
          , ptext (sLit "Wanted = ") <+> ppr wanted
	  , ptext (sLit "Binds = ") <+> ppr binds
          , pprSkolInfo (ctLocOrigin loc)
          , ppr (ctLocSpan loc) ])
\end{code}

%************************************************************************
%*									*
            CtLoc, CtOrigin
%*									*
%************************************************************************

The 'CtLoc' and 'CtOrigin' types gives information about where a
*wanted constraint* came from.  This is important for decent error
message reporting because dictionaries don't appear in the original
source code.  Doubtless this type will evolve...

\begin{code}
-------------------------------------------
data CtLoc orig = CtLoc orig SrcSpan [ErrCtxt]

ctLocSpan :: CtLoc o -> SrcSpan
ctLocSpan (CtLoc _ s _) = s

ctLocOrigin :: CtLoc o -> o
ctLocOrigin (CtLoc o _ _) = o

setCtLocOrigin :: CtLoc o -> o' -> CtLoc o'
setCtLocOrigin (CtLoc _ s c) o = CtLoc o s c

pprArising :: CtOrigin -> SDoc
pprArising (TypeEqOrigin {}) = empty
pprArising orig              = text "arising from" <+> ppr orig

pprArisingAt :: CtLoc CtOrigin -> SDoc
pprArisingAt (CtLoc o s _) = sep [pprArising o, text "at" <+> ppr s]

-------------------------------------------
-- CtOrigin gives the origin of *wanted* constraints
data CtOrigin
  = OccurrenceOf Name		-- Occurrence of an overloaded identifier
  | AppOrigin	 		-- An application of some kind

  | SpecPragOrigin Name		-- Specialisation pragma for identifier

  | TypeEqOrigin EqOrigin

  | IPOccOrigin  (IPName Name)	-- Occurrence of an implicit parameter

  | LiteralOrigin (HsOverLit Name)	-- Occurrence of a literal
  | NegateOrigin			-- Occurrence of syntactic negation

  | ArithSeqOrigin (ArithSeqInfo Name) -- [x..], [x..y] etc
  | PArrSeqOrigin  (ArithSeqInfo Name) -- [:x..y:] and [:x,y..z:]
  | SectionOrigin
  | TupleOrigin			       -- (..,..)
  | ExprSigOrigin	-- e :: ty
  | PatSigOrigin	-- p :: ty
  | PatOrigin	        -- Instantiating a polytyped pattern at a constructor
  | RecordUpdOrigin
  | ViewPatOrigin

  | ScOrigin	        -- Typechecking superclasses of an instance declaration
  | DerivOrigin		-- Typechecking deriving
  | StandAloneDerivOrigin -- Typechecking stand-alone deriving
  | DefaultOrigin	-- Typechecking a default decl
  | DoOrigin		-- Arising from a do expression
  | ProcOrigin		-- Arising from a proc expression
  | AnnOrigin           -- An annotation

data EqOrigin 
  = UnifyOrigin 
       { uo_actual   :: TcType
       , uo_expected :: TcType }

instance Outputable CtOrigin where
  ppr orig = pprO orig

pprO :: CtOrigin -> SDoc
pprO (OccurrenceOf name)   = hsep [ptext (sLit "a use of"), quotes (ppr name)]
pprO AppOrigin             = ptext (sLit "an application")
pprO (SpecPragOrigin name) = hsep [ptext (sLit "a specialisation pragma for"), quotes (ppr name)]
pprO (IPOccOrigin name)    = hsep [ptext (sLit "a use of implicit parameter"), quotes (ppr name)]
pprO RecordUpdOrigin       = ptext (sLit "a record update")
pprO ExprSigOrigin         = ptext (sLit "an expression type signature")
pprO PatSigOrigin          = ptext (sLit "a pattern type signature")
pprO PatOrigin             = ptext (sLit "a pattern")
pprO ViewPatOrigin         = ptext (sLit "a view pattern")
pprO (LiteralOrigin lit)   = hsep [ptext (sLit "the literal"), quotes (ppr lit)]
pprO (ArithSeqOrigin seq)  = hsep [ptext (sLit "the arithmetic sequence"), quotes (ppr seq)]
pprO (PArrSeqOrigin seq)   = hsep [ptext (sLit "the parallel array sequence"), quotes (ppr seq)]
pprO SectionOrigin	   = ptext (sLit "an operator section")
pprO TupleOrigin	   = ptext (sLit "a tuple")
pprO NegateOrigin	   = ptext (sLit "a use of syntactic negation")
pprO ScOrigin	           = ptext (sLit "the superclasses of an instance declaration")
pprO DerivOrigin	   = ptext (sLit "the 'deriving' clause of a data type declaration")
pprO StandAloneDerivOrigin = ptext (sLit "a 'deriving' declaration")
pprO DefaultOrigin	   = ptext (sLit "a 'default' declaration")
pprO DoOrigin	           = ptext (sLit "a do statement")
pprO ProcOrigin	           = ptext (sLit "a proc expression")
pprO (TypeEqOrigin eq)     = ptext (sLit "an equality") <+> ppr eq
pprO AnnOrigin             = ptext (sLit "an annotation")

instance Outputable EqOrigin where
  ppr (UnifyOrigin t1 t2) = ppr t1 <+> char '~' <+> ppr t2
\end{code}
