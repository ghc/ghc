%
% (c) The GRASP Project, Glasgow University, 1992-2002
%
\begin{code}
module TcRnTypes(
	TcRn, TcM, RnM,	-- The monad is opaque outside this module

	-- Standard monadic operations
	thenM, thenM_, returnM, failM,

	-- Non-standard operations
	runTcRn, fixM, tryM, ioToTcRn,
	newMutVar, readMutVar, writeMutVar,
	getEnv, setEnv, updEnv, unsafeInterleaveM, 
		
	-- The environment types
	Env(..), TopEnv(..), TcGblEnv(..), 
	TcLclEnv(..), RnLclEnv(..),

	-- Ranamer types
	RnMode(..), isInterfaceMode, isCmdLineMode,
	EntityUsage, emptyUsages, ErrCtxt,
	ImportAvails(..), emptyImportAvails, plusImportAvails, 
	plusAvail, pruneAvails,  
	AvailEnv, emptyAvailEnv, unitAvailEnv, plusAvailEnv, 
	mkAvailEnv, lookupAvailEnv, availEnvElts, addAvail,
	WhereFrom(..),

	-- Typechecker types
	TcTyThing(..),

	-- Template Haskell
	Stage(..), topStage, topSpliceStage,
	Level, impLevel, topLevel,

	-- Insts
	Inst(..), InstOrigin(..), InstLoc(..), pprInstLoc, instLocSrcLoc,
	LIE, emptyLIE, unitLIE, plusLIE, consLIE, 
	plusLIEs, mkLIE, isEmptyLIE, lieToList, listToLIE,

	-- Misc other types
	TcRef, TcId, TcIdSet
  ) where

#include "HsVersions.h"

import HsSyn		( PendingSplice, HsOverLit, MonoBinds, RuleDecl, ForeignDecl )
import RnHsSyn		( RenamedHsExpr, RenamedPat, RenamedArithSeqInfo )
import HscTypes		( GhciMode, ExternalPackageState, HomePackageTable, NameCache,
			  GlobalRdrEnv, LocalRdrEnv, FixityEnv, TypeEnv, TyThing, 
			  Avails, GenAvailInfo(..), AvailInfo, availName,
			  IsBootInterface, Deprecations )
import Packages		( PackageName )
import TcType		( TcTyVarSet, TcType, TcTauType, TcThetaType, TcPredType, TcKind,
			  tcCmpPred, tcCmpType, tcCmpTypes )
import InstEnv		( DFunId, InstEnv )
import Name		( Name )
import NameEnv
import NameSet		( NameSet, emptyNameSet )
import Type		( Type )
import Class		( Class )
import Var		( Id, TyVar )
import VarEnv		( TidyEnv )
import Module
import SrcLoc		( SrcLoc )
import VarSet		( IdSet )
import ErrUtils		( Messages, Message )
import CmdLineOpts	( DynFlags )
import UniqSupply	( UniqSupply )
import BasicTypes	( IPName )
import Util		( thenCmp )
import Bag
import Outputable
import DATA_IOREF	( IORef, newIORef, readIORef, writeIORef )
import UNSAFE_IO	( unsafeInterleaveIO )
import FIX_IO		( fixIO )
import EXCEPTION	( Exception )
import Maybe		( mapMaybe )
import ListSetOps	( unionLists )
import Panic		( tryMost )
\end{code}


\begin{code}
type TcRef a = IORef a
type TcId    = Id 			-- Type may be a TcType
type TcIdSet = IdSet
\end{code}

%************************************************************************
%*									*
	       Standard monad definition for TcRn
    All the combinators for the monad can be found in TcRnMonad
%*									*
%************************************************************************

The monad itself has to be defined here, 
because it is mentioned by ErrCtxt

\begin{code}
newtype TcRn m a = TcRn (Env m -> IO a)
unTcRn (TcRn f) = f

type TcM a = TcRn TcLclEnv a
type RnM a = TcRn RnLclEnv a

returnM :: a -> TcRn m a
returnM a = TcRn (\ env -> return a)

thenM :: TcRn m a -> (a -> TcRn m b) -> TcRn m b
thenM (TcRn m) f = TcRn (\ env -> do { r <- m env ;
				       unTcRn (f r) env })

thenM_ :: TcRn m a -> TcRn m b -> TcRn m b
thenM_ (TcRn m) f = TcRn (\ env -> do { m env ; unTcRn f env })

failM :: TcRn m a
failM = TcRn (\ env -> ioError (userError "TcRn failure"))

instance Monad (TcRn m) where
  (>>=)  = thenM
  (>>)   = thenM_
  return = returnM
  fail s = failM	-- Ignore the string
\end{code}


%************************************************************************
%*									*
	Fundmantal combinators specific to the monad
%*									*
%************************************************************************

Running it

\begin{code}
runTcRn :: Env m -> TcRn m a -> IO a
runTcRn env (TcRn m) = m env
\end{code}

The fixpoint combinator

\begin{code}
{-# NOINLINE fixM #-}
  -- Aargh!  Not inlining fixTc alleviates a space leak problem.
  -- Normally fixTc is used with a lazy tuple match: if the optimiser is
  -- shown the definition of fixTc, it occasionally transforms the code
  -- in such a way that the code generator doesn't spot the selector
  -- thunks.  Sigh.

fixM :: (a -> TcRn m a) -> TcRn m a
fixM f = TcRn (\ env -> fixIO (\ r -> unTcRn (f r) env))
\end{code}

Error recovery

\begin{code}
tryM :: TcRn m r -> TcRn m (Either Exception r)
-- Reflect exception into TcRn monad
tryM (TcRn thing) = TcRn (\ env -> tryMost (thing env))
\end{code}

Lazy interleave 

\begin{code}
unsafeInterleaveM :: TcRn m a -> TcRn m a
unsafeInterleaveM (TcRn m) = TcRn (\ env -> unsafeInterleaveIO (m env))
\end{code}

\end{code}

Performing arbitrary I/O, plus the read/write var (for efficiency)

\begin{code}
ioToTcRn :: IO a -> TcRn m a
ioToTcRn io = TcRn (\ env -> io)

newMutVar :: a -> TcRn m (TcRef a)
newMutVar val = TcRn (\ env -> newIORef val)

writeMutVar :: TcRef a -> a -> TcRn m ()
writeMutVar var val = TcRn (\ env -> writeIORef var val)

readMutVar :: TcRef a -> TcRn m a
readMutVar var = TcRn (\ env -> readIORef var)
\end{code}

Getting the environment

\begin{code}
getEnv :: TcRn m (Env m)
{-# INLINE getEnv #-}
getEnv = TcRn (\ env -> return env)

setEnv :: Env n -> TcRn n a -> TcRn m a
{-# INLINE setEnv #-}
setEnv new_env (TcRn m) = TcRn (\ env -> m new_env)

updEnv :: (Env m -> Env n) -> TcRn n a -> TcRn m a
{-# INLINE updEnv #-}
updEnv upd (TcRn m) = TcRn (\ env -> m (upd env))
\end{code}


%************************************************************************
%*									*
		The main environment types
%*									*
%************************************************************************

\begin{code}
data Env a	-- Changes as we move into an expression
  = Env {
	env_top	 :: TopEnv,	-- Top-level stuff that never changes
				--   Mainly a bunch of updatable refs
				--   Includes all info about imported things
	env_gbl  :: TcGblEnv,	-- Info about things defined at the top leve
				--   of the module being compiled

	env_lcl  :: a,		-- Different for the type checker 
				-- and the renamer

	env_loc  :: SrcLoc	-- Source location
    }

data TopEnv	-- Built once at top level then does not change
		-- Concerns imported stuff
		-- Exceptions: error recovery points, meta computation points
   = TopEnv {
	top_mode    :: GhciMode,
        top_dflags  :: DynFlags,

	-- Stuff about imports
	top_eps	   :: TcRef ExternalPackageState,
		-- PIT, ImportedModuleInfo
		-- DeclsMap, IfaceRules, IfaceInsts, InstGates
		-- TypeEnv, InstEnv, RuleBase
		-- Mutable, because we demand-load declarations that extend the state

	top_hpt	 :: HomePackageTable,
		-- The home package table that we've accumulated while 
		-- compiling the home package, 
		-- *excluding* the module we are compiling right now.
		-- (In one-shot mode the current module is the only
		--  home-package module, so tc_hpt is empty.  All other
		--  modules count as "external-package" modules.)
		-- tc_hpt is not mutable because we only demand-load 
		-- external packages; the home package is eagerly 
		-- loaded by the compilation manager.

	-- The global name supply
	top_nc	   :: TcRef NameCache,		-- Maps original names to Names
	top_us     :: TcRef UniqSupply,		-- Unique supply for this module
	top_errs   :: TcRef Messages
   }

-- TcGblEnv describes the top-level of the module at the 
-- point at which the typechecker is finished work.
-- It is this structure that is handed on to the desugarer

data TcGblEnv
  = TcGblEnv {
	tcg_mod    :: Module,		-- Module being compiled
	tcg_usages :: TcRef EntityUsage,  -- What version of what entities 
					  -- have been used from other home-pkg modules
	tcg_rdr_env :: GlobalRdrEnv,	-- Top level envt; used during renaming
	tcg_fix_env :: FixityEnv,	-- Ditto
	tcg_default :: [Type],		-- Types used for defaulting

	tcg_type_env :: TypeEnv,	-- Global type env for the module we are compiling now
		-- All TyCons and Classes (for this module) end up in here right away,
		-- along with their derived constructors, selectors.
		--
		-- (Ids defined in this module start in the local envt, 
		--  though they move to the global envt during zonking)
	
	tcg_inst_env :: TcRef InstEnv,	-- Global instance env: a combination of 
					--	tc_pcs, tc_hpt, *and* tc_insts
		-- This field is mutable so that it can be updated inside a
		-- Template Haskell splice, which might suck in some new
		-- instance declarations.  This is a slightly different strategy
		-- than for the type envt, where we look up first in tcg_type_env
		-- and then in the mutable EPS, because the InstEnv for this module
		-- is constructed (in principle at least) only from the modules
		-- 'below' this one, so it's this-module-specific
		--
		-- On the other hand, a declaration quote [d| ... |] may introduce
		-- some new instance declarations that we *don't* want to persist
		-- outside the quote, so we tiresomely need to revert the InstEnv
		-- after finishing the quote (see TcSplice.tcBracket)

		-- Now a bunch of things about this module that are simply 
		-- accumulated, but never consulted until the end.  
		-- Nevertheless, it's convenient to accumulate them along 
		-- with the rest of the info from this module.
	tcg_exports :: Avails,			-- What is exported
	tcg_imports :: ImportAvails,		-- Information about what was imported 
						--    from where, including things bound
						--    in this module

		-- The next fields accumulate the payload of the module
		-- The binds, rules and foreign-decl fiels are collected
		-- initially in un-zonked form and are finally zonked in tcRnSrcDecls
	tcg_binds   :: MonoBinds Id,		-- Value bindings in this module
	tcg_deprecs :: Deprecations,		-- ...Deprecations 
	tcg_insts   :: [DFunId],		-- ...Instances
	tcg_rules   :: [RuleDecl Id],		-- ...Rules
	tcg_fords   :: [ForeignDecl Id]		-- ...Foreign import & exports
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
data TcLclEnv
  = TcLclEnv {
	tcl_ctxt :: ErrCtxt,	-- Error context

	tcl_level  :: Stage,		-- Template Haskell context

	tcl_env    :: NameEnv TcTyThing,  -- The local type environment: Ids and TyVars
					  -- defined in this module
					
	tcl_tyvars :: TcRef TcTyVarSet,	-- The "global tyvars"
					-- Namely, the in-scope TyVars bound in tcl_lenv, 
					-- plus the tyvars mentioned in the types of 
					-- Ids bound in tcl_lenv
					-- Why mutable? see notes with tcGetGlobalTyVars

	tcl_lie :: TcRef LIE		-- Place to accumulate type constraints
    }

type Level = Int

data Stage
  = Comp   				-- Ordinary compiling, at level topLevel
  | Splice Level 			-- Inside a splice
  | Brack  Level 			-- Inside brackets; 
	   (TcRef [PendingSplice])	--   accumulate pending splices here
	   (TcRef LIE)			--   and type constraints here
topStage, topSpliceStage :: Stage
topStage       = Comp
topSpliceStage = Splice (topLevel - 1)	-- Stage for the body of a top-level splice


impLevel, topLevel :: Level
topLevel = 1	-- Things defined at top level of this module
impLevel = 0	-- Imported things; they can be used inside a top level splice
--
-- For example: 
--	f = ...
--	g1 = $(map ...)		is OK
--	g2 = $(f ...)		is not OK; because we havn't compiled f yet

data TcTyThing
  = AGlobal TyThing		-- Used only in the return type of a lookup
  | ATcId   TcId Level 		-- Ids defined in this module; may not be fully zonked
  | ATyVar  TyVar 		-- Type variables
  | AThing  TcKind		-- Used temporarily, during kind checking
-- Here's an example of how the AThing guy is used
-- Suppose we are checking (forall a. T a Int):
--	1. We first bind (a -> AThink kv), where kv is a kind variable. 
--	2. Then we kind-check the (T a Int) part.
--	3. Then we zonk the kind variable.
--	4. Now we know the kind for 'a', and we add (a -> ATyVar a::K) to the environment

instance Outputable TcTyThing where	-- Debugging only
   ppr (AGlobal g) = text "AGlobal" <+> ppr g
   ppr (ATcId g l) = text "ATcId" <+> ppr g <+> ppr l
   ppr (ATyVar t)  = text "ATyVar" <+> ppr t
   ppr (AThing k)  = text "AThing" <+> ppr k
\end{code}

\begin{code}
type ErrCtxt = [TidyEnv -> TcM (TidyEnv, Message)]	
			-- Innermost first.  Monadic so that we have a chance
			-- to deal with bound type variables just before error
			-- message construction
\end{code}


%************************************************************************
%*									*
		The local renamer environment
%*									*
%************************************************************************

\begin{code}
data RnLclEnv
  = RnLclEnv {
	rn_mode :: RnMode,
	rn_lenv :: LocalRdrEnv		-- Local name envt
		--   Does *not* include global name envt; may shadow it
		--   Includes both ordinary variables and type variables;
		--   they are kept distinct because tyvar have a different
		--   occurrence contructor (Name.TvOcc)
		-- We still need the unsullied global name env so that
    		--   we can look up record field names
     }	

data RnMode = SourceMode		-- Renaming source code
	    | InterfaceMode Module	-- Renaming interface declarations from M
	    | CmdLineMode		-- Renaming a command-line expression

isInterfaceMode (InterfaceMode _) = True
isInterfaceMode _ 		  = False

isCmdLineMode CmdLineMode = True
isCmdLineMode _ = False
\end{code}


%************************************************************************
%*									*
			EntityUsage
%*									*
%************************************************************************

EntityUsage tells what things are actually need in order to compile this
module.  It is used for generating the usage-version field of the ModIface.

Note that we do not record version info for entities from 
other (non-home) packages.  If the package changes, GHC doesn't help.

\begin{code}
type EntityUsage = NameSet
	-- The Names are all the (a) home-package
	--			 (b) "big" (i.e. no data cons, class ops)
	--	   		 (c) non-locally-defined
	--			 (d) non-wired-in
	-- names that have been slurped in so far.
	-- This is used to generate the "usage" information for this module.

emptyUsages :: EntityUsage
emptyUsages = emptyNameSet
\end{code}


%************************************************************************
%*									*
	Operations over ImportAvails
%*									*
%************************************************************************

ImportAvails summarises what was imported from where, irrespective
of whether the imported htings are actually used or not
It is used 	* when porcessing the export list
		* when constructing usage info for the inteface file
		* to identify the list of directly imported modules
			for initialisation purposes

\begin{code}
data ImportAvails 
   = ImportAvails {
	imp_env :: AvailEnv,
		-- All the things that are available from the import
		-- Its domain is all the "main" things;
		-- i.e. *excluding* class ops and constructors
		--	(which appear inside their parent AvailTC)

	imp_qual :: ModuleEnv AvailEnv,
		-- Used to figure out "module M" export specifiers
		-- (see 1.4 Report Section 5.1.1).  Ultimately, we want to find 
		-- everything that is unambiguously in scope as 'M.x'
		-- and where plain 'x' is (perhaps ambiguously) in scope.
		-- So the starting point is all things that are in scope as 'M.x',
		-- which is what this field tells us.
		--
		-- Domain is the *module qualifier* for imports.
		--   e.g.	 import List as Foo
		-- would add a binding Foo |-> ...stuff from List...
		-- to imp_qual.
		-- We keep the stuff as an AvailEnv so that it's easy to 
		-- combine stuff coming from different (unqualified) 
		-- imports of the same module

	imp_mods :: ModuleEnv (Module, Bool),
		-- Domain is all directly-imported modules
		-- Bool is True if there was an unrestricted import
		--	(i.e. not a selective list)
		-- We need the Module in the range because we can't get
		-- 	the keys of a ModuleEnv
		-- Used 
		--   (a) to help construct the usage information in 
		--       the interface file; if we import everything we
		--       need to recompile if the module version changes
		--   (b) to specify what child modules to initialise

	imp_dep_mods :: ModuleEnv (ModuleName, IsBootInterface),
		-- Home-package modules needed by the module being compiled
		--
		-- It doesn't matter whether any of these dependencies are actually
		-- *used* when compiling the module; they are listed if they are below
		-- it at all.  For example, suppose M imports A which imports X.  Then
		-- compiling M might not need to consult X.hi, but X is still listed
		-- in M's dependencies.

	imp_dep_pkgs :: [PackageName],
		-- Packages needed by the module being compiled, whether
		-- directly, or via other modules in this package, or via
		-- modules imported from other packages.

 	imp_orphs :: [ModuleName]
		-- Orphan modules below us in the import tree
      }

emptyImportAvails :: ImportAvails
emptyImportAvails = ImportAvails { imp_env    	= emptyAvailEnv, 
				   imp_qual 	= emptyModuleEnv, 
				   imp_mods   	= emptyModuleEnv,
				   imp_dep_mods = emptyModuleEnv,
				   imp_dep_pkgs = [],
				   imp_orphs    = [] }

plusImportAvails ::  ImportAvails ->  ImportAvails ->  ImportAvails
plusImportAvails
  (ImportAvails { imp_env = env1, imp_qual = unqual1, imp_mods = mods1,
		  imp_dep_mods = dmods1, imp_dep_pkgs = dpkgs1, imp_orphs = orphs1 })
  (ImportAvails { imp_env = env2, imp_qual = unqual2, imp_mods = mods2,
		  imp_dep_mods = dmods2, imp_dep_pkgs = dpkgs2, imp_orphs = orphs2 })
  = ImportAvails { imp_env      = env1 `plusAvailEnv` env2, 
		   imp_qual     = plusModuleEnv_C plusAvailEnv unqual1 unqual2, 
		   imp_mods     = mods1  `plusModuleEnv` mods2,	
		   imp_dep_mods = plusModuleEnv_C plus_mod_dep dmods1 dmods2,	
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

lookupAvailEnv = lookupNameEnv

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

  | ImportForUsage IsBootInterface	-- Import when chasing usage info from an interaface file
					-- 	Failure in this case is not an error

  | ImportBySystem			-- Non user import.

instance Outputable WhereFrom where
  ppr (ImportByUser is_boot) | is_boot     = ptext SLIT("{- SOURCE -}")
			     | otherwise   = empty
  ppr (ImportForUsage is_boot) | is_boot   = ptext SLIT("{- USAGE SOURCE -}")
			       | otherwise = ptext SLIT("{- USAGE -}")
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
	Id
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

	TcTauType	-- The tau-type of the method

	InstLoc

	-- INVARIANT 1: in (Method u f tys theta tau loc)
	--	type of (f tys dicts(from theta)) = tau

	-- INVARIANT 2: tau must not be of form (Pred -> Tau)
	--   Reason: two methods are considerd equal if the 
	--   	     base Id matches, and the instantiating types
	--	     match.  The TcThetaType should then match too.
	--   This only bites in the call to tcInstClassOp in TcClassDcl.mkMethodBind

  | LitInst
	Id
	HsOverLit	-- The literal from the occurrence site
			--	INVARIANT: never a rebindable-syntax literal
			--	Reason: tcSyntaxName does unification, and we
			--		don't want to deal with that during tcSimplify
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

cmpInst (Dict _ pred1 _)     	  (Dict _ pred2 _)	    = pred1 `tcCmpPred` pred2
cmpInst (Dict _ _ _)	     	  other 		    = LT

cmpInst (Method _ _ _ _ _ _) 	  (Dict _ _ _)	  	    = GT
cmpInst (Method _ id1 tys1 _ _ _) (Method _ id2 tys2 _ _ _) = (id1 `compare` id2) `thenCmp` (tys1 `tcCmpTypes` tys2)
cmpInst (Method _ _ _ _ _ _)      other			    = LT

cmpInst (LitInst _ _ _ _)	  (Dict _ _ _) 		    = GT
cmpInst (LitInst _ _ _ _)	  (Method _ _ _ _ _ _)	    = GT
cmpInst (LitInst _ lit1 ty1 _)	  (LitInst _ lit2 ty2 _)    = (lit1 `compare` lit2) `thenCmp` (ty1 `tcCmpType` ty2)
\end{code}


%************************************************************************
%*									*
\subsection[Inst-collections]{LIE: a collection of Insts}
%*									*
%************************************************************************

\begin{code}
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
data InstLoc = InstLoc InstOrigin SrcLoc ErrCtxt

instLocSrcLoc :: InstLoc -> SrcLoc
instLocSrcLoc (InstLoc _ src_loc _) = src_loc

data InstOrigin
  = OccurrenceOf Name		-- Occurrence of an overloaded identifier

  | IPOcc (IPName Name)		-- Occurrence of an implicit parameter
  | IPBind (IPName Name)	-- Binding site of an implicit parameter

  | RecordUpdOrigin

  | DataDeclOrigin		-- Typechecking a data declaration

  | InstanceDeclOrigin		-- Typechecking an instance decl

  | LiteralOrigin HsOverLit	-- Occurrence of a literal

  | PatOrigin RenamedPat

  | ArithSeqOrigin RenamedArithSeqInfo -- [x..], [x..y] etc
  | PArrSeqOrigin  RenamedArithSeqInfo -- [:x..y:] and [:x,y..z:]

  | SignatureOrigin		-- A dict created from a type signature
  | Rank2Origin			-- A dict created when typechecking the argument
				-- of a rank-2 typed function

  | DoOrigin			-- The monad for a do expression

  | ClassDeclOrigin		-- Manufactured during a class decl

  | InstanceSpecOrigin	Class	-- in a SPECIALIZE instance pragma
			Type

	-- When specialising instances the instance info attached to
	-- each class is not yet ready, so we record it inside the
	-- origin information.  This is a bit of a hack, but it works
	-- fine.  (Patrick is to blame [WDP].)

  | ValSpecOrigin	Name	-- in a SPECIALIZE pragma for a value

	-- Argument or result of a ccall
	-- Dictionaries with this origin aren't actually mentioned in the
	-- translated term, and so need not be bound.  Nor should they
	-- be abstracted over.

  | CCallOrigin		String			-- CCall label
			(Maybe RenamedHsExpr)	-- Nothing if it's the result
						-- Just arg, for an argument

  | LitLitOrigin	String	-- the litlit

  | UnknownOrigin	-- Help! I give up...
\end{code}

\begin{code}
pprInstLoc :: InstLoc -> SDoc
pprInstLoc (InstLoc orig locn ctxt)
  = hsep [text "arising from", pp_orig orig, text "at", ppr locn]
  where
    pp_orig (OccurrenceOf name)
      	= hsep [ptext SLIT("use of"), quotes (ppr name)]
    pp_orig (IPOcc name)
      	= hsep [ptext SLIT("use of implicit parameter"), quotes (ppr name)]
    pp_orig (IPBind name)
      	= hsep [ptext SLIT("binding for implicit parameter"), quotes (ppr name)]
    pp_orig RecordUpdOrigin
	= ptext SLIT("a record update")
    pp_orig DataDeclOrigin
	= ptext SLIT("the data type declaration")
    pp_orig InstanceDeclOrigin
	= ptext SLIT("the instance declaration")
    pp_orig (LiteralOrigin lit)
	= hsep [ptext SLIT("the literal"), quotes (ppr lit)]
    pp_orig (PatOrigin pat)
	= hsep [ptext SLIT("the pattern"), quotes (ppr pat)]
    pp_orig (ArithSeqOrigin seq)
	= hsep [ptext SLIT("the arithmetic sequence"), quotes (ppr seq)]
    pp_orig (PArrSeqOrigin seq)
	= hsep [ptext SLIT("the parallel array sequence"), quotes (ppr seq)]
    pp_orig (SignatureOrigin)
	=  ptext SLIT("a type signature")
    pp_orig (Rank2Origin)
	=  ptext SLIT("a function with an overloaded argument type")
    pp_orig (DoOrigin)
	=  ptext SLIT("a do statement")
    pp_orig (ClassDeclOrigin)
	=  ptext SLIT("a class declaration")
    pp_orig (InstanceSpecOrigin clas ty)
	= hsep [text "a SPECIALIZE instance pragma; class",
	        quotes (ppr clas), text "type:", ppr ty]
    pp_orig (ValSpecOrigin name)
	= hsep [ptext SLIT("a SPECIALIZE user-pragma for"), quotes (ppr name)]
    pp_orig (CCallOrigin clabel Nothing{-ccall result-})
	= hsep [ptext SLIT("the result of the _ccall_ to"), quotes (text clabel)]
    pp_orig (CCallOrigin clabel (Just arg_expr))
	= hsep [ptext SLIT("an argument in the _ccall_ to"), quotes (text clabel) <> comma, 
		text "namely", quotes (ppr arg_expr)]
    pp_orig (LitLitOrigin s)
	= hsep [ptext SLIT("the ``literal-literal''"), quotes (text s)]
    pp_orig (UnknownOrigin)
	= ptext SLIT("...oops -- I don't know where the overloading came from!")
\end{code}
