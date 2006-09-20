%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1999
%

Analysis functions over data types.  Specficially, detecting recursive types.

This stuff is only used for source-code decls; it's recorded in interface
files for imported data types.


\begin{code}
module TcTyDecls(
	calcRecFlags, 
	calcClassCycles, calcSynCycles
    ) where

#include "HsVersions.h"

import TypeRep          ( Type(..), TyNote(..), PredType(..) )  -- friend
import HsSyn		( TyClDecl(..), HsPred(..), LTyClDecl, isClassDecl )
import RnHsSyn		( extractHsTyNames )
import Type		( predTypeRep, tcView )
import HscTypes		( TyThing(..), ModDetails(..) )
import TyCon            ( TyCon, tyConArity, tyConDataCons, tyConTyVars,
                          isSynTyCon, isAlgTyCon, 
			  tyConName, isNewTyCon, isProductTyCon, newTyConRhs,
			  isOpenTyCon )
import Class		( classTyCon )
import DataCon          ( dataConOrigArgTys )
import Var              ( TyVar )
import VarSet
import Name		( Name, isTyVarName )
import NameEnv
import NameSet
import Digraph 		( SCC(..), stronglyConnComp, stronglyConnCompR )
import BasicTypes	( RecFlag(..) )
import SrcLoc		( Located(..), unLoc )
import Outputable
\end{code}


%************************************************************************
%*									*
	Cycles in class and type synonym declarations
%*									*
%************************************************************************

Checking for class-decl loops is easy, because we don't allow class decls
in interface files.

We allow type synonyms in hi-boot files, but we *trust* hi-boot files, 
so we don't check for loops that involve them.  So we only look for synonym
loops in the module being compiled.

We check for type synonym and class cycles on the *source* code.
Main reasons: 

  a) Otherwise we'd need a special function to extract type-synonym tycons
	from a type, whereas we have extractHsTyNames already

  b) If we checked for type synonym loops after building the TyCon, we
	can't do a hoistForAllTys on the type synonym rhs, (else we fall into
	a black hole) which seems unclean.  Apart from anything else, it'd mean 
	that a type-synonym rhs	could have for-alls to the right of an arrow, 
	which means adding new cases to the validity checker

	Indeed, in general, checking for cycles beforehand means we need to
	be less careful about black holes through synonym cycles.

The main disadvantage is that a cycle that goes via a type synonym in an 
.hi-boot file can lead the compiler into a loop, because it assumes that cycles
only occur entirely within the source code of the module being compiled.  
But hi-boot files are trusted anyway, so this isn't much worse than (say) 
a kind error.

[  NOTE ----------------------------------------------
If we reverse this decision, this comment came from tcTyDecl1, and should
 go back there
	-- dsHsType, not tcHsKindedType, to avoid a loop.  tcHsKindedType does hoisting,
	-- which requires looking through synonyms... and therefore goes into a loop
	-- on (erroneously) recursive synonyms.
	-- Solution: do not hoist synonyms, because they'll be hoisted soon enough
	-- 	     when they are substituted

We'd also need to add back in this definition

synTyConsOfType :: Type -> [TyCon]
-- Does not look through type synonyms at all
-- Return a list of synonym tycons
synTyConsOfType ty
  = nameEnvElts (go ty)
  where
     go :: Type -> NameEnv TyCon  -- The NameEnv does duplicate elim
     go (TyVarTy v)	  	  = emptyNameEnv
     go (TyConApp tc tys) 	  = go_tc tc tys
     go (AppTy a b)	  	  = go a `plusNameEnv` go b
     go (FunTy a b)	  	  = go a `plusNameEnv` go b
     go (PredTy (IParam _ ty))    = go ty	
     go (PredTy (ClassP cls tys)) = go_s tys	-- Ignore class
     go (NoteTy _ ty)		  = go ty	
     go (ForAllTy _ ty)	  	  = go ty

     go_tc tc tys | isSynTyCon tc = extendNameEnv (go_s tys) (tyConName tc) tc
		  | otherwise	  = go_s tys
     go_s tys = foldr (plusNameEnv . go) emptyNameEnv tys
---------------------------------------- END NOTE ]

\begin{code}
calcSynCycles :: [LTyClDecl Name] -> [SCC (LTyClDecl Name)]
calcSynCycles decls
  = stronglyConnComp syn_edges
  where
    syn_edges = [ (ldecl, unLoc (tcdLName decl), 
			  mk_syn_edges (tcdSynRhs decl))
		| ldecl@(L _ decl) <- decls ]

    mk_syn_edges rhs = [ tc | tc <- nameSetToList (extractHsTyNames rhs), 
			      not (isTyVarName tc) ]


calcClassCycles :: [LTyClDecl Name] -> [[LTyClDecl Name]]
calcClassCycles decls
  = [decls | CyclicSCC decls <- stronglyConnComp cls_edges]
  where
    cls_edges = [ (ldecl, unLoc (tcdLName decl), 	
			  mk_cls_edges (unLoc (tcdCtxt decl)))
		| ldecl@(L _ decl) <- decls, isClassDecl decl ]

    mk_cls_edges ctxt = [ cls | L _ (HsClassP cls _) <- ctxt ]
\end{code}


%************************************************************************
%*									*
	Deciding which type constructors are recursive
%*									*
%************************************************************************

For newtypes, we label some as "recursive" such that

    INVARIANT: there is no cycle of non-recursive newtypes

In any loop, only one newtype need be marked as recursive; it is
a "loop breaker".  Labelling more than necessary as recursive is OK,
provided the invariant is maintained.

A newtype M.T is defined to be "recursive" iff
	(a) it is declared in an hi-boot file (see RdrHsSyn.hsIfaceDecl)
	(b) it is declared in a source file, but that source file has a
	    companion hi-boot file which declares the type
   or	(c) one can get from T's rhs to T via type 
	    synonyms, or non-recursive newtypes *in M*
	     e.g.  newtype T = MkT (T -> Int)

(a) is conservative; declarations in hi-boot files are always 
	made loop breakers. That's why in (b) we can restrict attention
	to tycons in M, because any loops through newtypes outside M
	will be broken by those newtypes
(b) ensures that a newtype is not treated as a loop breaker in one place
and later as a non-loop-breaker.  This matters in GHCi particularly, when
a newtype T might be embedded in many types in the environment, and then
T's source module is compiled.  We don't want T's recursiveness to change.

The "recursive" flag for algebraic data types is irrelevant (never consulted)
for types with more than one constructor.

An algebraic data type M.T is "recursive" iff
	it has just one constructor, and 
	(a) it is declared in an hi-boot file (see RdrHsSyn.hsIfaceDecl)
	(b) it is declared in a source file, but that source file has a
	    companion hi-boot file which declares the type
 or	(c) one can get from its arg types to T via type synonyms, 
	    or by non-recursive newtypes or non-recursive product types in M
	     e.g.  data T = MkT (T -> Int) Bool
Just like newtype in fact

A type synonym is recursive if one can get from its
right hand side back to it via type synonyms.  (This is
reported as an error.)

A class is recursive if one can get from its superclasses
back to it.  (This is an error too.)

Hi-boot types
~~~~~~~~~~~~~
A data type read from an hi-boot file will have an AbstractTyCon as its AlgTyConRhs
and will respond True to isHiBootTyCon. The idea is that we treat these as if one
could get from these types to anywhere.  So when we see

  	module Baz where
	import {-# SOURCE #-} Foo( T )
	newtype S = MkS T

then we mark S as recursive, just in case. What that means is that if we see

	import Baz( S )
	newtype R = MkR S

then we don't need to look inside S to compute R's recursiveness.  Since S is imported
(not from an hi-boot file), one cannot get from R back to S except via an hi-boot file,
and that means that some data type will be marked recursive along the way.  So R is
unconditionly non-recursive (i.e. there'll be a loop breaker elsewhere if necessary)

This in turn means that we grovel through fewer interface files when computing 
recursiveness, because we need only look at the type decls in the module being
compiled, plus the outer structure of directly-mentioned types.

\begin{code}
calcRecFlags :: ModDetails -> [TyThing] -> (Name -> RecFlag)
-- The 'boot_names' are the things declared in M.hi-boot, if M is the current module.
-- Any type constructors in boot_names are automatically considered loop breakers
calcRecFlags boot_details tyclss
  = is_rec
  where
    is_rec n | n `elemNameSet` rec_names = Recursive
	     | otherwise		 = NonRecursive

    boot_name_set = md_exports boot_details
    rec_names = boot_name_set	  `unionNameSets` 
		nt_loop_breakers  `unionNameSets`
	        prod_loop_breakers

    all_tycons = [ tc | tycls <- tyclss,
			   -- Recursion of newtypes/data types can happen via 
			   -- the class TyCon, so tyclss includes the class tycons
			let tc = getTyCon tycls,
			not (tyConName tc `elemNameSet` boot_name_set) ]
			   -- Remove the boot_name_set because they are going 
			   -- to be loop breakers regardless.

	-------------------------------------------------
	-- 			NOTE
	-- These edge-construction loops rely on
	-- every loop going via tyclss, the types and classes
	-- in the module being compiled.  Stuff in interface 
	-- files should be correctly marked.  If not (e.g. a
	-- type synonym in a hi-boot file) we can get an infinite
	-- loop.  We could program round this, but it'd make the code
	-- rather less nice, so I'm not going to do that yet.

	--------------- Newtypes ----------------------
    new_tycons = filter isNewTyConAndNotOpen all_tycons
    isNewTyConAndNotOpen tycon = isNewTyCon tycon && not (isOpenTyCon tycon)
    nt_loop_breakers = mkNameSet (findLoopBreakers nt_edges)
    is_rec_nt tc = tyConName tc  `elemNameSet` nt_loop_breakers
	-- is_rec_nt is a locally-used helper function

    nt_edges = [(t, mk_nt_edges t) | t <- new_tycons]

    mk_nt_edges nt 	-- Invariant: nt is a newtype
	= concatMap (mk_nt_edges1 nt) (tcTyConsOfType (new_tc_rhs nt))
			-- tyConsOfType looks through synonyms

    mk_nt_edges1 nt tc 
	| tc `elem` new_tycons = [tc]		-- Loop
		-- At this point we know that either it's a local *data* type,
		-- or it's imported.  Either way, it can't form part of a newtype cycle
	| otherwise = []

	--------------- Product types ----------------------
	-- The "prod_tycons" are the non-newtype products
    prod_tycons = [tc | tc <- all_tycons, 
			not (isNewTyCon tc), isProductTyCon tc]
    prod_loop_breakers = mkNameSet (findLoopBreakers prod_edges)

    prod_edges = [(tc, mk_prod_edges tc) | tc <- prod_tycons]
	
    mk_prod_edges tc 	-- Invariant: tc is a product tycon
	= concatMap (mk_prod_edges1 tc) (dataConOrigArgTys (head (tyConDataCons tc)))

    mk_prod_edges1 ptc ty = concatMap (mk_prod_edges2 ptc) (tcTyConsOfType ty)

    mk_prod_edges2 ptc tc 
 	| tc `elem` prod_tycons   = [tc]		-- Local product
 	| tc `elem` new_tycons    = if is_rec_nt tc 	-- Local newtype
				    then []
				    else mk_prod_edges1 ptc (new_tc_rhs tc)
		-- At this point we know that either it's a local non-product data type,
		-- or it's imported.  Either way, it can't form part of a cycle
	| otherwise = []
			
new_tc_rhs tc = snd (newTyConRhs tc)	-- Ignore the type variables

getTyCon (ATyCon tc) = tc
getTyCon (AClass cl) = classTyCon cl

findLoopBreakers :: [(TyCon, [TyCon])] -> [Name]
-- Finds a set of tycons that cut all loops
findLoopBreakers deps
  = go [(tc,tc,ds) | (tc,ds) <- deps]
  where
    go edges = [ name
	       | CyclicSCC ((tc,_,_) : edges') <- stronglyConnCompR edges,
		 name <- tyConName tc : go edges']
\end{code}

These two functions know about type representations, so they could be
in Type or TcType -- but they are very specialised to this module, so 
I've chosen to put them here.

\begin{code}
tcTyConsOfType :: Type -> [TyCon]
-- tcTyConsOfType looks through all synonyms, but not through any newtypes.  
-- When it finds a Class, it returns the class TyCon.  The reaons it's here
-- (not in Type.lhs) is because it is newtype-aware.
tcTyConsOfType ty 
  = nameEnvElts (go ty)
  where
     go :: Type -> NameEnv TyCon  -- The NameEnv does duplicate elim
     go ty | Just ty' <- tcView ty = go ty'
     go (TyVarTy v)	  	   = emptyNameEnv
     go (TyConApp tc tys) 	   = go_tc tc tys
     go (AppTy a b)	  	   = go a `plusNameEnv` go b
     go (FunTy a b)	  	   = go a `plusNameEnv` go b
     go (PredTy (IParam _ ty))     = go ty
     go (PredTy (ClassP cls tys))  = go_tc (classTyCon cls) tys
     go (ForAllTy _ ty)	  	   = go ty

     go_tc tc tys = extendNameEnv (go_s tys) (tyConName tc) tc
     go_s tys = foldr (plusNameEnv . go) emptyNameEnv tys
\end{code}
