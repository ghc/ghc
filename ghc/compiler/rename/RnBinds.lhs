%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnBinds]{Renaming and dependency analysis of bindings}

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).

\begin{code}
module RnBinds (
	rnTopBinds, rnTopMonoBinds,
	rnMethodBinds, renameSigs,
	rnBinds,
	unknownSigErr
   ) where

#include "HsVersions.h"

import {-# SOURCE #-} RnSource ( rnHsSigType )

import HsSyn
import HsBinds		( sigsForMe )
import RdrHsSyn
import RnHsSyn
import RnMonad
import RnExpr		( rnMatch, rnGRHSs, rnPat, checkPrecMatch )
import RnEnv		( bindLocatedLocalsRn, lookupBndrRn, lookupGlobalOccRn,
			  warnUnusedLocalBinds, mapFvRn, 
			  FreeVars, emptyFVs, plusFV, plusFVs, unitFV, addOneFV,
			  unknownNameErr
			)
import CmdLineOpts	( opt_WarnMissingSigs )
import Digraph		( stronglyConnComp, SCC(..) )
import Name		( OccName, Name, nameOccName )
import NameSet
import RdrName		( RdrName, rdrNameOcc  )
import BasicTypes	( RecFlag(..), TopLevelFlag(..) )
import Util		( thenCmp, removeDups )
import List		( partition )
import ListSetOps	( minusList )
import Bag		( bagToList )
import FiniteMap	( lookupFM, listToFM )
import Maybe		( isJust )
import Outputable
\end{code}

-- ToDo: Put the annotations into the monad, so that they arrive in the proper
-- place and can be used when complaining.

The code tree received by the function @rnBinds@ contains definitions
in where-clauses which are all apparently mutually recursive, but which may
not really depend upon each other. For example, in the top level program
\begin{verbatim}
f x = y where a = x
	      y = x
\end{verbatim}
the definitions of @a@ and @y@ do not depend on each other at all.
Unfortunately, the typechecker cannot always check such definitions.
\footnote{Mycroft, A. 1984. Polymorphic type schemes and recursive
definitions. In Proceedings of the International Symposium on Programming,
Toulouse, pp. 217-39. LNCS 167. Springer Verlag.}
However, the typechecker usually can check definitions in which only the
strongly connected components have been collected into recursive bindings.
This is precisely what the function @rnBinds@ does.

ToDo: deal with case where a single monobinds binds the same variable
twice.

The vertag tag is a unique @Int@; the tags only need to be unique
within one @MonoBinds@, so that unique-Int plumbing is done explicitly
(heavy monad machinery not needed).

\begin{code}
type VertexTag	= Int
type Cycle	= [VertexTag]
type Edge	= (VertexTag, VertexTag)
\end{code}

%************************************************************************
%*									*
%* naming conventions							*
%*									*
%************************************************************************

\subsection[name-conventions]{Name conventions}

The basic algorithm involves walking over the tree and returning a tuple
containing the new tree plus its free variables. Some functions, such
as those walking polymorphic bindings (HsBinds) and qualifier lists in
list comprehensions (@Quals@), return the variables bound in local
environments. These are then used to calculate the free variables of the
expression evaluated in these environments.

Conventions for variable names are as follows:
\begin{itemize}
\item
new code is given a prime to distinguish it from the old.

\item
a set of variables defined in @Exp@ is written @dvExp@

\item
a set of variables free in @Exp@ is written @fvExp@
\end{itemize}

%************************************************************************
%*									*
%* analysing polymorphic bindings (HsBinds, Bind, MonoBinds)		*
%*									*
%************************************************************************

\subsubsection[dep-HsBinds]{Polymorphic bindings}

Non-recursive expressions are reconstructed without any changes at top
level, although their component expressions may have to be altered.
However, non-recursive expressions are currently not expected as
\Haskell{} programs, and this code should not be executed.

Monomorphic bindings contain information that is returned in a tuple
(a @FlatMonoBindsInfo@) containing:

\begin{enumerate}
\item
a unique @Int@ that serves as the ``vertex tag'' for this binding.

\item
the name of a function or the names in a pattern. These are a set
referred to as @dvLhs@, the defined variables of the left hand side.

\item
the free variables of the body. These are referred to as @fvBody@.

\item
the definition's actual code. This is referred to as just @code@.
\end{enumerate}

The function @nonRecDvFv@ returns two sets of variables. The first is
the set of variables defined in the set of monomorphic bindings, while the
second is the set of free variables in those bindings.

The set of variables defined in a non-recursive binding is just the
union of all of them, as @union@ removes duplicates. However, the
free variables in each successive set of cumulative bindings is the
union of those in the previous set plus those of the newest binding after
the defined variables of the previous set have been removed.

@rnMethodBinds@ deals only with the declarations in class and
instance declarations.	It expects only to see @FunMonoBind@s, and
it expects the global environment to contain bindings for the binders
(which are all class operations).

%************************************************************************
%*									*
%* 		Top-level bindings
%*									*
%************************************************************************

@rnTopBinds@ assumes that the environment already
contains bindings for the binders of this particular binding.

\begin{code}
rnTopBinds    :: RdrNameHsBinds -> RnMS (RenamedHsBinds, FreeVars)

rnTopBinds EmptyBinds		       	  = returnRn (EmptyBinds, emptyFVs)
rnTopBinds (MonoBind bind sigs _) 	  = rnTopMonoBinds bind sigs
  -- The parser doesn't produce other forms


rnTopMonoBinds EmptyMonoBinds sigs 
  = returnRn (EmptyBinds, emptyFVs)

rnTopMonoBinds mbinds sigs
 =  mapRn lookupBndrRn binder_rdr_names	`thenRn` \ binder_names ->
    let
	binder_set    = mkNameSet binder_names
	binder_occ_fm = listToFM [(nameOccName x,x) | x <- binder_names]
    in
    renameSigs opt_WarnMissingSigs binder_set
	       (lookupSigOccRn binder_occ_fm) sigs	`thenRn` \ (siglist, sig_fvs) ->
    rn_mono_binds siglist mbinds			`thenRn` \ (final_binds, bind_fvs) ->
    returnRn (final_binds, bind_fvs `plusFV` sig_fvs)
  where
    binder_rdr_names = map fst (bagToList (collectMonoBinders mbinds))

-- the names appearing in the sigs have to be bound by 
-- this group's binders.
lookupSigOccRn binder_occ_fm rdr_name
  = case lookupFM binder_occ_fm (rdrNameOcc rdr_name) of
	Nothing -> failWithRn (mkUnboundName rdr_name)
			      (unknownNameErr rdr_name)
	Just x  -> returnRn x
\end{code}

%************************************************************************
%*									*
%* 		Nested binds
%*									*
%************************************************************************

@rnMonoBinds@
	- collects up the binders for this declaration group,
	- checks that they form a set
	- extends the environment to bind them to new local names
	- calls @rnMonoBinds@ to do the real work

\begin{code}
rnBinds	      :: RdrNameHsBinds 
	      -> (RenamedHsBinds -> RnMS (result, FreeVars))
	      -> RnMS (result, FreeVars)

rnBinds EmptyBinds	       thing_inside = thing_inside EmptyBinds
rnBinds (MonoBind bind sigs _) thing_inside = rnMonoBinds bind sigs thing_inside
  -- the parser doesn't produce other forms


rnMonoBinds :: RdrNameMonoBinds 
            -> [RdrNameSig]
	    -> (RenamedHsBinds -> RnMS (result, FreeVars))
	    -> RnMS (result, FreeVars)

rnMonoBinds EmptyMonoBinds sigs thing_inside = thing_inside EmptyBinds

rnMonoBinds mbinds sigs	thing_inside -- Non-empty monobinds
  =	-- Extract all the binders in this group,
	-- and extend current scope, inventing new names for the new binders
	-- This also checks that the names form a set
    bindLocatedLocalsRn (text "a binding group") mbinders_w_srclocs		$ \ new_mbinders ->
    let
	binder_set  = mkNameSet new_mbinders

	   -- Weed out the fixity declarations that do not
	   -- apply to any of the binders in this group.
	(sigs_for_me, fixes_not_for_me) = partition forLocalBind sigs

	forLocalBind (FixSig sig@(FixitySig name _ _ )) =
	    isJust (lookupFM binder_occ_fm (rdrNameOcc name))
	forLocalBind _ = True

	binder_occ_fm = listToFM [(nameOccName x,x) | x <- new_mbinders]

    in
       -- Report the fixity declarations in this group that 
       -- don't refer to any of the group's binders.
       --
    mapRn_ (unknownSigErr) fixes_not_for_me     `thenRn_`
    renameSigs False binder_set
	       (lookupSigOccRn binder_occ_fm) sigs_for_me   `thenRn` \ (siglist, sig_fvs) ->
    let
	fixity_sigs = [(name,sig) | FixSig sig@(FixitySig name _ _) <- siglist ]
    in
       -- Install the fixity declarations that do apply here and go.
    extendFixityEnv fixity_sigs (
      rn_mono_binds siglist mbinds
    )	 				   `thenRn` \ (binds, bind_fvs) ->

	-- Now do the "thing inside", and deal with the free-variable calculations
    thing_inside binds					`thenRn` \ (result,result_fvs) ->
    let
	all_fvs        = result_fvs `plusFV` bind_fvs `plusFV` sig_fvs
	unused_binders = nameSetToList (binder_set `minusNameSet` all_fvs)
    in
    warnUnusedLocalBinds unused_binders	`thenRn_`
    returnRn (result, delListFromNameSet all_fvs new_mbinders)
  where
    mbinders_w_srclocs = bagToList (collectMonoBinders mbinds)
\end{code}


%************************************************************************
%*									*
%* 		MonoBinds -- the main work is done here
%*									*
%************************************************************************

@rn_mono_binds@ is used by *both* top-level and nested bindings.  It
assumes that all variables bound in this group are already in scope.
This is done *either* by pass 3 (for the top-level bindings), *or* by
@rnMonoBinds@ (for the nested ones).

\begin{code}
rn_mono_binds :: [RenamedSig]	        -- Signatures attached to this group
	      -> RdrNameMonoBinds	
	      -> RnMS (RenamedHsBinds, 	-- 
		         FreeVars)	-- Free variables

rn_mono_binds siglist mbinds
  =
	 -- Rename the bindings, returning a MonoBindsInfo
	 -- which is a list of indivisible vertices so far as
	 -- the strongly-connected-components (SCC) analysis is concerned
    flattenMonoBinds siglist mbinds		`thenRn` \ mbinds_info ->

	 -- Do the SCC analysis
    let 
        edges	    = mkEdges (mbinds_info `zip` [(0::Int)..])
	scc_result  = stronglyConnComp edges
	final_binds = foldr1 ThenBinds (map reconstructCycle scc_result)

	 -- Deal with bound and free-var calculation
	rhs_fvs = plusFVs [fvs | (_,fvs,_,_) <- mbinds_info]
    in
    returnRn (final_binds, rhs_fvs)
\end{code}

@flattenMonoBinds@ is ever-so-slightly magical in that it sticks
unique ``vertex tags'' on its output; minor plumbing required.

Sigh - need to pass along the signatures for the group of bindings,
in case any of them 

\begin{code}
flattenMonoBinds :: [RenamedSig]		-- Signatures
		 -> RdrNameMonoBinds
		 -> RnMS [FlatMonoBindsInfo]

flattenMonoBinds sigs EmptyMonoBinds = returnRn []

flattenMonoBinds sigs (AndMonoBinds bs1 bs2)
  = flattenMonoBinds sigs bs1	`thenRn` \ flat1 ->
    flattenMonoBinds sigs bs2	`thenRn` \ flat2 ->
    returnRn (flat1 ++ flat2)

flattenMonoBinds sigs (PatMonoBind pat grhss locn)
  = pushSrcLocRn locn		 	$
    rnPat pat				`thenRn` \ (pat', pat_fvs) ->

	 -- Find which things are bound in this group
    let
	names_bound_here = mkNameSet (collectPatBinders pat')
	sigs_for_me      = sigsForMe (`elemNameSet` names_bound_here) sigs
    in
    rnGRHSs grhss			`thenRn` \ (grhss', fvs) ->
    returnRn 
	[(names_bound_here,
	  fvs `plusFV` pat_fvs,
	  PatMonoBind pat' grhss' locn,
	  sigs_for_me
	 )]

flattenMonoBinds sigs (FunMonoBind name inf matches locn)
  = pushSrcLocRn locn				 	$
    lookupBndrRn name					`thenRn` \ new_name ->
    let
	sigs_for_me = sigsForMe (new_name ==) sigs
    in
    mapFvRn rnMatch matches				`thenRn` \ (new_matches, fvs) ->
    mapRn_ (checkPrecMatch inf new_name) new_matches	`thenRn_`
    returnRn
      [(unitNameSet new_name,
	fvs,
	FunMonoBind new_name inf new_matches locn,
	sigs_for_me
	)]
\end{code}


@rnMethodBinds@ is used for the method bindings of a class and an instance
declaration.   like @rnMonoBinds@ but without dependency analysis.

NOTA BENE: we record each *binder* of a method-bind group as a free variable.
That's crucial when dealing with an instance decl:
	instance Foo (T a) where
	   op x = ...
This might be the *sole* occurrence of 'op' for an imported class Foo,
and unless op occurs we won't treat the type signature of op in the class
decl for Foo as a source of instance-decl gates.  But we should!  Indeed,
in many ways the op in an instance decl is just like an occurrence, not
a binder.

\begin{code}
rnMethodBinds :: RdrNameMonoBinds -> RnMS (RenamedMonoBinds, FreeVars)

rnMethodBinds EmptyMonoBinds = returnRn (EmptyMonoBinds, emptyFVs)

rnMethodBinds (AndMonoBinds mb1 mb2)
  = rnMethodBinds mb1	`thenRn` \ (mb1', fvs1) ->
    rnMethodBinds mb2	`thenRn` \ (mb2', fvs2) ->
    returnRn (mb1' `AndMonoBinds` mb2', fvs1 `plusFV` fvs2)

rnMethodBinds (FunMonoBind name inf matches locn)
  = pushSrcLocRn locn				   	$

    lookupGlobalOccRn name				`thenRn` \ sel_name -> 
	-- We use the selector name as the binder

    mapFvRn rnMatch matches				`thenRn` \ (new_matches, fvs) ->
    mapRn_ (checkPrecMatch inf sel_name) new_matches	`thenRn_`
    returnRn (FunMonoBind sel_name inf new_matches locn, fvs `addOneFV` sel_name)

rnMethodBinds (PatMonoBind (VarPatIn name) grhss locn)
  = pushSrcLocRn locn			$
    lookupGlobalOccRn name		`thenRn` \ sel_name -> 
    rnGRHSs grhss			`thenRn` \ (grhss', fvs) ->
    returnRn (PatMonoBind (VarPatIn sel_name) grhss' locn, fvs `addOneFV` sel_name)

-- Can't handle method pattern-bindings which bind multiple methods.
rnMethodBinds mbind@(PatMonoBind other_pat _ locn)
  = pushSrcLocRn locn	$
    failWithRn (EmptyMonoBinds, emptyFVs) (methodBindErr mbind)
\end{code}


%************************************************************************
%*									*
\subsection[reconstruct-deps]{Reconstructing dependencies}
%*									*
%************************************************************************

This @MonoBinds@- and @ClassDecls@-specific code is segregated here,
as the two cases are similar.

\begin{code}
reconstructCycle :: SCC FlatMonoBindsInfo
		 -> RenamedHsBinds

reconstructCycle (AcyclicSCC (_, _, binds, sigs))
  = MonoBind binds sigs NonRecursive

reconstructCycle (CyclicSCC cycle)
  = MonoBind this_gp_binds this_gp_sigs Recursive
  where
    this_gp_binds      = foldr1 AndMonoBinds [binds | (_, _, binds, _) <- cycle]
    this_gp_sigs       = foldr1 (++)	     [sigs  | (_, _, _, sigs) <- cycle]
\end{code}

%************************************************************************
%*									*
%*	Manipulating FlatMonoBindInfo					*
%*									*
%************************************************************************

During analysis a @MonoBinds@ is flattened to a @FlatMonoBindsInfo@.
The @RenamedMonoBinds@ is always an empty bind, a pattern binding or
a function binding, and has itself been dependency-analysed and
renamed.

\begin{code}
type FlatMonoBindsInfo
  = (NameSet,			-- Set of names defined in this vertex
     NameSet,			-- Set of names used in this vertex
     RenamedMonoBinds,
     [RenamedSig])		-- Signatures, if any, for this vertex

mkEdges :: [(FlatMonoBindsInfo, VertexTag)] -> [(FlatMonoBindsInfo, VertexTag, [VertexTag])]

mkEdges flat_info
  = [ (info, tag, dest_vertices (nameSetToList names_used))
    | (info@(names_defined, names_used, mbind, sigs), tag) <- flat_info
    ]
  where
 	 -- An edge (v,v') indicates that v depends on v'
    dest_vertices src_mentions = [ target_vertex
			         | ((names_defined, _, _, _), target_vertex) <- flat_info,
				   mentioned_name <- src_mentions,
				   mentioned_name `elemNameSet` names_defined
			         ]
\end{code}


%************************************************************************
%*									*
\subsubsection[dep-Sigs]{Signatures (and user-pragmas for values)}
%*									*
%************************************************************************

@renameSigs@ checks for: (a)~more than one sig for one thing;
(b)~signatures given for things not bound here; (c)~with suitably
flaggery, that all top-level things have type signatures.

At the moment we don't gather free-var info from the types in
signatures.  We'd only need this if we wanted to report unused tyvars.

\begin{code}
renameSigs ::  Bool			-- True => warn if (required) type signatures are missing.
	    -> NameSet			-- Set of names bound in this group
	    -> (RdrName -> RnMS Name)
	    -> [RdrNameSig]
	    -> RnMS ([RenamedSig], FreeVars)		 -- List of Sig constructors

renameSigs sigs_required binders lookup_occ_nm sigs
  =	 -- Rename the signatures
    mapFvRn (renameSig lookup_occ_nm) sigs   	`thenRn` \ (sigs', fvs) ->

	-- Check for (a) duplicate signatures
	--	     (b) signatures for things not in this group
	--	     (c) optionally, bindings with no signature
    let
	(goodies, dups) = removeDups cmp_sig (sigsForMe (not . isUnboundName) sigs')
	not_this_group  = sigsForMe (not . (`elemNameSet` binders)) goodies
	type_sig_vars	= [n | Sig n _ _     <- goodies]
	un_sigd_binders | sigs_required = nameSetToList binders `minusList` type_sig_vars
			| otherwise	= []
    in
    mapRn_ dupSigDeclErr dups 				`thenRn_`
    mapRn_ unknownSigErr not_this_group			`thenRn_`
    mapRn_ (addWarnRn.missingSigWarn) un_sigd_binders	`thenRn_`
    returnRn (sigs', fvs)	
		-- bad ones and all:
		-- we need bindings of *some* sort for every name

-- We use lookupOccRn in the signatures, which is a little bit unsatisfactory
-- because this won't work for:
--	instance Foo T where
--	  {-# INLINE op #-}
--	  Baz.op = ...
-- We'll just rename the INLINE prag to refer to whatever other 'op'
-- is in scope.  (I'm assuming that Baz.op isn't in scope unqualified.)
-- Doesn't seem worth much trouble to sort this.

renameSig lookup_occ_nm (Sig v ty src_loc)
  = pushSrcLocRn src_loc $
    lookup_occ_nm v				`thenRn` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty		`thenRn` \ (new_ty,fvs) ->
    returnRn (Sig new_v new_ty src_loc, fvs `addOneFV` new_v)

renameSig _ (SpecInstSig ty src_loc)
  = pushSrcLocRn src_loc $
    rnHsSigType (text "A SPECIALISE instance pragma") ty	`thenRn` \ (new_ty, fvs) ->
    returnRn (SpecInstSig new_ty src_loc, fvs)

renameSig lookup_occ_nm (SpecSig v ty src_loc)
  = pushSrcLocRn src_loc $
    lookup_occ_nm v			`thenRn` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty	`thenRn` \ (new_ty,fvs) ->
    returnRn (SpecSig new_v new_ty src_loc, fvs `addOneFV` new_v)

renameSig lookup_occ_nm (InlineSig v src_loc)
  = pushSrcLocRn src_loc $
    lookup_occ_nm v		`thenRn` \ new_v ->
    returnRn (InlineSig new_v src_loc, unitFV new_v)

renameSig lookup_occ_nm (FixSig (FixitySig v fix src_loc))
  = pushSrcLocRn src_loc $
    lookup_occ_nm v		`thenRn` \ new_v ->
    returnRn (FixSig (FixitySig new_v fix src_loc), unitFV new_v)

renameSig lookup_occ_nm (NoInlineSig v src_loc)
  = pushSrcLocRn src_loc $
    lookup_occ_nm v		`thenRn` \ new_v ->
    returnRn (NoInlineSig new_v src_loc, unitFV new_v)
\end{code}

Checking for distinct signatures; oh, so boring

\begin{code}
cmp_sig :: RenamedSig -> RenamedSig -> Ordering
cmp_sig (Sig n1 _ _)	     (Sig n2 _ _)    	  = n1 `compare` n2
cmp_sig (InlineSig n1 _)     (InlineSig n2 _) 	  = n1 `compare` n2
cmp_sig (NoInlineSig n1 _)   (NoInlineSig n2 _)	  = n1 `compare` n2
cmp_sig (SpecInstSig ty1 _)  (SpecInstSig ty2 _)  = cmpHsType compare ty1 ty2
cmp_sig (SpecSig n1 ty1 _)   (SpecSig n2 ty2 _) 
  = -- may have many specialisations for one value;
    -- but not ones that are exactly the same...
	thenCmp (n1 `compare` n2) (cmpHsType compare ty1 ty2)

cmp_sig other_1 other_2					-- Tags *must* be different
  | (sig_tag other_1) _LT_ (sig_tag other_2) = LT 
  | otherwise				     = GT

sig_tag (Sig n1 _ _)    	   = (ILIT(1) :: FAST_INT)
sig_tag (SpecSig n1 _ _)    	   = ILIT(2)
sig_tag (InlineSig n1 _)  	   = ILIT(3)
sig_tag (NoInlineSig n1 _)  	   = ILIT(4)
sig_tag (SpecInstSig _ _)	   = ILIT(5)
sig_tag (FixSig _)		   = ILIT(6)
sig_tag _			   = panic# "tag(RnBinds)"
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dupSigDeclErr (sig:sigs)
  = pushSrcLocRn loc $
    addErrRn (sep [ptext SLIT("Duplicate") <+> ptext what_it_is <> colon,
		   ppr sig])
  where
    (what_it_is, loc) = sig_doc sig

unknownSigErr sig
  = pushSrcLocRn loc $
    addErrRn (sep [ptext SLIT("Misplaced"),
		   ptext what_it_is <> colon,
		   ppr sig])
  where
    (what_it_is, loc) = sig_doc sig

sig_doc (Sig        _ _ loc) 	     = (SLIT("type signature"),loc)
sig_doc (ClassOpSig _ _ _ loc) 	     = (SLIT("class-method type signature"), loc)
sig_doc (SpecSig    _ _ loc) 	     = (SLIT("SPECIALISE pragma"),loc)
sig_doc (InlineSig  _     loc) 	     = (SLIT("INLINE pragma"),loc)
sig_doc (NoInlineSig  _   loc) 	     = (SLIT("NOINLINE pragma"),loc)
sig_doc (SpecInstSig _ loc)	     = (SLIT("SPECIALISE instance pragma"),loc)
sig_doc (FixSig (FixitySig _ _ loc)) = (SLIT("fixity declaration"), loc)

missingSigWarn var
  = sep [ptext SLIT("definition but no type signature for"), quotes (ppr var)]

methodBindErr mbind
 =  hang (ptext SLIT("Can't handle multiple methods defined by one pattern binding"))
       4 (ppr mbind)
\end{code}
