%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnBinds]{Renaming and dependency analysis of bindings}

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).

\begin{code}
#include "HsVersions.h"

module RnBinds (
	rnTopBinds, rnTopMonoBinds,
	rnMethodBinds,
	rnBinds, rnMonoBinds
   ) where

IMP_Ubiq()
IMPORT_DELOOPER(RnLoop)		-- break the RnPass/RnExpr/RnBinds loops

import HsSyn
import HsPragmas	( isNoGenPragmas, noGenPragmas )
import RdrHsSyn
import RnHsSyn
import RnMonad
import RnExpr		( rnMatch, rnGRHSsAndBinds, rnPat, checkPrecMatch )
import RnEnv		( bindLocatedLocalsRn, lookupBndrRn, lookupOccRn, newLocalNames, isUnboundName )

import CmdLineOpts	( opt_SigsRequired )
import Digraph		( stronglyConnComp, SCC(..) )
import ErrUtils		( addErrLoc, addShortErrLocLine )
import Name		( OccName(..), Provenance, 
			  Name {- instance Eq -},
			  NameSet(..), emptyNameSet, mkNameSet, unionNameSets, 
		 	  minusNameSet, unionManyNameSets, elemNameSet, unitNameSet, nameSetToList
			)
import Maybes		( catMaybes )
--import PprStyle--ToDo:rm
import Pretty
import Util		( Ord3(..), thenCmp, isIn, removeDups, panic, panic#, assertPanic, assocDefault )
import UniqSet		( SYN_IE(UniqSet) )
import ListSetOps	( minusList )
import Bag		( bagToList )
import UniqFM		( UniqFM )
import ErrUtils		( SYN_IE(Error) )
#if __GLASGOW_HASKELL__ >= 202
import Outputable
#endif

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
rnTopBinds    :: RdrNameHsBinds -> RnMS s RenamedHsBinds

rnTopBinds EmptyBinds		       	  = returnRn EmptyBinds
rnTopBinds (MonoBind bind sigs _) 	  = rnTopMonoBinds bind sigs
  -- The parser doesn't produce other forms


rnTopMonoBinds EmptyMonoBinds sigs 
  = returnRn EmptyBinds

rnTopMonoBinds mbinds sigs
 =  mapRn lookupBndrRn binder_rdr_names	`thenRn` \ binder_names ->
    let
	binder_set = mkNameSet binder_names
    in
    rn_mono_binds True {- top level -}
		  binder_set mbinds sigs		`thenRn` \ (new_binds, fv_set) ->
    returnRn new_binds
  where
    binder_rdr_names = map fst (bagToList (collectMonoBinders mbinds))
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
	      -> (RenamedHsBinds -> RnMS s (result, FreeVars))
	      -> RnMS s (result, FreeVars)

rnBinds EmptyBinds	       thing_inside = thing_inside EmptyBinds
rnBinds (MonoBind bind sigs _) thing_inside = rnMonoBinds bind sigs thing_inside
  -- the parser doesn't produce other forms


rnMonoBinds :: RdrNameMonoBinds -> [RdrNameSig]
	    -> (RenamedHsBinds -> RnMS s (result, FreeVars))
	    -> RnMS s (result, FreeVars)

rnMonoBinds EmptyMonoBinds sigs thing_inside = thing_inside EmptyBinds

rnMonoBinds mbinds sigs	thing_inside -- Non-empty monobinds
  =	-- Extract all the binders in this group,
	-- and extend current scope, inventing new names for the new binders
	-- This also checks that the names form a set
    bindLocatedLocalsRn (\_ -> text "binding group") mbinders_w_srclocs		$ \ new_mbinders ->
    let
	binder_set = mkNameSet new_mbinders
    in
    rn_mono_binds False {- not top level -}
		  binder_set mbinds sigs	`thenRn` \ (binds,bind_fvs) ->

	-- Now do the "thing inside", and deal with the free-variable calculations
    thing_inside binds					`thenRn` \ (result,result_fvs) ->
    returnRn (result, (result_fvs `unionNameSets` bind_fvs) `minusNameSet` binder_set)
  where
    mbinders_w_srclocs = bagToList (collectMonoBinders mbinds)
\end{code}


%************************************************************************
%*									*
%* 		MonoBinds -- the main work is done here
%*									*
%************************************************************************

@rnMonoBinds@ is used by *both* top-level and nested bindings.  It
assumes that all variables bound in this group are already in scope.
This is done *either* by pass 3 (for the top-level bindings), *or* by
@rnNestedMonoBinds@ (for the nested ones).

\begin{code}
rn_mono_binds :: Bool			-- True <=> top level
	      -> NameSet		-- Binders of this group
	      -> RdrNameMonoBinds	
	      -> [RdrNameSig]		-- Signatures attached to this group
	      -> RnMS s (RenamedHsBinds, 	-- 
		         FreeVars)	-- Free variables

rn_mono_binds is_top_lev binders mbinds sigs
  =
	 -- Rename the bindings, returning a MonoBindsInfo
	 -- which is a list of indivisible vertices so far as
	 -- the strongly-connected-components (SCC) analysis is concerned
    rnBindSigs is_top_lev binders sigs	`thenRn` \ siglist ->
    flattenMonoBinds 0 siglist mbinds	`thenRn` \ (_, mbinds_info) ->

	 -- Do the SCC analysis
    let edges	    = mkEdges mbinds_info
	scc_result  = stronglyConnComp edges
	final_binds = foldr1 ThenBinds (map reconstructCycle scc_result)

	 -- Deal with bound and free-var calculation
	rhs_fvs = unionManyNameSets [fvs | (_,_,fvs,_,_) <- mbinds_info]
    in
    returnRn (final_binds, rhs_fvs)
\end{code}

@flattenMonoBinds@ is ever-so-slightly magical in that it sticks
unique ``vertex tags'' on its output; minor plumbing required.

\begin{code}
flattenMonoBinds :: Int				-- Next free vertex tag
		 -> [RenamedSig]		-- Signatures
		 -> RdrNameMonoBinds
		 -> RnMS s (Int, [FlatMonoBindsInfo])

flattenMonoBinds uniq sigs EmptyMonoBinds = returnRn (uniq, [])

flattenMonoBinds uniq sigs (AndMonoBinds bs1 bs2)
  = flattenMonoBinds uniq  sigs bs1	`thenRn` \ (uniq1, flat1) ->
    flattenMonoBinds uniq1 sigs bs2	`thenRn` \ (uniq2, flat2) ->
    returnRn (uniq2, flat1 ++ flat2)

flattenMonoBinds uniq sigs (PatMonoBind pat grhss_and_binds locn)
  = pushSrcLocRn locn		 	$
    rnPat pat				`thenRn` \ pat' ->
    rnGRHSsAndBinds grhss_and_binds	`thenRn` \ (grhss_and_binds', fvs) ->

	 -- Find which things are bound in this group
    let
	names_bound_here = mkNameSet (collectPatBinders pat')
	sigs_for_me      = filter ((`elemNameSet` names_bound_here) . sig_name) sigs
	sigs_fvs         = foldr sig_fv emptyNameSet sigs_for_me
    in
    returnRn (
	uniq + 1,
	[(uniq,
	  names_bound_here,
	  fvs `unionNameSets` sigs_fvs,
	  PatMonoBind pat' grhss_and_binds' locn,
	  sigs_for_me
	 )]
    )

flattenMonoBinds uniq sigs (FunMonoBind name inf matches locn)
  = pushSrcLocRn locn				 $
    mapRn (checkPrecMatch inf name) matches	`thenRn_`
    lookupBndrRn name				`thenRn` \ name' ->
    mapAndUnzipRn rnMatch matches		`thenRn` \ (new_matches, fv_lists) ->
    let
	fvs	    = unionManyNameSets fv_lists
	sigs_for_me = filter ((name' ==) . sig_name) sigs
	sigs_fvs    = foldr sig_fv emptyNameSet sigs_for_me
    in
    returnRn (
      uniq + 1,
      [(uniq,
	unitNameSet name',
	fvs `unionNameSets` sigs_fvs,
	FunMonoBind name' inf new_matches locn,
	sigs_for_me
	)]
    )
\end{code}


@rnMethodBinds@ is used for the method bindings of an instance
declaration.   like @rnMonoBinds@ but without dependency analysis.

\begin{code}
rnMethodBinds :: RdrNameMonoBinds -> RnMS s RenamedMonoBinds

rnMethodBinds EmptyMonoBinds = returnRn EmptyMonoBinds

rnMethodBinds (AndMonoBinds mb1 mb2)
  = andRn AndMonoBinds (rnMethodBinds mb1)
		       (rnMethodBinds mb2)

rnMethodBinds (FunMonoBind occname inf matches locn)
  = pushSrcLocRn locn				   $
    mapRn (checkPrecMatch inf occname) matches	`thenRn_`

    newLocalNames [(occname, locn)]		`thenRn` \ [op_name] ->
	-- Make a fresh local for the bound variable; it must be different
	-- to occurrences of the same thing on the LHS, which refer to the global
	-- selectors.

    mapAndUnzipRn rnMatch matches		`thenRn` \ (new_matches, _) ->
    returnRn (FunMonoBind op_name inf new_matches locn)

rnMethodBinds (PatMonoBind (VarPatIn occname) grhss_and_binds locn)
  = pushSrcLocRn locn			$
    newLocalNames [(occname, locn)]	`thenRn` \ [op_name] ->
    rnGRHSsAndBinds grhss_and_binds	`thenRn` \ (grhss_and_binds', _) ->
    returnRn (PatMonoBind (VarPatIn op_name) grhss_and_binds' locn)

-- Can't handle method pattern-bindings which bind multiple methods.
rnMethodBinds mbind@(PatMonoBind other_pat _ locn)
  = pushSrcLocRn locn	$
    failWithRn EmptyMonoBinds (methodBindErr mbind)
\end{code}

\begin{code}
-- If a SPECIALIZE pragma is of the "... = blah" form,
-- then we'd better make sure "blah" is taken into
-- acct in the dependency analysis (or we get an
-- unexpected out-of-scope error)! WDP 95/07

sig_fv (SpecSig _ _ (Just blah) _) acc = acc `unionNameSets` (unitNameSet blah)
sig_fv _			   acc = acc
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

reconstructCycle (AcyclicSCC (_, _, _, binds, sigs))
  = MonoBind binds sigs nonRecursive

reconstructCycle (CyclicSCC cycle)
  = MonoBind this_gp_binds this_gp_sigs recursive
  where
    this_gp_binds      = foldr1 AndMonoBinds [binds | (_, _, _, binds, _) <- cycle]
    this_gp_sigs       = foldr1 (++)	     [sigs  | (_, _, _, _, sigs) <- cycle]
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
  = (VertexTag,			-- Identifies the vertex
     NameSet,			-- Set of names defined in this vertex
     NameSet,			-- Set of names used in this vertex
     RenamedMonoBinds,		-- Binding for this vertex (always just one binding, either fun or pat)
     [RenamedSig])		-- Signatures, if any, for this vertex


mkEdges :: [FlatMonoBindsInfo] -> [(FlatMonoBindsInfo, VertexTag, [VertexTag])]

mkEdges flat_info
  = [ (info, tag, dest_vertices (nameSetToList names_used))
    | info@(tag, names_defined, names_used, mbind, sigs) <- flat_info
    ]
  where
 	 -- An edge (v,v') indicates that v depends on v'
    dest_vertices src_mentions = [ target_vertex
			         | (target_vertex, names_defined, _, _, _) <- flat_info,
				   mentioned_name <- src_mentions,
				   mentioned_name `elemNameSet` names_defined
			         ]
\end{code}


%************************************************************************
%*									*
\subsubsection[dep-Sigs]{Signatures (and user-pragmas for values)}
%*									*
%************************************************************************

@rnBindSigs@ checks for: (a)~more than one sig for one thing;
(b)~signatures given for things not bound here; (c)~with suitably
flaggery, that all top-level things have type signatures.

\begin{code}
rnBindSigs :: Bool		    	-- True <=> top-level binders
	    -> NameSet			-- Set of names bound in this group
	    -> [RdrNameSig]
	    -> RnMS s [RenamedSig]		 -- List of Sig constructors

rnBindSigs is_toplev binders sigs
  =	 -- Rename the signatures
    mapRn renameSig sigs   	`thenRn` \ sigs' ->

	-- Check for (a) duplicate signatures
	--	     (b) signatures for things not in this group
	--	     (c) optionally, bindings with no signature
    let
	(goodies, dups) = removeDups cmp_sig (filter (not.isUnboundName.sig_name) sigs')
	not_this_group  = filter (\sig -> not (sig_name sig `elemNameSet` binders)) goodies
	type_sig_vars	= [n | Sig n _ _ <- goodies]
	un_sigd_binders 
	    | is_toplev && opt_SigsRequired = nameSetToList binders `minusList` type_sig_vars
	    | otherwise			    = []
    in
    mapRn dupSigDeclErr dups 				`thenRn_`
    mapRn unknownSigErr not_this_group			`thenRn_`
    mapRn (addErrRn.missingSigErr) un_sigd_binders	`thenRn_`

    returnRn sigs' -- bad ones and all:
		   -- we need bindings of *some* sort for every name


renameSig (Sig v ty src_loc)
  = pushSrcLocRn src_loc $
    lookupBndrRn v				`thenRn` \ new_v ->
    rnHsSigType (\ sty -> ppr sty v) ty		`thenRn` \ new_ty ->
    returnRn (Sig new_v new_ty src_loc)

renameSig (SpecSig v ty using src_loc)
  = pushSrcLocRn src_loc $
    lookupBndrRn v			`thenRn` \ new_v ->
    rnHsSigType (\ sty -> ppr sty v) ty	`thenRn` \ new_ty ->
    rn_using using			`thenRn` \ new_using ->
    returnRn (SpecSig new_v new_ty new_using src_loc)
  where
    rn_using Nothing  = returnRn Nothing
    rn_using (Just x) = lookupOccRn x `thenRn` \ new_x ->
			returnRn (Just new_x)

renameSig (InlineSig v src_loc)
  = pushSrcLocRn src_loc $
    lookupBndrRn v		`thenRn` \ new_v ->
    returnRn (InlineSig new_v src_loc)

renameSig (DeforestSig v src_loc)
  = pushSrcLocRn src_loc $
    lookupBndrRn v        `thenRn` \ new_v ->
    returnRn (DeforestSig new_v src_loc)

renameSig (MagicUnfoldingSig v str src_loc)
  = pushSrcLocRn src_loc $
    lookupBndrRn v		`thenRn` \ new_v ->
    returnRn (MagicUnfoldingSig new_v str src_loc)
\end{code}

Checking for distinct signatures; oh, so boring

\begin{code}
cmp_sig :: RenamedSig -> RenamedSig -> TAG_
cmp_sig (Sig n1 _ _)	           (Sig n2 _ _)    	  = n1 `cmp` n2
cmp_sig (InlineSig n1 _)  	   (InlineSig n2 _) 	  = n1 `cmp` n2
cmp_sig (MagicUnfoldingSig n1 _ _) (MagicUnfoldingSig n2 _ _) = n1 `cmp` n2
cmp_sig (SpecSig n1 ty1 _ _)       (SpecSig n2 ty2 _ _)
  = -- may have many specialisations for one value;
	-- but not ones that are exactly the same...
	thenCmp (n1 `cmp` n2) (cmpHsType cmp ty1 ty2)

cmp_sig other_1 other_2					-- Tags *must* be different
  | (sig_tag other_1) _LT_ (sig_tag other_2) = LT_ 
  | otherwise				     = GT_

sig_tag (Sig n1 _ _)    	   = (ILIT(1) :: FAST_INT)
sig_tag (SpecSig n1 _ _ _)    	   = ILIT(2)
sig_tag (InlineSig n1 _)  	   = ILIT(3)
sig_tag (MagicUnfoldingSig n1 _ _) = ILIT(4)
sig_tag (DeforestSig n1 _)         = ILIT(5)
sig_tag _			   = panic# "tag(RnBinds)"

sig_name (Sig        n _ _) 	   = n
sig_name (ClassOpSig n _ _ _) 	   = n
sig_name (SpecSig    n _ _ _) 	   = n
sig_name (InlineSig  n     _) 	   = n  
sig_name (MagicUnfoldingSig n _ _) = n
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dupSigDeclErr (sig:sigs)
  = pushSrcLocRn loc $
    addErrRn (\sty -> sep [ptext SLIT("more than one"), 
		      	     ptext what_it_is, ptext SLIT("given for"), 
			     ppr sty (sig_name sig)])
  where
    (what_it_is, loc) = sig_doc sig

unknownSigErr sig
  = pushSrcLocRn loc $
    addErrRn (\sty -> sep [ptext flavour, ptext SLIT("but no definition for"),
			     ppr sty (sig_name sig)])
  where
    (flavour, loc) = sig_doc sig

sig_doc (Sig        _ _ loc) 	    = (SLIT("type signature"),loc)
sig_doc (ClassOpSig _ _ _ loc) 	    = (SLIT("class-method type signature"), loc)
sig_doc (SpecSig    _ _ _ loc) 	    = (SLIT("SPECIALIZE pragma"),loc)
sig_doc (InlineSig  _     loc) 	    = (SLIT("INLINE pragma"),loc)
sig_doc (MagicUnfoldingSig _ _ loc) = (SLIT("MAGIC_UNFOLDING pragma"),loc)

missingSigErr var sty
  = sep [ptext SLIT("a definition but no type signature for"), ppr sty var]

methodBindErr mbind sty
 =  hang (ptext SLIT("Can't handle multiple methods defined by one pattern binding"))
	   4 (ppr sty mbind)
\end{code}
