%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnBinds4]{Renaming and dependency analysis of bindings}

This module does renaming and dependency analysis on value bindings in
the abstract syntax.  It does {\em not} do cycle-checks on class or
type-synonym declarations; those cannot be done at this stage because
they may be affected by renaming (which isn't fully worked out yet).

\begin{code}
#include "HsVersions.h"

module RnBinds4 (
	rnTopBinds, rnMethodBinds,
	rnBinds,
	FreeVars(..), DefinedVars(..)

	-- and to make the interface self-sufficient...
   ) where

import Ubiq{-uitous-}
import RnLoop	-- break the RnPass4/RnExpr4/RnBinds4 loops

import HsSyn
import RdrHsSyn
import RnHsSyn
import HsPragmas	( noGenPragmas )
import RnMonad4

-- others:
import CmdLineOpts	( opt_SigsRequired )
import Digraph		( stronglyConnComp )
import ErrUtils		( addErrLoc, addShortErrLocLine )
import Maybes		( catMaybes )
import Name		( isUnboundName, Name{-instances-} )
import Pretty
import ProtoName	( elemByLocalNames, eqByLocalName, ProtoName{-instances-} )
import RnExpr4		-- OK to look here; but not the other way 'round
import UniqSet		( emptyUniqSet, singletonUniqSet, mkUniqSet,
			  unionUniqSets, unionManyUniqSets,
			  elementOfUniqSet,
			  uniqSetToList,
			  UniqSet(..)
			)
import Util		( isIn, removeDups, panic, panic# )
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

Sets of variable names are represented as sets explicitly, rather than lists.

\begin{code}
type DefinedVars = UniqSet Name
type FreeVars    = UniqSet Name
\end{code}

i.e., binders.

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

\begin{code}
rnTopBinds	:: ProtoNameHsBinds -> Rn4M RenamedHsBinds
rnMethodBinds  :: Name{-class-} -> ProtoNameMonoBinds -> Rn4M RenamedMonoBinds
rnBinds	:: ProtoNameHsBinds -> Rn4M (RenamedHsBinds, FreeVars, [Name])

rnTopBinds EmptyBinds		       	   = returnRn4 EmptyBinds
rnTopBinds (SingleBind (RecBind bind))    = rnTopMonoBinds bind []
rnTopBinds (BindWith (RecBind bind) sigs) = rnTopMonoBinds bind sigs
  -- the parser doesn't produce other forms

-- ********************************************************************

rnMethodBinds class_name EmptyMonoBinds = returnRn4 EmptyMonoBinds

rnMethodBinds class_name (AndMonoBinds mb1 mb2)
  = andRn4 AndMonoBinds (rnMethodBinds class_name mb1)
			(rnMethodBinds class_name mb2)

rnMethodBinds class_name (FunMonoBind pname matches locn)
  = pushSrcLocRn4 locn			      	  (
    lookupClassOp class_name pname  	`thenRn4` \ op_name ->
    mapAndUnzipRn4 rnMatch matches	`thenRn4` \ (new_matches, _) ->
    returnRn4 (FunMonoBind op_name new_matches locn)
    )

rnMethodBinds class_name (PatMonoBind (VarPatIn pname) grhss_and_binds locn)
  = pushSrcLocRn4 locn				  (
    lookupClassOp class_name pname	`thenRn4` \ op_name ->
    rnGRHSsAndBinds grhss_and_binds	`thenRn4` \ (grhss_and_binds', _) ->
    returnRn4 (PatMonoBind (VarPatIn op_name) grhss_and_binds' locn)
    )

-- Can't handle method pattern-bindings which bind multiple methods.
rnMethodBinds _ mbind@(PatMonoBind other_pat _ locn)
  = failButContinueRn4 EmptyMonoBinds (methodBindErr mbind locn)

-- ********************************************************************

rnBinds EmptyBinds			= returnRn4 (EmptyBinds,emptyUniqSet,[])
rnBinds (SingleBind (RecBind bind))	= rnNestedMonoBinds bind []
rnBinds (BindWith (RecBind bind) sigs) = rnNestedMonoBinds bind sigs
  -- the parser doesn't produce other forms
\end{code}

@rnNestedMonoBinds@
	- collects up the binders for this declaration group,
	- checkes that they form a set
	- extends the environment to bind them to new local names
	- calls @rnMonoBinds@ to do the real work

In contrast, @rnTopMonoBinds@ doesn't extend the environment, because that's
already done in pass3.	All it does is call @rnMonoBinds@ and discards
the free var info.

\begin{code}
rnTopMonoBinds :: ProtoNameMonoBinds -> [ProtoNameSig] -> Rn4M RenamedHsBinds

rnTopMonoBinds EmptyMonoBinds sigs = returnRn4 EmptyBinds

rnTopMonoBinds mbs sigs
 = rnBindSigs True{-top-level-} (collectMonoBinders mbs) sigs `thenRn4` \ siglist ->
   rnMonoBinds mbs siglist `thenRn4` \ (new_binds, fv_set) ->
   returnRn4 new_binds


rnNestedMonoBinds :: ProtoNameMonoBinds -> [ProtoNameSig]
		      -> Rn4M (RenamedHsBinds, FreeVars, [Name])

rnNestedMonoBinds EmptyMonoBinds sigs
  = returnRn4 (EmptyBinds, emptyUniqSet, [])

rnNestedMonoBinds mbinds sigs	-- Non-empty monobinds
  =
	-- Extract all the binders in this group,
	-- and extend current scope, inventing new names for the new binders
	-- This also checks that the names form a set
    let
	mbinders_w_srclocs = collectMonoBindersAndLocs mbinds
	mbinders    	   = map fst mbinders_w_srclocs
    in
    namesFromProtoNames
	"variable" -- in binding group
	mbinders_w_srclocs		`thenRn4` \ new_mbinders ->

    extendSS2 new_mbinders (
	 rnBindSigs False{-not top- level-} mbinders sigs `thenRn4` \ siglist ->
	 rnMonoBinds mbinds  siglist
    )					`thenRn4` \ (new_binds, fv_set) ->
    returnRn4 (new_binds, fv_set, new_mbinders)
\end{code}

@rnMonoBinds@ is used by *both* top-level and nested bindings.  It
assumes that all variables bound in this group are already in scope.
This is done *either* by pass 3 (for the top-level bindings),
*or* by @rnNestedMonoBinds@ (for the nested ones).

\begin{code}
rnMonoBinds :: ProtoNameMonoBinds
	     -> [RenamedSig]	-- Signatures attached to this group
	     -> Rn4M (RenamedHsBinds, FreeVars)

rnMonoBinds mbinds siglist
  =
	 -- Rename the bindings, returning a MonoBindsInfo
	 -- which is a list of indivisible vertices so far as
	 -- the strongly-connected-components (SCC) analysis is concerned
    flattenMonoBinds 0 siglist mbinds	`thenRn4` \ (_, mbinds_info) ->

	 -- Do the SCC analysis
    let vertices = mkVertices mbinds_info
	edges	= mkEdges vertices mbinds_info

	scc_result = stronglyConnComp (==) edges vertices

	 -- Deal with bound and free-var calculation
	rhs_free_vars = foldr f emptyUniqSet mbinds_info

	final_binds = reconstructRec scc_result edges mbinds_info

	happy_answer = returnRn4 (final_binds, rhs_free_vars)
    in
    case (inline_sigs_in_recursive_binds final_binds) of
      Nothing -> happy_answer
      Just names_n_locns ->
-- SLPJ: sometimes want recursive INLINE for worker wrapper style stuff
-- 	addErrRn4 (inlineInRecursiveBindsErr names_n_locns) `thenRn4_`
	{-not so-}happy_answer
  where
    f :: (a,b, FreeVars, c,d) -> FreeVars -> FreeVars

    f (_, _, fvs_body, _, _) fvs_sofar = fvs_sofar `unionUniqSets` fvs_body

    inline_sigs_in_recursive_binds (BindWith (RecBind _) sigs)
      = case [(n, locn) | (InlineSig n locn) <- sigs ] of
	  []   -> Nothing
	  sigh ->
#if OMIT_DEFORESTER
		Just sigh
#else
    		-- Allow INLINEd recursive functions if they are
		-- designated DEFORESTable too.
		case [(n, locn) | (DeforestSig n locn) <- sigs ] of
	  		[]   -> Just sigh
	  		sigh -> Nothing
#endif

    inline_sigs_in_recursive_binds (ThenBinds b1 b2)
      = case (inline_sigs_in_recursive_binds b1) of
	  Nothing -> inline_sigs_in_recursive_binds b2
	  Just  x -> Just x -- NB: won't report error(s) in b2

    inline_sigs_in_recursive_binds anything_else = Nothing
\end{code}

@flattenMonoBinds@ is ever-so-slightly magical in that it sticks
unique ``vertex tags'' on its output; minor plumbing required.

\begin{code}
flattenMonoBinds :: Int				-- Next free vertex tag
		 -> [RenamedSig]		-- Signatures
		 -> ProtoNameMonoBinds
		 -> Rn4M (Int, FlatMonoBindsInfo)

flattenMonoBinds uniq sigs EmptyMonoBinds = returnRn4 (uniq, [])

flattenMonoBinds uniq sigs (AndMonoBinds mB1 mB2)
  = flattenMonoBinds uniq sigs mB1	`thenRn4` \ (uniq1, flat1) ->
    flattenMonoBinds uniq1 sigs mB2	`thenRn4` \ (uniq2, flat2) ->
    returnRn4 (uniq2, flat1 ++ flat2)

flattenMonoBinds uniq sigs (PatMonoBind pat grhss_and_binds locn)
  = pushSrcLocRn4 locn		 		  (
    rnPat pat				`thenRn4` \ pat' ->
    rnGRHSsAndBinds grhss_and_binds	`thenRn4` \ (grhss_and_binds', fvs) ->

	 -- Find which things are bound in this group
    let
	names_bound_here = collectPatBinders pat'

	sigs_etc_for_here = foldl (sig_for_here (\ n -> n `is_elem` names_bound_here))
				  [] sigs

	sigs_fvs = foldr sig_fv emptyUniqSet sigs_etc_for_here

	is_elem = isIn "flattenMonoBinds"
    in
    returnRn4 (
	uniq + 1,
	[(uniq,
	  mkUniqSet names_bound_here,
	   fvs `unionUniqSets` sigs_fvs,
	   PatMonoBind pat' grhss_and_binds' locn,
	   sigs_etc_for_here
	 )]
    ))

flattenMonoBinds uniq sigs (FunMonoBind name matches locn)
  = pushSrcLocRn4 locn			    	  (
    lookupValue name			`thenRn4` \ name' ->
    mapAndUnzipRn4 rnMatch matches	`thenRn4` \ (new_matches, fv_lists) ->
    let
	fvs = unionManyUniqSets fv_lists

	sigs_for_me = foldl (sig_for_here (\ n -> n == name')) [] sigs

	sigs_fvs = foldr sig_fv emptyUniqSet sigs_for_me
    in
    returnRn4 (
      uniq + 1,
      [(uniq,
	singletonUniqSet name',
	fvs `unionUniqSets` sigs_fvs,
	FunMonoBind name' new_matches locn,
	sigs_for_me
	)]
    ))
\end{code}

Grab type-signatures/user-pragmas of interest:
\begin{code}
sig_for_here want_me acc s@(Sig n _ _ _)     | want_me n = s:acc
sig_for_here want_me acc s@(InlineSig n _)   | want_me n = s:acc
sig_for_here want_me acc s@(DeforestSig n _) | want_me n = s:acc
sig_for_here want_me acc s@(SpecSig n _ _ _) | want_me n = s:acc
sig_for_here want_me acc s@(MagicUnfoldingSig n _ _)
					     | want_me n = s:acc
sig_for_here want_me acc other_wise			 = acc

-- If a SPECIALIZE pragma is of the "... = blah" form,
-- then we'd better make sure "blah" is taken into
-- acct in the dependency analysis (or we get an
-- unexpected out-of-scope error)! WDP 95/07

sig_fv (SpecSig _ _ (Just blah) _) acc = acc `unionUniqSets` singletonUniqSet blah
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
reconstructRec	:: [Cycle]	-- Result of SCC analysis; at least one
		-> [Edge]	-- Original edges
		-> FlatMonoBindsInfo
		-> RenamedHsBinds

reconstructRec cycles edges mbi
  = foldr1 ThenBinds (map (reconstructCycle mbi) cycles)
  where
    reconstructCycle :: FlatMonoBindsInfo -> Cycle -> RenamedHsBinds

    reconstructCycle mbi2 cycle
      = BIND [(binds,sigs) | (vertex, _, _, binds, sigs) <- mbi2, vertex `is_elem` cycle]
		  _TO_ relevant_binds_and_sigs ->

	BIND (unzip relevant_binds_and_sigs) _TO_ (binds, sig_lists) ->

	BIND (foldr AndMonoBinds EmptyMonoBinds binds) _TO_ this_gp_binds ->
	let
	    this_gp_sigs	= foldr1 (++) sig_lists
	    have_sigs		= not (null sig_lists)
		-- ToDo: this might not be the right
		-- thing to call this predicate;
		-- e.g. "have_sigs [[], [], []]" ???????????
	in
	mk_binds this_gp_binds this_gp_sigs (isCyclic edges cycle) have_sigs
	BEND BEND BEND
      where
	is_elem = isIn "reconstructRec"

	mk_binds :: RenamedMonoBinds -> [RenamedSig]
		 -> Bool -> Bool -> RenamedHsBinds

	mk_binds bs ss True  False		= SingleBind (RecBind    bs)
	mk_binds bs ss True  True{-have sigs-}	= BindWith   (RecBind    bs) ss
	mk_binds bs ss False False		= SingleBind (NonRecBind bs)
	mk_binds bs ss False True{-have sigs-}	= BindWith   (NonRecBind bs) ss

	-- moved from Digraph, as this is the only use here
	-- (avoid overloading cost).  We have to use elem
	-- (not FiniteMaps or whatever), because there may be
	-- many edges out of one vertex.  We give it its own
	-- "elem" just for speed.

	isCyclic es []  = panic "isCyclic: empty component"
	isCyclic es [v] = (v,v) `elem` es
	isCyclic es vs  = True

	elem _ []	= False
	elem x (y:ys)	= x==y || elem x ys
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
  = [(VertexTag,		-- Identifies the vertex
      UniqSet Name,		-- Set of names defined in this vertex
      UniqSet Name,		-- Set of names used in this vertex
      RenamedMonoBinds,		-- Binding for this vertex (always just one binding, either fun or pat)
      [RenamedSig])		-- Signatures, if any, for this vertex
    ]

mkVertices :: FlatMonoBindsInfo -> [VertexTag]
mkVertices info = [ vertex | (vertex,_,_,_,_) <- info]

mkEdges :: [VertexTag] -> FlatMonoBindsInfo -> [Edge]

mkEdges vertices flat_info
 -- An edge (v,v') indicates that v depends on v'
 = [ (source_vertex, target_vertex)
   | (source_vertex, _, used_names, _, _) <- flat_info,
     target_name   <- uniqSetToList used_names,
     target_vertex <- vertices_defining target_name flat_info
   ]
   where
   -- If each name only has one binding in this group, then
   -- vertices_defining will always return the empty list, or a
   -- singleton.  The case when there is more than one binding (an
   -- error) needs more thought.

   vertices_defining name flat_info2
    = [ vertex |  (vertex, names_defined, _, _, _) <- flat_info2,
		name `elementOfUniqSet` names_defined
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
rnBindSigs :: Bool		    -- True <=> top-level binders
	    -> [ProtoName]	    -- Binders for this decl group
	    -> [ProtoNameSig]
	    -> Rn4M [RenamedSig]    -- List of Sig constructors

rnBindSigs is_toplev binder_pnames sigs
  =
	 -- Rename the signatures
	 -- Will complain about sigs for variables not in this group
    mapRn4 rename_sig sigs   `thenRn4` \ sigs_maybe ->
    let
	sigs' = catMaybes sigs_maybe

	 -- Discard unbound ones we've already complained about, so we
	 -- complain about duplicate ones.

	(goodies, dups) = removeDups compare (filter not_unbound sigs')
    in
    mapRn4 (addErrRn4 . dupSigDeclErr) dups `thenRn4_`

    getSrcLocRn4		`thenRn4` \ locn ->

    (if (is_toplev && opt_SigsRequired) then
	let
	    sig_frees = catMaybes (map (sig_free sigs) binder_pnames)
	in
	mapRn4 (addErrRn4 . missingSigErr locn) sig_frees
     else
	returnRn4 []
    )					    `thenRn4_`

    returnRn4 sigs' -- bad ones and all:
		    -- we need bindings of *some* sort for every name
  where
    rename_sig (Sig v ty pragma src_loc)
      = pushSrcLocRn4 src_loc 	(

	if not (v `elemByLocalNames` binder_pnames) then
	   addErrRn4 (unknownSigDeclErr "type signature" v src_loc) `thenRn4_`
	   returnRn4 Nothing
	else
	   lookupValue v				`thenRn4` \ new_v ->
	   rnPolyType False nullTyVarNamesEnv ty	`thenRn4` \ new_ty ->
	   recoverQuietlyRn4 noGenPragmas (
		rnGenPragmas pragma
	   )					    `thenRn4` \ new_pragma ->
	   returnRn4 (Just (Sig new_v new_ty new_pragma src_loc))
	)

    -- and now, the various flavours of value-modifying user-pragmas:

    rename_sig (SpecSig v ty using src_loc)
      = pushSrcLocRn4 src_loc 	(

	if not (v `elemByLocalNames` binder_pnames) then
	   addErrRn4 (unknownSigDeclErr "SPECIALIZE pragma" v src_loc) `thenRn4_`
	   returnRn4 Nothing
	else
	   lookupValue v				`thenRn4` \ new_v ->
	   rnPolyType False nullTyVarNamesEnv ty	`thenRn4` \ new_ty ->
	   rn_using using				`thenRn4` \ new_using ->
	   returnRn4 (Just (SpecSig new_v new_ty new_using src_loc))
	)
      where
	rn_using Nothing  = returnRn4 Nothing
	rn_using (Just x) = lookupValue x `thenRn4` \ new_x ->
			    returnRn4 (Just new_x)

    rename_sig (InlineSig v src_loc)
      = pushSrcLocRn4 src_loc 	(

	if not (v `elemByLocalNames` binder_pnames) then
	   addErrRn4 (unknownSigDeclErr "INLINE pragma" v src_loc) `thenRn4_`
	   returnRn4 Nothing
	else
	   lookupValue v	`thenRn4` \ new_v ->
	   returnRn4 (Just (InlineSig new_v src_loc))
	)

    rename_sig (DeforestSig v src_loc)
      = pushSrcLocRn4 src_loc   (

	if not (v `elemByLocalNames` binder_pnames) then
	   addErrRn4 (unknownSigDeclErr "DEFOREST pragma" v src_loc) `thenRn4_`
	   returnRn4 Nothing
	else
	   lookupValue v        `thenRn4` \ new_v ->
	   returnRn4 (Just (DeforestSig new_v src_loc))
	)

    rename_sig (MagicUnfoldingSig v str src_loc)
      = pushSrcLocRn4 src_loc 	(

	if not (v `elemByLocalNames` binder_pnames) then
	   addErrRn4 (unknownSigDeclErr "MAGIC_UNFOLDING pragma" v src_loc) `thenRn4_`
	   returnRn4 Nothing
	else
	   lookupValue v	`thenRn4` \ new_v ->
	   returnRn4 (Just (MagicUnfoldingSig new_v str src_loc))
	)

    not_unbound :: RenamedSig -> Bool

    not_unbound (Sig n _ _ _)		  = not (isUnboundName n)
    not_unbound (SpecSig n _ _ _)	  = not (isUnboundName n)
    not_unbound (InlineSig n _)		  = not (isUnboundName n)
    not_unbound (DeforestSig n _)	  = not (isUnboundName n)
    not_unbound (MagicUnfoldingSig n _ _) = not (isUnboundName n)

    -------------------------------------
    sig_free :: [ProtoNameSig] -> ProtoName -> Maybe ProtoName
	-- Return "Just x" if "x" has no type signature in
	-- sigs.  Nothing, otherwise.

    sig_free [] ny = Just ny
    sig_free (Sig nx _ _ _ : rest) ny
      = if (nx `eqByLocalName` ny) then Nothing else sig_free rest ny
    sig_free (_ : rest) ny = sig_free rest ny

    -------------------------------------
    compare :: RenamedSig -> RenamedSig -> TAG_
    compare x y = c x y

    c (Sig n1 _ _ _)	    	 (Sig n2 _ _ _)    	    = n1 `cmp` n2
    c (InlineSig n1 _)  	 (InlineSig n2 _) 	    = n1 `cmp` n2
    c (MagicUnfoldingSig n1 _ _) (MagicUnfoldingSig n2 _ _) = n1 `cmp` n2
    c (SpecSig n1 ty1 _ _)	 (SpecSig n2 ty2 _ _)
      = -- may have many specialisations for one value;
	-- but not ones that are exactly the same...
	case (n1 `cmp` n2) of
	  EQ_   -> cmpPolyType cmp ty1 ty2
	  other -> other

    c other_1 other_2	-- tags *must* be different
      = let tag1 = tag other_1
	    tag2 = tag other_2
	in
	if tag1 _LT_ tag2 then LT_ else GT_

    tag (Sig n1 _ _ _)	    	   = (ILIT(1) :: FAST_INT)
    tag (SpecSig n1 _ _ _)    	   = ILIT(2)
    tag (InlineSig n1 _)  	   = ILIT(3)
    tag (MagicUnfoldingSig n1 _ _) = ILIT(4)
    tag (DeforestSig n1 _)         = ILIT(5)
    tag _ = panic# "tag(RnBinds4)"
\end{code}

%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dupSigDeclErr sigs
  = let
	undup_sigs = fst (removeDups cmp_sig sigs)
    in
    addErrLoc locn1
	("more than one "++what_it_is++"\n\thas been given for these variables") ( \ sty ->
    ppAboves (map (ppr sty) undup_sigs) )
  where
    (what_it_is, locn1)
      = case (head sigs) of
	  Sig        _ _ _ loc -> ("type signature",loc)
	  ClassOpSig _ _ _ loc -> ("class-method type signature", loc)
	  SpecSig    _ _ _ loc -> ("SPECIALIZE pragma",loc)
	  InlineSig  _     loc -> ("INLINE pragma",loc)
	  MagicUnfoldingSig _ _ loc -> ("MAGIC_UNFOLDING pragma",loc)

    cmp_sig a b = get_name a `cmp` get_name b

    get_name (Sig        n _ _ _) = n
    get_name (ClassOpSig n _ _ _) = n
    get_name (SpecSig    n _ _ _) = n
    get_name (InlineSig  n     _) = n
    get_name (MagicUnfoldingSig n _ _) = n

------------------------
methodBindErr mbind locn
 = addErrLoc locn "Can't handle multiple methods defined by one pattern binding"
	(\ sty -> ppr sty mbind)

--------------------------
missingSigErr locn var
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr "a definition but no type signature for `",
	       ppr sty var,
	       ppStr "'."])

--------------------------------
unknownSigDeclErr flavor var locn
  = addShortErrLocLine locn ( \ sty ->
    ppBesides [ppStr flavor, ppStr " but no definition for `",
	       ppr sty var,
	       ppStr "'."])
\end{code}
