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
	rnTopMonoBinds, rnMonoBinds, rnMonoBindsAndThen,
	rnMethodBinds, renameSigs, checkSigs, unknownSigErr
   ) where

#include "HsVersions.h"


import HsSyn
import HsBinds		( eqHsSig, hsSigDoc )
import RdrHsSyn
import RnHsSyn
import TcRnMonad
import RnTypes		( rnHsSigType, rnHsType, rnPat )
import RnExpr		( rnMatch, rnGRHSs, checkPrecMatch )
import RnEnv		( bindLocatedLocalsRn, lookupBndrRn, lookupInstDeclBndr,
			  lookupSigOccRn, bindPatSigTyVars, bindLocalFixities,
			  warnUnusedLocalBinds, mapFvRn, extendTyVarEnvFVRn,
			)
import CmdLineOpts	( DynFlag(..) )
import Digraph		( SCC(..), stronglyConnComp )
import Name		( Name, nameOccName, nameSrcLoc )
import NameSet
import RdrName		( RdrName, rdrNameOcc )
import BasicTypes	( RecFlag(..) )
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
(a @FlatMonoBinds@) containing:

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
\subsubsection{ Top-level bindings}
%*									*
%************************************************************************

@rnTopMonoBinds@ assumes that the environment already
contains bindings for the binders of this particular binding.

\begin{code}
rnTopMonoBinds :: RdrNameMonoBinds 
	       -> [RdrNameSig]
	       -> RnM (RenamedHsBinds, FreeVars)

-- Assumes the binders of the binding are in scope already
-- Very like rnMonoBinds, bu checks for missing signatures too

rnTopMonoBinds mbinds sigs
 =  bindPatSigTyVars (collectSigTysFromMonoBinds mbinds) $ 
	-- Hmm; by analogy with Ids, this doesn't look right

    renameSigs sigs			`thenM` \ siglist ->
    rn_mono_binds siglist mbinds	`thenM` \ (binders, final_binds, bind_fvs) ->
    checkSigs okBindSig binders siglist	`thenM_`

	-- Warn about missing signatures, but not in interface mode
	-- (This is important when renaming bindings from 'deriving' clauses.)
    getModeRn						`thenM` \ mode ->
    doptM Opt_WarnMissingSigs 				`thenM` \ warn_missing_sigs ->
    (if warn_missing_sigs && not (isInterfaceMode mode) then
	let
	    type_sig_vars   = [n | Sig n _ _ <- siglist]
	    un_sigd_binders = filter (not . (`elem` type_sig_vars)) 
				     (nameSetToList binders)
	in
        mappM_ missingSigWarn un_sigd_binders
     else
	returnM ()  
    )						`thenM_`

    returnM (final_binds, bind_fvs `plusFV` hsSigsFVs siglist)
\end{code}


%************************************************************************
%*									*
%* 		Nested binds
%*									*
%************************************************************************

\begin{code}
rnMonoBindsAndThen :: RdrNameMonoBinds 
	           -> [RdrNameSig]
	   	   -> (RenamedHsBinds -> RnM (result, FreeVars))
		   -> RnM (result, FreeVars)

rnMonoBindsAndThen mbinds sigs thing_inside -- Non-empty monobinds
  =	-- Extract all the binders in this group, and extend the
	-- current scope, inventing new names for the new binders
	-- This also checks that the names form a set
    bindLocatedLocalsRn doc mbinders_w_srclocs			$ \ new_mbinders ->
    bindPatSigTyVars (collectSigTysFromMonoBinds mbinds)	$ 

	-- Then install local fixity declarations
	-- Notice that they scope over thing_inside too
    bindLocalFixities [sig | FixSig sig <- sigs ] 	$

	-- Do the business
    rnMonoBinds mbinds sigs		`thenM` \ (binds, bind_fvs) ->

	-- Now do the "thing inside"
    thing_inside binds 			   `thenM` \ (result,result_fvs) ->

	-- Final error checking
    let
	all_fvs        = result_fvs `plusFV` bind_fvs
	unused_binders = filter (not . (`elemNameSet` all_fvs)) new_mbinders
    in
    warnUnusedLocalBinds unused_binders		`thenM_`

    returnM (result, delListFromNameSet all_fvs new_mbinders)
  where
    mbinders_w_srclocs = collectLocatedMonoBinders mbinds
    doc = text "In the binding group for:"
	  <+> pprWithCommas ppr (map fst mbinders_w_srclocs)
\end{code}


\begin{code}
rnMonoBinds :: RdrNameMonoBinds 
	    -> [RdrNameSig]
	    -> RnM (RenamedHsBinds, FreeVars)

-- Assumes the binders of the binding are in scope already

rnMonoBinds mbinds sigs
 =  renameSigs sigs			`thenM` \ siglist ->
    rn_mono_binds siglist mbinds	`thenM` \ (binders, final_binds, bind_fvs) ->
    checkSigs okBindSig binders siglist	`thenM_`
    returnM (final_binds, bind_fvs `plusFV` hsSigsFVs siglist)
\end{code}

%************************************************************************
%*									*
\subsubsection{		MonoBinds -- the main work is done here}
%*									*
%************************************************************************

@rn_mono_binds@ is used by {\em both} top-level and nested bindings.
It assumes that all variables bound in this group are already in scope.
This is done {\em either} by pass 3 (for the top-level bindings),
{\em or} by @rnMonoBinds@ (for the nested ones).

\begin{code}
rn_mono_binds :: [RenamedSig]	        -- Signatures attached to this group
	      -> RdrNameMonoBinds	
	      -> RnM (NameSet,		-- Binders
		      RenamedHsBinds, 	-- Dependency analysed
		      FreeVars)		-- Free variables

rn_mono_binds siglist mbinds
  =	 -- Rename the bindings, returning a MonoBindsInfo
	 -- which is a list of indivisible vertices so far as
	 -- the strongly-connected-components (SCC) analysis is concerned
    flattenMonoBinds siglist mbinds		`thenM` \ mbinds_info ->

	 -- Do the SCC analysis
    let 
	scc_result  = rnSCC mbinds_info
	final_binds = foldr (ThenBinds . reconstructCycle) EmptyBinds scc_result

	-- Deal with bound and free-var calculation
	-- Caller removes binders from free-var set
	rhs_fvs = plusFVs [fvs  | (_,fvs,_)  <- mbinds_info]
	bndrs   = plusFVs [defs | (defs,_,_) <- mbinds_info]
    in
    returnM (bndrs, final_binds, rhs_fvs)
\end{code}

@flattenMonoBinds@ is ever-so-slightly magical in that it sticks
unique ``vertex tags'' on its output; minor plumbing required.

Sigh --- need to pass along the signatures for the group of bindings,
in case any of them \fbox{\ ???\ } 

\begin{code}
flattenMonoBinds :: [RenamedSig]		-- Signatures
		 -> RdrNameMonoBinds
		 -> RnM [FlatMonoBinds]

flattenMonoBinds sigs EmptyMonoBinds = returnM []

flattenMonoBinds sigs (AndMonoBinds bs1 bs2)
  = flattenMonoBinds sigs bs1	`thenM` \ flat1 ->
    flattenMonoBinds sigs bs2	`thenM` \ flat2 ->
    returnM (flat1 ++ flat2)

flattenMonoBinds sigs (PatMonoBind pat grhss locn)
  = addSrcLoc locn		 	$
    rnPat pat				`thenM` \ (pat', pat_fvs) ->

	 -- Find which things are bound in this group
    let
	names_bound_here = mkNameSet (collectPatBinders pat')
    in
    sigsForMe names_bound_here sigs	`thenM` \ sigs_for_me ->
    rnGRHSs PatBindRhs grhss		`thenM` \ (grhss', fvs) ->
    returnM 
	[(names_bound_here,
	  fvs `plusFV` pat_fvs,
	  (PatMonoBind pat' grhss' locn, sigs_for_me)
	)]

flattenMonoBinds sigs (FunMonoBind name inf matches locn)
  = addSrcLoc locn				 	$
    lookupBndrRn name					`thenM` \ new_name ->
    let
	names_bound_here = unitNameSet new_name
    in
    sigsForMe names_bound_here sigs			`thenM` \ sigs_for_me ->
    mapFvRn (rnMatch (FunRhs new_name)) matches		`thenM` \ (new_matches, fvs) ->
    mappM_ (checkPrecMatch inf new_name) new_matches	`thenM_`
    returnM
      [(unitNameSet new_name,
	fvs,
	(FunMonoBind new_name inf new_matches locn, sigs_for_me)
      )]


sigsForMe names_bound_here sigs
  = foldlM check [] (filter (sigForThisGroup names_bound_here) sigs)
  where
    check sigs sig = case filter (eqHsSig sig) sigs of
			[]    -> returnM (sig:sigs)
			other -> dupSigDeclErr sig	`thenM_`
				 returnM sigs
\end{code}


@rnMethodBinds@ is used for the method bindings of a class and an instance
declaration.   Like @rnMonoBinds@ but without dependency analysis.

NOTA BENE: we record each {\em binder} of a method-bind group as a free variable.
That's crucial when dealing with an instance decl:
\begin{verbatim}
	instance Foo (T a) where
	   op x = ...
\end{verbatim}
This might be the {\em sole} occurrence of @op@ for an imported class @Foo@,
and unless @op@ occurs we won't treat the type signature of @op@ in the class
decl for @Foo@ as a source of instance-decl gates.  But we should!  Indeed,
in many ways the @op@ in an instance decl is just like an occurrence, not
a binder.

\begin{code}
rnMethodBinds :: Name			-- Class name
	      -> [Name]			-- Names for generic type variables
	      -> RdrNameMonoBinds
	      -> RnM (RenamedMonoBinds, FreeVars)

rnMethodBinds cls gen_tyvars EmptyMonoBinds = returnM (EmptyMonoBinds, emptyFVs)

rnMethodBinds cls gen_tyvars (AndMonoBinds mb1 mb2)
  = rnMethodBinds cls gen_tyvars mb1	`thenM` \ (mb1', fvs1) ->
    rnMethodBinds cls gen_tyvars mb2	`thenM` \ (mb2', fvs2) ->
    returnM (mb1' `AndMonoBinds` mb2', fvs1 `plusFV` fvs2)

rnMethodBinds cls gen_tyvars (FunMonoBind name inf matches locn)
  = addSrcLoc locn				   	$

    lookupInstDeclBndr cls name				`thenM` \ sel_name -> 
	-- We use the selector name as the binder

    mapFvRn (rn_match sel_name) matches			`thenM` \ (new_matches, fvs) ->
    mappM_ (checkPrecMatch inf sel_name) new_matches	`thenM_`
    returnM (FunMonoBind sel_name inf new_matches locn, fvs `addOneFV` sel_name)
  where
	-- Gruesome; bring into scope the correct members of the generic type variables
	-- See comments in RnSource.rnSourceDecl(ClassDecl)
    rn_match sel_name match@(Match (TypePat ty : _) _ _)
	= extendTyVarEnvFVRn gen_tvs 	$
	  rnMatch (FunRhs sel_name) match
	where
	  tvs     = map rdrNameOcc (extractHsTyRdrNames ty)
	  gen_tvs = [tv | tv <- gen_tyvars, nameOccName tv `elem` tvs] 

    rn_match sel_name match = rnMatch (FunRhs sel_name) match
	

-- Can't handle method pattern-bindings which bind multiple methods.
rnMethodBinds cls gen_tyvars mbind@(PatMonoBind other_pat _ locn)
  = addSrcLoc locn (addErr (methodBindErr mbind))	`thenM_`
    returnM (EmptyMonoBinds, emptyFVs) 
\end{code}


%************************************************************************
%*									*
	Strongly connected components

%*									*
%************************************************************************

During analysis a @MonoBinds@ is flattened to a @FlatMonoBinds@.
The @RenamedMonoBinds@ is always an empty bind, a pattern binding or
a function binding, and has itself been dependency-analysed and
renamed.

\begin{code}
type BindWithSigs = (RenamedMonoBinds, [RenamedSig])
			-- Signatures, if any, for this vertex

type FlatMonoBinds = (NameSet,	-- Defs
	 	      NameSet, 	-- Uses
		      BindWithSigs)

rnSCC :: [FlatMonoBinds] -> [SCC BindWithSigs]
rnSCC nodes = stronglyConnComp (mkEdges nodes)

type VertexTag	= Int

mkEdges :: [FlatMonoBinds] -> [(BindWithSigs, VertexTag, [VertexTag])]
mkEdges nodes
  = [ (thing, tag, dest_vertices uses)
    | ((defs, uses, thing), tag) <- tagged_nodes
    ]
  where
    tagged_nodes = nodes `zip` [0::VertexTag ..]

 	 -- An edge (v,v') indicates that v depends on v'
    dest_vertices uses = [ target_vertex
			 | ((defs, _, _), target_vertex) <- tagged_nodes,
			   mentioned_name <- nameSetToList uses,
			   mentioned_name `elemNameSet` defs
			 ]

reconstructCycle :: SCC BindWithSigs -> RenamedHsBinds
reconstructCycle (AcyclicSCC (binds, sigs))
  = MonoBind binds sigs NonRecursive
reconstructCycle (CyclicSCC cycle)
  = MonoBind this_gp_binds this_gp_sigs Recursive
  where
    (binds,sigs)  = unzip cycle
    this_gp_binds = foldr1 AndMonoBinds binds
    this_gp_sigs  = foldr1 (++)	        sigs
\end{code}


%************************************************************************
%*									*
\subsubsection[dep-Sigs]{Signatures (and user-pragmas for values)}
%*									*
%************************************************************************

@renameSigs@ checks for:
\begin{enumerate}
\item more than one sig for one thing;
\item signatures given for things not bound here;
\item with suitably flaggery, that all top-level things have type signatures.
\end{enumerate}
%
At the moment we don't gather free-var info from the types in
signatures.  We'd only need this if we wanted to report unused tyvars.

\begin{code}
checkSigs :: (NameSet -> RenamedSig -> Bool)	-- OK-sig predicbate
	  -> NameSet				-- Binders of this group
	  -> [RenamedSig]
	  -> RnM ()
checkSigs ok_sig bndrs sigs
	-- Check for (a) duplicate signatures
	--	     (b) signatures for things not in this group
	-- Well, I can't see the check for (b)... ToDo!
  = mappM_ unknownSigErr bad_sigs
  where
    bad_sigs = filter (not . ok_sig bndrs) sigs

-- We use lookupSigOccRn in the signatures, which is a little bit unsatisfactory
-- because this won't work for:
--	instance Foo T where
--	  {-# INLINE op #-}
--	  Baz.op = ...
-- We'll just rename the INLINE prag to refer to whatever other 'op'
-- is in scope.  (I'm assuming that Baz.op isn't in scope unqualified.)
-- Doesn't seem worth much trouble to sort this.

renameSigs :: [Sig RdrName] -> RnM [Sig Name]
renameSigs sigs = mappM renameSig (filter (not . isFixitySig) sigs)
	-- Remove fixity sigs which have been dealt with already

renameSig :: Sig RdrName -> RnM (Sig Name)
-- ClassOpSig, FixitSig is renamed elsewhere.
renameSig (Sig v ty src_loc)
  = addSrcLoc src_loc $
    lookupSigOccRn v				`thenM` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty		`thenM` \ new_ty ->
    returnM (Sig new_v new_ty src_loc)

renameSig (SpecInstSig ty src_loc)
  = addSrcLoc src_loc $
    rnHsType (text "A SPECIALISE instance pragma") ty `thenM` \ new_ty ->
    returnM (SpecInstSig new_ty src_loc)

renameSig (SpecSig v ty src_loc)
  = addSrcLoc src_loc $
    lookupSigOccRn v			`thenM` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty	`thenM` \ new_ty ->
    returnM (SpecSig new_v new_ty src_loc)

renameSig (InlineSig b v p src_loc)
  = addSrcLoc src_loc $
    lookupSigOccRn v		`thenM` \ new_v ->
    returnM (InlineSig b new_v p src_loc)
\end{code}


%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dupSigDeclErr sig
  = addSrcLoc loc $
    addErr (sep [ptext SLIT("Duplicate") <+> what_it_is <> colon,
		   ppr sig])
  where
    (what_it_is, loc) = hsSigDoc sig

unknownSigErr sig
  = addSrcLoc loc $
    addErr (sep [ptext SLIT("Misplaced") <+> what_it_is <> colon,
		   ppr sig])
  where
    (what_it_is, loc) = hsSigDoc sig

missingSigWarn var
  = addSrcLoc (nameSrcLoc var) $
    addWarn (sep [ptext SLIT("Definition but no type signature for"), quotes (ppr var)])

methodBindErr mbind
 =  hang (ptext SLIT("Can't handle multiple methods defined by one pattern binding"))
       4 (ppr mbind)
\end{code}
