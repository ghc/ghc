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
	rnTopBinds, rnBinds, rnBindsAndThen,
	rnMethodBinds, renameSigs, checkSigs
   ) where

#include "HsVersions.h"


import HsSyn
import HsBinds		( hsSigDoc, eqHsSig )
import RdrHsSyn
import RnHsSyn
import TcRnMonad
import RnTypes		( rnHsSigType, rnLHsType, rnLPat )
import RnExpr		( rnMatchGroup, rnMatch, rnGRHSs, checkPrecMatch )
import RnEnv		( bindLocatedLocalsRn, lookupLocatedBndrRn, 
			  lookupLocatedInstDeclBndr,
			  lookupLocatedSigOccRn, bindPatSigTyVars, bindPatSigTyVarsFV,
			  bindLocalFixities, bindSigTyVarsFV, 
			  warnUnusedLocalBinds, mapFvRn, extendTyVarEnvFVRn,
			)
import DynFlags	( DynFlag(..) )
import Digraph		( SCC(..), stronglyConnComp )
import Name		( Name, nameOccName, nameSrcLoc )
import NameSet
import PrelNames	( isUnboundName )
import RdrName		( RdrName, rdrNameOcc )
import BasicTypes	( RecFlag(..), TopLevelFlag(..), isTopLevel )
import List		( unzip4 )
import SrcLoc		( mkSrcSpan, Located(..), unLoc )
import Bag
import Outputable
import Monad		( foldM )
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
%* analysing polymorphic bindings (HsBindGroup, HsBind)
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
rnTopBinds :: LHsBinds RdrName
	   -> [LSig RdrName]
	   -> RnM ([HsBindGroup Name], DefUses)

-- The binders of the binding are in scope already;
-- the top level scope resolution does that

rnTopBinds mbinds sigs
 =  do	{ is_boot <- tcIsHsBoot
	; if is_boot then
		rnHsBoot mbinds sigs
	  else  bindPatSigTyVars (collectSigTysFromHsBinds (bagToList mbinds)) $ \ _ -> 
			-- Hmm; by analogy with Ids, this doesn't look right
			-- Top-level bound type vars should really scope over 
			-- everything, but we only scope them over the other bindings
		rnBinds TopLevel mbinds sigs }

rnHsBoot :: LHsBinds RdrName
	   -> [LSig RdrName]
	   -> RnM ([HsBindGroup Name], DefUses)
-- A hs-boot file has no bindings. 
-- Return a single HsBindGroup with empty binds and renamed signatures
rnHsBoot mbinds sigs
  = do	{ checkErr (isEmptyLHsBinds mbinds) (bindsInHsBootFile mbinds)
	; sigs' <- renameSigs sigs
	; return ([HsBindGroup emptyLHsBinds sigs' NonRecursive], 
		  usesOnly (hsSigsFVs sigs')) }
\end{code}


%************************************************************************
%*									*
%* 		Nested binds
%*									*
%************************************************************************

\begin{code}
rnBindsAndThen :: Bag (LHsBind RdrName)
	       -> [LSig RdrName]
	       -> ([HsBindGroup Name] -> RnM (result, FreeVars))
	       -> RnM (result, FreeVars)

rnBindsAndThen mbinds sigs thing_inside
  =	-- Extract all the binders in this group, and extend the
	-- current scope, inventing new names for the new binders
	-- This also checks that the names form a set
    bindLocatedLocalsRn doc mbinders_w_srclocs			$ \ _ ->
    bindPatSigTyVarsFV (collectSigTysFromHsBinds (bagToList mbinds))	$ 

	-- Then install local fixity declarations
	-- Notice that they scope over thing_inside too
    bindLocalFixities [sig | L _ (FixSig sig) <- sigs ] 	$

	-- Do the business
    rnBinds NotTopLevel mbinds sigs	`thenM` \ (binds, bind_dus) ->

	-- Now do the "thing inside"
    thing_inside binds 			`thenM` \ (result,result_fvs) ->

	-- Final error checking
    let
	all_uses     = duUses bind_dus `plusFV` result_fvs
	bndrs        = duDefs bind_dus
	unused_bndrs = nameSetToList (bndrs `minusNameSet` all_uses)
    in
    warnUnusedLocalBinds unused_bndrs	`thenM_`

    returnM (result, all_uses `minusNameSet` bndrs)
	-- duUses: It's important to return all the uses, not the 'real uses' used for
	-- warning about unused bindings.  Otherwise consider:
	--	x = 3
	--	y = let p = x in 'x'	-- NB: p not used
	-- If we don't "see" the dependency of 'y' on 'x', we may put the
	-- bindings in the wrong order, and the type checker will complain
	-- that x isn't in scope
  where
    mbinders_w_srclocs = collectHsBindLocatedBinders mbinds
    doc = text "In the binding group for:"
	  <+> pprWithCommas ppr (map unLoc mbinders_w_srclocs)
\end{code}


%************************************************************************
%*									*
\subsubsection{rnBinds -- the main work is done here}
%*									*
%************************************************************************

@rnMonoBinds@ is used by {\em both} top-level and nested bindings.
It assumes that all variables bound in this group are already in scope.
This is done {\em either} by pass 3 (for the top-level bindings),
{\em or} by @rnMonoBinds@ (for the nested ones).

\begin{code}
rnBinds :: TopLevelFlag
	-> LHsBinds RdrName
	-> [LSig RdrName]
	-> RnM ([HsBindGroup Name], DefUses)

-- Assumes the binders of the binding are in scope already

rnBinds top_lvl mbinds sigs
 =  renameSigs sigs			`thenM` \ siglist ->

  	 -- Rename the bindings, returning a [HsBindVertex]
	 -- which is a list of indivisible vertices so far as
	 -- the strongly-connected-components (SCC) analysis is concerned
    mkBindVertices siglist mbinds	`thenM` \ mbinds_info ->

	 -- Do the SCC analysis
    let 
	scc_result  = rnSCC mbinds_info
	(groups, bind_dus_s) = unzip (map reconstructCycle scc_result)
	bind_dus    = mkDUs bind_dus_s	
	binders     = duDefs bind_dus
    in
	-- Check for duplicate or mis-placed signatures
    checkSigs (okBindSig binders) siglist	`thenM_`

	-- Warn about missing signatures, 
	-- but only at top level, and not in interface mode
	-- (The latter is important when renaming bindings from 'deriving' clauses.)
    doptM Opt_WarnMissingSigs 		`thenM` \ warn_missing_sigs ->
    (if isTopLevel top_lvl && 
	 warn_missing_sigs
     then let
	    type_sig_vars   = [ unLoc n | L _ (Sig n _) <- siglist]
	    un_sigd_binders = filter (not . (`elem` type_sig_vars)) 
				     (nameSetToList binders)
	  in
          mappM_ missingSigWarn un_sigd_binders
     else
	returnM ()  
    )						`thenM_`

    returnM (groups, bind_dus `plusDU` usesOnly (hsSigsFVs siglist))
\end{code}

@mkBindVertices@ is ever-so-slightly magical in that it sticks
unique ``vertex tags'' on its output; minor plumbing required.

\begin{code}
mkBindVertices :: [LSig Name]		-- Signatures
	       -> LHsBinds RdrName
	       -> RnM [BindVertex]
mkBindVertices sigs = mapM (mkBindVertex sigs) . bagToList

mkBindVertex :: [LSig Name] -> LHsBind RdrName -> RnM BindVertex
mkBindVertex sigs (L loc (PatBind pat grhss ty))
  = setSrcSpan loc $
    rnLPat pat				`thenM` \ (pat', pat_fvs) ->

	 -- Find which things are bound in this group
    let
	names_bound_here = mkNameSet (collectPatBinders pat')
    in
    sigsForMe names_bound_here sigs	`thenM` \ sigs_for_me ->
    bindSigTyVarsFV sigs_for_me (
        rnGRHSs PatBindRhs grhss
    )					`thenM` \ (grhss', fvs) ->
    returnM 
	(names_bound_here, fvs `plusFV` pat_fvs,
	  L loc (PatBind pat' grhss' ty), sigs_for_me
	)

mkBindVertex sigs (L loc (FunBind name inf matches))
  = setSrcSpan loc $ 
    lookupLocatedBndrRn name				`thenM` \ new_name ->
    let
	plain_name = unLoc new_name
	names_bound_here = unitNameSet plain_name
    in
    sigsForMe names_bound_here sigs			`thenM` \ sigs_for_me ->
    bindSigTyVarsFV sigs_for_me (
    	rnMatchGroup (FunRhs plain_name) matches
    )							`thenM` \ (new_matches, fvs) ->
    checkPrecMatch inf plain_name new_matches		`thenM_`
    returnM
      (unitNameSet plain_name, fvs,
	L loc (FunBind new_name inf new_matches), sigs_for_me
      )

sigsForMe names_bound_here sigs
  = foldlM check [] (filter (sigForThisGroup names_bound_here) sigs)
  where
	-- sigForThisGroup only returns signatures for 
	-- which sigName returns a Just
    eq sig1 sig2 = eqHsSig (unLoc sig1) (unLoc sig2)

    check sigs sig = case filter (eq sig) sigs of
			[]    -> returnM (sig:sigs)
			other -> dupSigDeclErr sig other	`thenM_`
				 returnM sigs
\end{code}


@rnMethodBinds@ is used for the method bindings of a class and an instance
declaration.   Like @rnBinds@ but without dependency analysis.

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
	      -> LHsBinds RdrName
	      -> RnM (LHsBinds Name, FreeVars)

rnMethodBinds cls gen_tyvars binds
  = foldM do_one (emptyBag,emptyFVs) (bagToList binds)
  where do_one (binds,fvs) bind = do
	   (bind', fvs_bind) <- rnMethodBind cls gen_tyvars bind
	   return (bind' `unionBags` binds, fvs_bind `plusFV` fvs)

rnMethodBind cls gen_tyvars (L loc (FunBind name inf (MatchGroup matches _)))
  =  setSrcSpan loc $ 
     lookupLocatedInstDeclBndr cls name			`thenM` \ sel_name -> 
     let plain_name = unLoc sel_name in
	-- We use the selector name as the binder

    mapFvRn (rn_match plain_name) matches		`thenM` \ (new_matches, fvs) ->
    let 
	new_group = MatchGroup new_matches placeHolderType
    in
    checkPrecMatch inf plain_name new_group		`thenM_`
    returnM (unitBag (L loc (FunBind sel_name inf new_group)), fvs `addOneFV` plain_name)
  where
	-- Truly gruesome; bring into scope the correct members of the generic 
	-- type variables.  See comments in RnSource.rnSourceDecl(ClassDecl)
    rn_match sel_name match@(L _ (Match (L _ (TypePat ty) : _) _ _))
	= extendTyVarEnvFVRn gen_tvs 	$
	  rnMatch (FunRhs sel_name) match
	where
	  tvs     = map (rdrNameOcc.unLoc) (extractHsTyRdrTyVars ty)
	  gen_tvs = [tv | tv <- gen_tyvars, nameOccName tv `elem` tvs] 

    rn_match sel_name match = rnMatch (FunRhs sel_name) match


-- Can't handle method pattern-bindings which bind multiple methods.
rnMethodBind cls gen_tyvars mbind@(L loc (PatBind other_pat _ _))
  = addLocErr mbind methodBindErr	`thenM_`
    returnM (emptyBag, emptyFVs) 
\end{code}


%************************************************************************
%*									*
	Strongly connected components
%*									*
%************************************************************************

\begin{code}
type BindVertex = (Defs, Uses, LHsBind Name, [LSig Name])
			-- Signatures, if any, for this vertex

rnSCC :: [BindVertex] -> [SCC BindVertex]
rnSCC nodes = stronglyConnComp (mkEdges nodes)

type VertexTag	= Int

mkEdges :: [BindVertex] -> [(BindVertex, VertexTag, [VertexTag])]
	-- We keep the uses with the binding, 
	-- so we can track unused bindings better
mkEdges nodes
  = [ (thing, tag, dest_vertices uses)
    | (thing@(_, uses, _, _), tag) <- tagged_nodes
    ]
  where
    tagged_nodes = nodes `zip` [0::VertexTag ..]

 	 -- An edge (v,v') indicates that v depends on v'
    dest_vertices uses = [ target_vertex
			 | ((defs, _, _, _), target_vertex) <- tagged_nodes,
			   defs `intersectsNameSet` uses
			 ]

reconstructCycle :: SCC BindVertex -> (HsBindGroup Name, (Defs,Uses))
reconstructCycle (AcyclicSCC (defs, uses, bind, sigs))
  = (HsBindGroup (unitBag bind) sigs NonRecursive, (defs, uses))
reconstructCycle (CyclicSCC cycle)
  = (HsBindGroup this_gp_binds this_gp_sigs Recursive, 
     (unionManyNameSets defs_s, unionManyNameSets uses_s))
  where
    (defs_s, uses_s, binds_s, sigs_s) = unzip4 cycle
    this_gp_binds = listToBag binds_s
    this_gp_sigs  = foldr1 (++) sigs_s
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
checkSigs :: (LSig Name -> Bool)	-- OK-sig predicbate
	  -> [LSig Name]
	  -> RnM ()
checkSigs ok_sig sigs
	-- Check for (a) duplicate signatures
	--	     (b) signatures for things not in this group
	-- Well, I can't see the check for (a)... ToDo!
  = mappM_ unknownSigErr (filter bad sigs)
  where
    bad sig = not (ok_sig sig) && 
	      case sigName sig of
		Just n | isUnboundName n -> False
				-- Don't complain about an unbound name again
		other			 -> True

-- We use lookupSigOccRn in the signatures, which is a little bit unsatisfactory
-- because this won't work for:
--	instance Foo T where
--	  {-# INLINE op #-}
--	  Baz.op = ...
-- We'll just rename the INLINE prag to refer to whatever other 'op'
-- is in scope.  (I'm assuming that Baz.op isn't in scope unqualified.)
-- Doesn't seem worth much trouble to sort this.

renameSigs :: [LSig RdrName] -> RnM [LSig Name]
renameSigs sigs = mappM (wrapLocM renameSig) (filter (not . isFixityLSig) sigs)
	-- Remove fixity sigs which have been dealt with already

renameSig :: Sig RdrName -> RnM (Sig Name)
-- FixitSig is renamed elsewhere.
renameSig (Sig v ty)
  = lookupLocatedSigOccRn v			`thenM` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty		`thenM` \ new_ty ->
    returnM (Sig new_v new_ty)

renameSig (SpecInstSig ty)
  = rnLHsType (text "A SPECIALISE instance pragma") ty `thenM` \ new_ty ->
    returnM (SpecInstSig new_ty)

renameSig (SpecSig v ty)
  = lookupLocatedSigOccRn v		`thenM` \ new_v ->
    rnHsSigType (quotes (ppr v)) ty	`thenM` \ new_ty ->
    returnM (SpecSig new_v new_ty)

renameSig (InlineSig b v p)
  = lookupLocatedSigOccRn v		`thenM` \ new_v ->
    returnM (InlineSig b new_v p)
\end{code}


%************************************************************************
%*									*
\subsection{Error messages}
%*									*
%************************************************************************

\begin{code}
dupSigDeclErr (L loc sig) sigs
  = addErrAt loc $
	vcat [ptext SLIT("Duplicate") <+> what_it_is <> colon,
	      nest 2 (vcat (map ppr_sig (L loc sig:sigs)))]
  where
    what_it_is = hsSigDoc sig
    ppr_sig (L loc sig) = ppr loc <> colon <+> ppr sig

unknownSigErr (L loc sig)
  = addErrAt loc $
	sep [ptext SLIT("Misplaced") <+> what_it_is <> colon, ppr sig]
  where
    what_it_is = hsSigDoc sig

missingSigWarn var
  = addWarnAt (mkSrcSpan loc loc) $
      sep [ptext SLIT("Definition but no type signature for"), quotes (ppr var)]
  where 
    loc = nameSrcLoc var  -- TODO: make a proper span

methodBindErr mbind
 =  hang (ptext SLIT("Pattern bindings (except simple variables) not allowed in instance declarations"))
       2 (ppr mbind)

bindsInHsBootFile mbinds
  = hang (ptext SLIT("Bindings in hs-boot files are not allowed"))
       2 (ppr mbinds)
\end{code}
