%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[GenSpecEtc]{Code for GEN, SPEC, PRED, and REL}

\begin{code}
#include "HsVersions.h"

module GenSpecEtc (
	TcSigInfo(..), 
	genBinds, 
	checkSigTyVars, checkSigTyVarsGivenGlobals
    ) where

IMP_Ubiq()

import TcMonad		hiding ( rnMtoTcM )
import Inst		( Inst, InstOrigin(..), LIE(..), plusLIE, 
			  newDicts, tyVarsOfInst, instToId )
import TcEnv		( tcGetGlobalTyVars )
import TcSimplify	( tcSimplify, tcSimplifyAndCheck, tcSimplifyWithExtraGlobals )
import TcType		( TcType(..), TcThetaType(..), TcTauType(..), 
			  TcTyVarSet(..), TcTyVar(..),
			  newTyVarTy, zonkTcType, zonkTcTyVar, zonkTcTyVars 
			)
import Unify		( unifyTauTy )

import HsSyn		( HsBinds(..), Bind(..), MonoBinds(..), HsExpr, OutPat(..), 
			  Sig, HsLit, ArithSeqInfo, InPat, GRHSsAndBinds, Match, Fake
			)
import TcHsSyn		( TcIdOcc(..), TcIdBndr(..), TcHsBinds(..), TcBind(..), TcExpr(..), tcIdType )

import Bag		( Bag, foldBag, bagToList, listToBag, isEmptyBag )
import Class		( GenClass )
import Id		( GenId, Id(..), mkUserId, idType )
import Kind		( isUnboxedKind, isTypeKind, mkBoxedTypeKind )
import ListSetOps	( minusList, unionLists, intersectLists )
import Maybes		( Maybe(..), allMaybes )
import Name		( Name{--O only-} )
import Outputable	( interppSP, interpp'SP )
import Pretty
import PprType		( GenClass, GenType, GenTyVar )
import Type		( mkTyVarTy, splitSigmaTy, mkForAllTys, mkFunTys,
			  getTyVar, getTyVar_maybe, tyVarsOfTypes, eqSimpleTheta )
import TyVar		( GenTyVar, TyVar(..), tyVarKind, minusTyVarSet, emptyTyVarSet,
			  elementOfTyVarSet, unionTyVarSets, tyVarSetToList )
import Usage		( UVar(..) )
import Unique		( Unique )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[Gen-SignatureInfo]{The @TcSigInfo@ type}
%*									*
%************************************************************************

A type signature (or user-pragma) is typechecked to produce a
@TcSigInfo@.  It contains @TcTypes@ because they are unified with
the variable's type, and after that checked to see whether they've
been instantiated.

\begin{code}
data TcSigInfo s
  = TySigInfo	    (TcIdBndr s)	-- for this value...
		    [TcTyVar s] (TcThetaType s) (TcTauType s)
		    SrcLoc
\end{code}


%************************************************************************
%*									*
\subsection[Gen-GEN]{Generalising bindings}
%*									*
%************************************************************************

\begin{code}
genBinds :: [Name]				-- Binders
	 -> [TcIdBndr s]			-- Monomorphic binders
	 -> TcBind s				-- Type-checked monobind
	 -> LIE s 				-- LIE from typecheck of binds
	 -> [TcSigInfo s]			-- Signatures, if any
	 -> (Name -> PragmaInfo)		-- Gives pragma info for binder
	 -> TcM s (TcHsBinds s, LIE s, [TcIdBndr s])
\end{code}

In the call $(@genBinds@~env~bind~lie~lve)$, $(bind,lie,lve)$
is the result of typechecking a @Bind@.  @genBinds@ implements the BIND-GEN
and BIND-PRED rules.
$lie$ and $lve$ may or may not be
fixed points of the current substitution.

It returns
\begin{itemize}
\item
An @AbsBind@ which wraps up $bind$ in a suitable abstraction.
\item
an LIE, which is the part of the input LIE which isn't discharged by
the AbsBind.  This means the parts which predicate type variables
free in $env$.
\item
An LVE whose domain is identical to that passed in.
Its range is a new set of locals to that passed in,
because they have been gen'd.
\end{itemize}

@genBinds@ takes the
following steps:
\begin{itemize}
\item
find $constrained$, the free variables of $env$.
First we must apply the current substitution to the environment, so that the
correct set of constrained type vars are extracted!
\item
find $free$, the free variables of $lve$ which are not in $constrained$.
We need to apply the current subsitution to $lve$ first, of course.
\item
minimise $lie$ to give $lie'$; all the constraints in $lie'$ are on
single type variables.
\item
split $lie'$ into three: those predicating type variables in $constrained$,
those on type variables in $free$, and the rest.
\item
complain about ``the rest'' part of $lie'$.  These type variables are
ambiguous.
\item
generate new locals for each member of the domain of $lve$, with appropriately
gen'd types.
\item
generate a suitable AbsBinds to enclose the bindings.
\end{itemize}

\begin{code}
genBinds binder_names mono_ids bind lie sig_infos prag_info_fn
  =	-- CHECK THAT THE SIGNATURES MATCH
	-- Doesn't affect substitution
    mapTc checkSigMatch sig_infos	`thenTc_`

	-- CHECK THAT ALL THE SIGNATURE CONTEXTS ARE IDENTICAL
	-- The type signatures on a mutually-recursive group of definitions
	-- must all have the same context (or none).
	-- We have to zonk them first to make their type variables line up
    mapNF_Tc get_zonked_theta sig_infos		`thenNF_Tc` \ thetas ->
    checkTc (null thetas || all (eqSimpleTheta (head thetas)) (tail thetas)) 
	    (sigContextsErr sig_infos)		`thenTc_`

	-- COMPUTE VARIABLES OVER WHICH TO QUANTIFY, namely tyvars_to_gen
    mapNF_Tc (zonkTcType . idType) mono_ids	`thenNF_Tc` \ mono_id_types ->
    tcGetGlobalTyVars 				`thenNF_Tc` \ free_tyvars ->
    let
	mentioned_tyvars = tyVarsOfTypes mono_id_types
	tyvars_to_gen    = mentioned_tyvars `minusTyVarSet` free_tyvars
    in

	-- DEAL WITH OVERLOADING
    resolveOverloading tyvars_to_gen lie bind sig_infos
		 `thenTc` \ (lie', reduced_tyvars_to_gen, dict_binds, dicts_bound) ->

	-- Check for generaliseation over unboxed types, and
	-- default any TypeKind TyVars to BoxedTypeKind
    let
	tyvars = tyVarSetToList reduced_tyvars_to_gen	-- Commit to a particular order

        unboxed_kind_tyvars    = filter (isUnboxedKind . tyVarKind) tyvars
	unresolved_kind_tyvars = filter (isTypeKind    . tyVarKind) tyvars

	box_it tyvar = newTyVarTy mkBoxedTypeKind	`thenNF_Tc` \ boxed_ty ->
		       unifyTauTy (mkTyVarTy tyvar) boxed_ty

    in
    ASSERT( null unboxed_kind_tyvars )	-- The instCantBeGeneralised stuff in tcSimplify
					-- should have dealt with unboxed type variables;
					-- and it's better done there because we have more
					-- precise origin information

    mapTc box_it unresolved_kind_tyvars			`thenTc_`

	 -- BUILD THE NEW LOCALS
    let
	dict_tys    = map tcIdType dicts_bound
	poly_tys    = map (mkForAllTys tyvars . mkFunTys dict_tys) mono_id_types
	poly_ids    = zipWithEqual "genspecetc" mk_poly binder_names poly_tys
	mk_poly name ty = mkUserId name ty (prag_info_fn name)
    in
	 -- BUILD RESULTS
    returnTc (
	 AbsBinds tyvars
		  dicts_bound
		  (zipEqual "genBinds" (map TcId mono_ids) (map TcId poly_ids))
		  dict_binds
		  bind,
	 lie',
	 poly_ids
    )

get_zonked_theta (TySigInfo _ _ theta _ _)
  = mapNF_Tc (\ (c,t) -> zonkTcType t `thenNF_Tc` \ t' -> returnNF_Tc (c,t')) theta
\end{code}


\begin{code}
resolveOverloading
	:: TcTyVarSet s		-- Tyvars over which we are going to generalise
	-> LIE s		-- The LIE to deal with
	-> TcBind s		-- The binding group
	-> [TcSigInfo s]	-- And its real type-signature information
	-> TcM s (LIE s,			-- LIE to pass up the way; a fixed point of
						-- the current substitution
	    	  TcTyVarSet s,			-- Revised tyvars to generalise
	    	  [(TcIdOcc s, TcExpr s)],	-- Dict bindings
	    	  [TcIdOcc s])			-- List of dicts to bind here

resolveOverloading tyvars_to_gen dicts bind ty_sigs
  | not (isUnRestrictedGroup tysig_vars bind)
  = 	-- Restricted group, so bind no dictionaries, and
	-- remove from tyvars_to_gen any constrained type variables

	-- *Don't* simplify dicts at this point, because we aren't going
	-- to generalise over these dicts.  By the time we do simplify them
	-- we may well know more.  For example (this actually came up)
	--	f :: Array Int Int
	--	f x = array ... xs where xs = [1,2,3,4,5]
	-- We don't want to generate lots of (fromInt Int 1), (fromInt Int 2)
	-- stuff.  If we simplify only at the f-binding (not the xs-binding)
	-- we'll know that the literals are all Ints, and we can just produce
	-- Int literals!

	-- Find all the type variables involved in overloading, the "constrained_tyvars"
	-- These are the ones we *aren't* going to generalise.
	-- We must be careful about doing this:
	--  (a) If we fail to generalise a tyvar which is not actually
	--	constrained, then it will never, ever get bound, and lands
	--	up printed out in interface files!  Notorious example:
	--		instance Eq a => Eq (Foo a b) where ..
	--	Here, b is not constrained, even though it looks as if it is.
	-- 	Another, more common, example is when there's a Method inst in
	--	the LIE, whose type might very well involve non-overloaded
	--	type variables.
	--  (b) On the other hand, we mustn't generalise tyvars which are constrained,
	--	because we are going to pass on out the unmodified LIE, with those
	--	tyvars in it.  They won't be in scope if we've generalised them.
	--
	-- So we are careful, and do a complete simplification just to find the
	-- constrained tyvars. We don't use any of the results, except to
	-- find which tyvars are constrained.

	tcSimplify tyvars_to_gen dicts	    `thenTc` \ (_, _, dicts_sig) ->
	let
	  -- ASSERT: dicts_sig is already zonked!
	  constrained_tyvars    = foldBag unionTyVarSets tyVarsOfInst emptyTyVarSet dicts_sig
	  reduced_tyvars_to_gen = tyvars_to_gen `minusTyVarSet` constrained_tyvars
	in

	-- Do it again, but with increased free_tyvars/reduced_tyvars_to_gen:
	-- We still need to do this simplification, because some dictionaries 
	-- may gratuitouslyconstrain some tyvars over which we *are* going 
	-- to generalise. 
	-- For example d::Eq (Foo a b), where Foo is instanced as above.
	tcSimplifyWithExtraGlobals constrained_tyvars reduced_tyvars_to_gen dicts
				    `thenTc` \ (dicts_free, dicts_binds, dicts_sig2) ->
	ASSERT(isEmptyBag dicts_sig2)

	returnTc (dicts_free,			-- All these are left unbound
		  reduced_tyvars_to_gen,
		  dicts_binds, 			-- Local dict binds
		  [])				-- No lambda-bound dicts

		-- The returned LIE should be a fixed point of the substitution

  | otherwise	-- An unrestricted group
  = case ty_sigs of
	[] ->	-- NO TYPE SIGNATURES

	    tcSimplify tyvars_to_gen dicts  `thenTc` \ (dicts_free, dict_binds, dicts_sig) ->
	    returnTc (dicts_free, tyvars_to_gen, dict_binds, 
		      map instToId (bagToList dicts_sig))

	(TySigInfo _ _ theta _ _ : other) -> -- TYPE SIGNATURES PRESENT!

	    tcAddErrCtxt (sigsCtxt tysig_vars) $

	    newDicts SignatureOrigin theta	`thenNF_Tc` \ (dicts_sig, dict_ids) ->

		    -- Check that the needed dicts can be expressed in
		    -- terms of the signature ones
	    tcSimplifyAndCheck
		tyvars_to_gen 	-- Type vars over which we will quantify
		dicts_sig	-- Available dicts
		dicts		-- Want bindings for these dicts

				    `thenTc` \ (dicts_free, dict_binds) ->

	    returnTc (dicts_free, tyvars_to_gen, dict_binds, dict_ids)
  where
    tysig_vars   = [sig_var | (TySigInfo sig_var _ _ _ _) <- ty_sigs]
\end{code}

@checkSigMatch@ does the next step in checking signature matching.
The tau-type part has already been unified.  What we do here is to
check that this unification has not over-constrained the (polymorphic)
type variables of the original signature type.

The error message here is somewhat unsatisfactory, but it'll do for
now (ToDo).

\begin{code}
checkSigMatch :: TcSigInfo s -> TcM s ()

checkSigMatch (TySigInfo id sig_tyvars _ tau_ty src_loc)
  = tcAddSrcLoc src_loc	$
    tcAddErrCtxt (sigCtxt id) $
    checkSigTyVars sig_tyvars tau_ty
\end{code}


%************************************************************************
%*									*
\subsection[GenEtc-monomorphism]{The monomorphism restriction}
%*									*
%************************************************************************

Not exported:

\begin{code}
isUnRestrictedGroup :: [TcIdBndr s]		-- Signatures given for these
		    -> TcBind s
		    -> Bool

isUnRestrictedGroup sigs EmptyBind              = True
isUnRestrictedGroup sigs (NonRecBind monobinds) = isUnResMono sigs monobinds
isUnRestrictedGroup sigs (RecBind monobinds)    = isUnResMono sigs monobinds

is_elem v vs = isIn "isUnResMono" v vs

isUnResMono sigs (PatMonoBind (VarPat (TcId v)) _ _)	= v `is_elem` sigs
isUnResMono sigs (PatMonoBind other      _ _)		= False
isUnResMono sigs (VarMonoBind (TcId v) _)		= v `is_elem` sigs
isUnResMono sigs (FunMonoBind _ _ _ _)			= True
isUnResMono sigs (AndMonoBinds mb1 mb2)			= isUnResMono sigs mb1 &&
							  isUnResMono sigs mb2
isUnResMono sigs EmptyMonoBinds				= True
\end{code}


%************************************************************************
%*									*
\subsection[GenEtc-sig]{Matching a type signature}
%*									*
%************************************************************************

@checkSigTyVars@ is used after the type in a type signature has been unified with
the actual type found.  It then checks that the type variables of the type signature
are
	(a) still all type variables
		eg matching signature [a] against inferred type [(p,q)]
		[then a will be unified to a non-type variable]

	(b) still all distinct
		eg matching signature [(a,b)] against inferred type [(p,p)]
		[then a and b will be unified together]

BUT ACTUALLY THESE FIRST TWO ARE FORCED BY USING DontBind TYVARS

	(c) not mentioned in the environment
		eg the signature for f in this:

			g x = ... where
					f :: a->[a]
					f y = [x,y]

		Here, f is forced to be monorphic by the free occurence of x.

Before doing this, the substitution is applied to the signature type variable.

\begin{code}
checkSigTyVars :: [TcTyVar s]		-- The original signature type variables
	       -> TcType s		-- signature type (for err msg)
	       -> TcM s ()

checkSigTyVars sig_tyvars sig_tau
  = checkSigTyVarsGivenGlobals emptyTyVarSet sig_tyvars sig_tau

checkSigTyVarsGivenGlobals
	 :: TcTyVarSet s	-- Consider these tyvars as global in addition to envt ones
	 -> [TcTyVar s]		-- The original signature type variables
	 -> TcType s		-- signature type (for err msg)
	 -> TcM s ()

checkSigTyVarsGivenGlobals extra_globals sig_tyvars sig_tau
  = zonkTcTyVars extra_globals		`thenNF_Tc` \ extra_tyvars' ->
    tcGetGlobalTyVars			`thenNF_Tc` \ env_tyvars ->
    let
	globals     = env_tyvars `unionTyVarSets` extra_tyvars'
	mono_tyvars = filter (`elementOfTyVarSet` globals) sig_tyvars
    in
	-- TEMPORARY FIX
	-- Until the final Bind-handling stuff is in, several type signatures in the same
	-- bindings group can cause the signature type variable from the different
	-- signatures to be unified.  So we still need to zonk and check point (b).
	-- Remove when activating the new binding code
    mapNF_Tc zonkTcTyVar sig_tyvars	`thenNF_Tc` \ sig_tys ->
    checkTcM (hasNoDups (map (getTyVar "checkSigTyVars") sig_tys))
	     (zonkTcType sig_tau 	`thenNF_Tc` \ sig_tau' ->
	      failTc (badMatchErr sig_tau sig_tau')
	     )				`thenTc_`


	-- Check point (c)
	-- We want to report errors in terms of the original signature tyvars,
	-- ie sig_tyvars, NOT sig_tyvars'.  sig_tys and sig_tyvars' correspond
	-- 1-1 with sig_tyvars, so we can just map back.
    checkTc (null mono_tyvars)
	    (notAsPolyAsSigErr sig_tau mono_tyvars)
\end{code}




Contexts and errors
~~~~~~~~~~~~~~~~~~~
\begin{code}
notAsPolyAsSigErr sig_tau mono_tyvars sty
  = ppHang (ppStr "A type signature is more polymorphic than the inferred type")
	4  (ppAboves [ppStr "Some type variables in the inferred type can't be forall'd, namely:",
		      interpp'SP sty mono_tyvars,
		      ppStr "Possible cause: the RHS mentions something subject to the monomorphism restriction"
		     ])
\end{code}


\begin{code}
badMatchErr sig_ty inferred_ty sty
  = ppHang (ppStr "Type signature doesn't match inferred type") 
	 4 (ppAboves [ppHang (ppStr "Signature:") 4 (ppr sty sig_ty),
		      ppHang (ppStr "Inferred :") 4 (ppr sty inferred_ty)
	   ])

sigCtxt id sty 
  = ppSep [ppStr "When checking signature for", ppr sty id]
sigsCtxt ids sty 
  = ppSep [ppStr "When checking signature(s) for:", interpp'SP sty ids]
\end{code}


\begin{code}
sigContextsErr ty_sigs sty
  = ppHang (ppStr "A group of type signatures have mismatched contexts")
	 4 (ppAboves (map ppr_sig_info ty_sigs))
  where
    ppr_sig_info (TySigInfo val tyvars theta tau_ty _)
      = ppHang (ppBeside (ppr sty val) (ppStr " :: "))
    	     4 (if null theta
		then ppNil
		else ppBesides [ppStr "(", 
			        ppIntersperse (ppStr ", ") (map (ppr_inst sty) theta), 
				ppStr ") => ..."])
    ppr_inst sty (clas, ty) = ppCat [ppr sty clas, ppr sty ty]
\end{code}
