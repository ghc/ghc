%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcBinds]{TcBinds}

\begin{code}
#include "HsVersions.h"

module TcBinds ( tcBindsAndThen, tcPragmaSigs, checkSigTyVars ) where

IMP_Ubiq()

import HsSyn		( HsBinds(..), Bind(..), Sig(..), MonoBinds(..), 
			  Match, HsType, InPat(..), OutPat(..), HsExpr(..),
			  GRHSsAndBinds, ArithSeqInfo, HsLit, Fake, Stmt, DoOrListComp, Fixity, 
			  collectBinders )
import RnHsSyn		( SYN_IE(RenamedHsBinds), SYN_IE(RenamedBind), RenamedSig(..), 
			  SYN_IE(RenamedMonoBinds)
			)
import TcHsSyn		( SYN_IE(TcHsBinds), SYN_IE(TcBind), SYN_IE(TcMonoBinds),
			  TcIdOcc(..), SYN_IE(TcIdBndr), SYN_IE(TcExpr), 
			  tcIdType
			)

import TcMonad
import Inst		( Inst, SYN_IE(LIE), emptyLIE, plusLIE, InstOrigin(..),
			  newDicts, tyVarsOfInst, instToId
			)
import TcEnv		( tcExtendLocalValEnv, tcLookupLocalValueOK, newMonoIds,
			  tcGetGlobalTyVars, tcExtendGlobalTyVars
			)
import SpecEnv		( SpecEnv )
IMPORT_DELOOPER(TcLoop)		( tcGRHSsAndBinds )
import TcMatches	( tcMatchesFun )
import TcSimplify	( tcSimplify, tcSimplifyAndCheck )
import TcMonoType	( tcHsType )
import TcPat		( tcPat )
import TcSimplify	( bindInstsOfLocalFuns )
import TcType		( SYN_IE(TcType), SYN_IE(TcThetaType), SYN_IE(TcTauType), 
			  SYN_IE(TcTyVarSet), SYN_IE(TcTyVar),
			  newTyVarTy, zonkTcType, zonkTcTyVar, zonkTcTyVars,
			  newTcTyVar, tcInstSigType, newTyVarTys
			)
import Unify		( unifyTauTy )

import Kind		( isUnboxedTypeKind, mkTypeKind, isTypeKind, mkBoxedTypeKind )
import Id		( GenId, idType, mkUserLocal, mkUserId )
import IdInfo		( noIdInfo )
import Maybes		( assocMaybe, catMaybes )
import Name		( pprNonSym, getOccName, getSrcLoc, Name )
import PragmaInfo	( PragmaInfo(..) )
import Pretty
import Type		( mkTyVarTy, mkTyVarTys, isTyVarTy, tyVarsOfTypes, eqSimpleTheta, 
			  mkSigmaTy, splitSigmaTy, mkForAllTys, mkFunTys, getTyVar, 
			  splitRhoTy, mkForAllTy, splitForAllTy )
import TyVar		( GenTyVar, SYN_IE(TyVar), tyVarKind, minusTyVarSet, emptyTyVarSet,
			  elementOfTyVarSet, unionTyVarSets, tyVarSetToList )
import Bag		( bagToList, foldrBag, isEmptyBag )
import Util		( isIn, zipEqual, zipWithEqual, zipWith3Equal, hasNoDups, assoc,
			  assertPanic, panic )
import PprType		( GenClass, GenType, GenTyVar )
import Unique		( Unique )
import Outputable	( interppSP, interpp'SP )
\end{code}


%************************************************************************
%*									*
\subsection{Type-checking bindings}
%*									*
%************************************************************************

@tcBindsAndThen@ typechecks a @HsBinds@.  The "and then" part is because
it needs to know something about the {\em usage} of the things bound,
so that it can create specialisations of them.  So @tcBindsAndThen@
takes a function which, given an extended environment, E, typechecks
the scope of the bindings returning a typechecked thing and (most
important) an LIE.  It is this LIE which is then used as the basis for
specialising the things bound.

@tcBindsAndThen@ also takes a "combiner" which glues together the
bindings and the "thing" to make a new "thing".

The real work is done by @tcBindWithSigsAndThen@.

Recursive and non-recursive binds are handled in essentially the same
way: because of uniques there are no scoping issues left.  The only
difference is that non-recursive bindings can bind primitive values.

Even for non-recursive binding groups we add typings for each binder
to the LVE for the following reason.  When each individual binding is
checked the type of its LHS is unified with that of its RHS; and
type-checking the LHS of course requires that the binder is in scope.

At the top-level the LIE is sure to contain nothing but constant
dictionaries, which we resolve at the module level.

\begin{code}
tcBindsAndThen
	:: (TcHsBinds s -> thing -> thing)		-- Combinator
	-> RenamedHsBinds
	-> TcM s (thing, LIE s, thing_ty)
	-> TcM s (thing, LIE s, thing_ty)

tcBindsAndThen combiner EmptyBinds do_next
  = do_next 	`thenTc` \ (thing, lie, thing_ty) ->
    returnTc (combiner EmptyBinds thing, lie, thing_ty)

tcBindsAndThen combiner (SingleBind bind) do_next
  = tcBindWithSigsAndThen combiner bind [] do_next

tcBindsAndThen combiner (BindWith bind sigs) do_next
  = tcBindWithSigsAndThen combiner bind sigs do_next

tcBindsAndThen combiner (ThenBinds binds1 binds2) do_next
  = tcBindsAndThen combiner binds1 (tcBindsAndThen combiner binds2 do_next)
\end{code}

An aside.  The original version of @tcBindsAndThen@ which lacks a
combiner function, appears below.  Though it is perfectly well
behaved, it cannot be typed by Haskell, because the recursive call is
at a different type to the definition itself.  There aren't too many
examples of this, which is why I thought it worth preserving! [SLPJ]

\begin{pseudocode}
tcBindsAndThen
	:: RenamedHsBinds
	-> TcM s (thing, LIE s, thing_ty))
	-> TcM s ((TcHsBinds s, thing), LIE s, thing_ty)

tcBindsAndThen EmptyBinds do_next
  = do_next 		`thenTc` \ (thing, lie, thing_ty) ->
    returnTc ((EmptyBinds, thing), lie, thing_ty)

tcBindsAndThen (SingleBind bind) do_next
  = tcBindAndThen bind [] do_next

tcBindsAndThen (BindWith bind sigs) do_next
  = tcBindAndThen bind sigs do_next

tcBindsAndThen (ThenBinds binds1 binds2) do_next
  = tcBindsAndThen binds1 (tcBindsAndThen binds2 do_next)
	`thenTc` \ ((binds1', (binds2', thing')), lie1, thing_ty) ->

    returnTc ((binds1' `ThenBinds` binds2', thing'), lie1, thing_ty)
\end{pseudocode}


%************************************************************************
%*									*
\subsection{tcBindWithSigsAndThen}
%*									*
%************************************************************************

@tcBindAndThen@ deals with one binding group and the thing it scopes over.

\begin{code}
tcBindWithSigsAndThen
	:: (TcHsBinds s -> thing -> thing)		  -- Combinator
	-> RenamedBind					  -- The Bind to typecheck
	-> [RenamedSig]					  -- ...and its signatures
	-> TcM s (thing, LIE s, thing_ty)		  -- Thing to type check in
							  -- augmented envt
	-> TcM s (thing, LIE s, thing_ty)		  -- Results, incl the

tcBindWithSigsAndThen combiner bind sigs do_next
  = 
     recoverTc (
	-- If typechecking the binds fails, then return with each
	-- binder given type (forall a.a), to minimise subsequent
	-- error messages
	newTcTyVar mkBoxedTypeKind		`thenNF_Tc` \ alpha_tv ->
	let
	  forall_a_a = mkForAllTy alpha_tv (mkTyVarTy alpha_tv)
	  poly_ids   = [ mkUserId name forall_a_a NoPragmaInfo
		       | name <- binder_names]
	in
		-- Extend the environment to bind the new polymorphic Ids
		-- and do the thing inside
	tcExtendLocalValEnv binder_names poly_ids $
	do_next
    ) $

    fixTc (\ ~(prag_info_fn, _) ->
	-- This is the usual prag_info fix; the PragmaInfo field of an Id
	-- is not inspected till ages later in the compiler, so there
	-- should be no black-hole problems here.
    tcBindWithSigs binder_names bind 
		  sigs prag_info_fn	`thenTc` \ (poly_binds, poly_lie, poly_ids) ->

	-- Extend the environment to bind the new polymorphic Ids
    tcExtendLocalValEnv binder_names poly_ids $

	-- Build bindings and IdInfos corresponding to user pragmas
    tcPragmaSigs sigs			`thenTc` \ (prag_info_fn, prag_binds, prag_lie) ->

	-- Now do whatever happens next, in the augmented envt
    do_next				`thenTc` \ (thing, thing_lie, thing_ty) ->

	-- Create specialisations of functions bound here
    bindInstsOfLocalFuns (prag_lie `plusLIE` thing_lie)
			  poly_ids	`thenTc` \ (lie2, inst_mbinds) ->

	-- All done
    let
 	final_lie   = lie2 `plusLIE` poly_lie
	final_binds = poly_binds `ThenBinds`
		      SingleBind (NonRecBind inst_mbinds) `ThenBinds`
		      prag_binds
    in
    returnTc (prag_info_fn, (combiner final_binds thing, final_lie, thing_ty))
    )					`thenTc` \ (_, result) ->
    returnTc result
  where
    binder_names = map fst (bagToList (collectBinders bind))
\end{code}


%************************************************************************
%*									*
\subsection{tcBindWithSigs}
%*									*
%************************************************************************

@tcBindWithSigs@ deals with a single binding group.  It does generalisation,
so all the clever stuff is in here.

\begin{code}
tcBindWithSigs binder_names bind sigs prag_info_fn
  = 	-- Create a new identifier for each binder, with each being given
	-- a fresh unique, and a type-variable type.
    tcGetUniques no_of_binders			`thenNF_Tc` \ uniqs ->
    newTyVarTys no_of_binders kind		`thenNF_Tc` \ tys ->
    let
	mono_ids           = zipWith3Equal "tcBindAndSigs" mk_id binder_names uniqs tys
	mk_id name uniq ty = mkUserLocal (getOccName name) uniq ty (getSrcLoc name)
    in

	-- TYPECHECK THE SIGNATURES
    mapTc tcTySig ty_sigs				`thenTc` \ tc_ty_sigs ->

	-- TYPECHECK THE BINDINGS
    tcMonoBinds mbind binder_names mono_ids tc_ty_sigs	`thenTc` \ (mbind', lie) ->

	-- CHECK THAT THE SIGNATURES MATCH
	-- (must do this before getTyVarsToGen)
    checkSigMatch (binder_names `zip` mono_ids) tc_ty_sigs	`thenTc` \ sig_theta ->
	
	-- COMPUTE VARIABLES OVER WHICH TO QUANTIFY, namely tyvars_to_gen
	-- The tyvars_not_to_gen are free in the environment, and hence
	-- candidates for generalisation, but sometimes the monomorphism
	-- restriction means we can't generalise them nevertheless
    mapNF_Tc (zonkTcType . idType) mono_ids		`thenNF_Tc` \ mono_id_types ->
    getTyVarsToGen is_unrestricted mono_id_types lie	`thenTc` \ (tyvars_not_to_gen, tyvars_to_gen) ->
    let
	tyvars_to_gen_list = tyVarSetToList tyvars_to_gen	-- Commit to a particular order
    in

	-- SIMPLIFY THE LIE
    tcExtendGlobalTyVars tyvars_not_to_gen (
	if null tc_ty_sigs then
		-- No signatures, so just simplify the lie
	    tcSimplify tyvars_to_gen lie		`thenTc` \ (lie_free, dict_binds, lie_bound) ->
	    returnTc (lie_free, dict_binds, map instToId (bagToList lie_bound))

	else
	    newDicts SignatureOrigin sig_theta		`thenNF_Tc` \ (dicts_sig, dict_ids) ->
		-- It's important that sig_theta is zonked, because
		-- dict_id is later used to form the type of the polymorphic thing,
		-- and forall-types must be zonked so far as their bound variables
		-- are concerned

		-- Check that the needed dicts can be expressed in
		-- terms of the signature ones
	    tcAddErrCtxt (sigsCtxt tysig_names) $
	    tcSimplifyAndCheck tyvars_to_gen dicts_sig lie	`thenTc` \ (lie_free, dict_binds) ->
	    returnTc (lie_free, dict_binds, dict_ids)

    )						`thenTc` \ (lie_free, dict_binds, dicts_bound) ->

	-- DEAL WITH TYPE VARIABLE KINDS
    defaultUncommittedTyVars tyvars_to_gen_list	`thenTc_`

	 -- BUILD THE POLYMORPHIC RESULT IDs
    let
	dict_tys    = map tcIdType dicts_bound
	poly_tys    = map (mkForAllTys tyvars_to_gen_list . mkFunTys dict_tys) mono_id_types
	poly_ids    = zipWithEqual "genspecetc" mk_poly binder_names poly_tys
	mk_poly name ty = mkUserId name ty (prag_info_fn name)
    in

	-- MAKE EXTRA BINDS FOR THE TYPE-SIG POLYMORPHIC VARIABLES
	-- These are only needed to scope over the right-hand sides of the group,
	-- and hence aren't needed at all for non-recursive definitions.
	--
	-- Alas, the polymorphic variables from the type signature can't coincide
	-- with the poly_ids because the order of their type variables may not be
	-- the same.  These bindings just swizzle the type variables.
    let
	poly_binds | is_rec_bind = map mk_poly_bind tc_ty_sigs
		   | otherwise   = []

	mk_poly_bind (TySigInfo name rhs_poly_id rhs_tyvars _ _ _)
	  = (TcId rhs_poly_id, TyLam rhs_tyvars $
			       TyApp (HsVar (TcId main_poly_id)) $
			       mkTyVarTys tyvars_to_gen_list)
	  where
	    main_poly_id = head (filter ((== name) . getName) poly_ids)
    in
	 -- BUILD RESULTS
    returnTc (
	 AbsBinds tyvars_to_gen_list
		  dicts_bound
		  (zipEqual "genBinds" (map TcId mono_ids) (map TcId poly_ids))
		  (poly_binds ++ dict_binds)
		  (wrap_it mbind'),
	 lie_free,
	 poly_ids
    )
  where
    no_of_binders = length binder_names

    is_rec_bind = case bind of
	  		NonRecBind _ -> False
			RecBind _    -> True

    mbind = case bind of
		NonRecBind mb -> mb
		RecBind mb    -> mb

    ty_sigs         = [sig  | sig@(Sig name _ _) <- sigs]
    tysig_names     = [name | (Sig name _ _) <- ty_sigs]
    is_unrestricted = isUnRestrictedGroup tysig_names mbind

    kind | is_rec_bind = mkBoxedTypeKind	-- Recursive, so no unboxed types
	 | otherwise   = mkTypeKind		-- Non-recursive, so we permit unboxed types

    wrap_it mbind | is_rec_bind = RecBind mbind
		  | otherwise   = NonRecBind mbind

\end{code}

@getImplicitStuffToGen@ decides what type variables generalise over.

For a "restricted group" -- see the monomorphism restriction
for a definition -- we bind no dictionaries, and
remove from tyvars_to_gen any constrained type variables

*Don't* simplify dicts at this point, because we aren't going
to generalise over these dicts.  By the time we do simplify them
we may well know more.  For example (this actually came up)
	f :: Array Int Int
	f x = array ... xs where xs = [1,2,3,4,5]
We don't want to generate lots of (fromInt Int 1), (fromInt Int 2)
stuff.  If we simplify only at the f-binding (not the xs-binding)
we'll know that the literals are all Ints, and we can just produce
Int literals!

Find all the type variables involved in overloading, the
"constrained_tyvars".  These are the ones we *aren't* going to
generalise.  We must be careful about doing this:

 (a) If we fail to generalise a tyvar which is not actually
	constrained, then it will never, ever get bound, and lands
	up printed out in interface files!  Notorious example:
		instance Eq a => Eq (Foo a b) where ..
	Here, b is not constrained, even though it looks as if it is.
	Another, more common, example is when there's a Method inst in
	the LIE, whose type might very well involve non-overloaded
	type variables.

 (b) On the other hand, we mustn't generalise tyvars which are constrained,
	because we are going to pass on out the unmodified LIE, with those
	tyvars in it.  They won't be in scope if we've generalised them.

So we are careful, and do a complete simplification just to find the
constrained tyvars. We don't use any of the results, except to
find which tyvars are constrained.

\begin{code}
getTyVarsToGen is_unrestricted mono_id_types lie
  = tcGetGlobalTyVars 				`thenNF_Tc` \ free_tyvars ->
    let
	mentioned_tyvars = tyVarsOfTypes mono_id_types
	tyvars_to_gen    = mentioned_tyvars `minusTyVarSet` free_tyvars
    in
    if is_unrestricted
    then
	returnTc (emptyTyVarSet, tyvars_to_gen)
    else
	tcSimplify tyvars_to_gen lie	    `thenTc` \ (_, _, constrained_dicts) ->
	let
	  -- ASSERT: dicts_sig is already zonked!
	    constrained_tyvars    = foldrBag (unionTyVarSets . tyVarsOfInst) emptyTyVarSet constrained_dicts
	    reduced_tyvars_to_gen = tyvars_to_gen `minusTyVarSet` constrained_tyvars
        in
        returnTc (constrained_tyvars, reduced_tyvars_to_gen)
\end{code}


\begin{code}
isUnRestrictedGroup :: [Name]		-- Signatures given for these
		    -> RenamedMonoBinds
		    -> Bool

is_elem v vs = isIn "isUnResMono" v vs

isUnRestrictedGroup sigs (PatMonoBind (VarPatIn v) _ _) = v `is_elem` sigs
isUnRestrictedGroup sigs (PatMonoBind other      _ _)	= False
isUnRestrictedGroup sigs (VarMonoBind v _)	        = v `is_elem` sigs
isUnRestrictedGroup sigs (FunMonoBind _ _ _ _)		= True
isUnRestrictedGroup sigs (AndMonoBinds mb1 mb2)		= isUnRestrictedGroup sigs mb1 &&
							  isUnRestrictedGroup sigs mb2
isUnRestrictedGroup sigs EmptyMonoBinds			= True
\end{code}

@defaultUncommittedTyVars@ checks for generalisation over unboxed
types, and defaults any TypeKind TyVars to BoxedTypeKind.

\begin{code}
defaultUncommittedTyVars tyvars
  = ASSERT( null unboxed_kind_tyvars )	-- The instCantBeGeneralised stuff in tcSimplify
					-- should have dealt with unboxed type variables;
					-- and it's better done there because we have more
					-- precise origin information.
					-- That's why we call this *after* simplifying.
					-- (NB: unboxed tyvars are always introduced along
					--  with a class constraint.)

    mapTc box_it unresolved_kind_tyvars
  where
    unboxed_kind_tyvars    = filter (isUnboxedTypeKind . tyVarKind) tyvars
    unresolved_kind_tyvars = filter (isTypeKind        . tyVarKind) tyvars

    box_it tyvar = newTyVarTy mkBoxedTypeKind	`thenNF_Tc` \ boxed_ty ->
		   unifyTauTy boxed_ty (mkTyVarTy tyvar) 
\end{code}


%************************************************************************
%*									*
\subsection{tcMonoBind}
%*									*
%************************************************************************

@tcMonoBinds@ deals with a single @MonoBind@.  
The signatures have been dealt with already.

\begin{code}
tcMonoBinds :: RenamedMonoBinds 
	    -> [Name] -> [TcIdBndr s]
	    -> [TcSigInfo s]
	    -> TcM s (TcMonoBinds s, LIE s)

tcMonoBinds mbind binder_names mono_ids tc_ty_sigs
  = tcExtendLocalValEnv binder_names mono_ids (
	tc_mono_binds mbind
    )
  where
    sig_names = [name | (TySigInfo name _ _ _ _ _) <- tc_ty_sigs]
    sig_ids   = [id   | (TySigInfo _   id _ _ _ _) <- tc_ty_sigs]

    tc_mono_binds EmptyMonoBinds = returnTc (EmptyMonoBinds, emptyLIE)

    tc_mono_binds (AndMonoBinds mb1 mb2)
      = tc_mono_binds mb1		`thenTc` \ (mb1a, lie1) ->
        tc_mono_binds mb2		`thenTc` \ (mb2a, lie2) ->
        returnTc (AndMonoBinds mb1a mb2a, lie1 `plusLIE` lie2)

    tc_mono_binds (FunMonoBind name inf matches locn)
      = tcAddSrcLoc locn				$
	tcLookupLocalValueOK "tc_mono_binds" name	`thenNF_Tc` \ id ->

		-- Before checking the RHS, extend the envt with
		-- bindings for the *polymorphic* Ids from any type signatures
	tcExtendLocalValEnv sig_names sig_ids		$
	tcMatchesFun name (idType id) matches		`thenTc` \ (matches', lie) ->

	returnTc (FunMonoBind (TcId id) inf matches' locn, lie)

    tc_mono_binds bind@(PatMonoBind pat grhss_and_binds locn)
      = tcAddSrcLoc locn		 	$
	tcPat pat	     			`thenTc` \ (pat2, lie_pat, pat_ty) ->
	tcExtendLocalValEnv sig_names sig_ids	$
	tcGRHSsAndBinds grhss_and_binds		`thenTc` \ (grhss_and_binds2, lie, grhss_ty) ->
	tcAddErrCtxt (patMonoBindsCtxt bind)	$
	unifyTauTy pat_ty grhss_ty		`thenTc_`
	returnTc (PatMonoBind pat2 grhss_and_binds2 locn,
		  plusLIE lie_pat lie)
\end{code}

%************************************************************************
%*									*
\subsection{Signatures}
%*									*
%************************************************************************

@tcSigs@ checks the signatures for validity, and returns a list of
{\em freshly-instantiated} signatures.  That is, the types are already
split up, and have fresh type variables installed.  All non-type-signature
"RenamedSigs" are ignored.

The @TcSigInfo@ contains @TcTypes@ because they are unified with
the variable's type, and after that checked to see whether they've
been instantiated.

\begin{code}
data TcSigInfo s
  = TySigInfo	    Name
		    (TcIdBndr s)	-- *Polymorphic* binder for this value...
		    [TcTyVar s] (TcThetaType s) (TcTauType s)
		    SrcLoc
\end{code}


\begin{code}
tcTySig :: RenamedSig -> TcM s (TcSigInfo s)

tcTySig (Sig v ty src_loc)
 = tcAddSrcLoc src_loc $
   tcHsType ty			`thenTc` \ sigma_ty ->
   tcGetUnique 			`thenNF_Tc` \ uniq ->
   tcInstSigType sigma_ty	`thenNF_Tc` \ sigma_ty' ->
   let
     poly_id = mkUserLocal (getOccName v) uniq sigma_ty' src_loc
     (tyvars', theta', tau') = splitSigmaTy sigma_ty'
   in
   returnTc (TySigInfo v poly_id tyvars' theta' tau' src_loc)
\end{code}

@checkSigMatch@ does the next step in checking signature matching.
The tau-type part has already been unified.  What we do here is to
check that this unification has not over-constrained the (polymorphic)
type variables of the original signature type.

The error message here is somewhat unsatisfactory, but it'll do for
now (ToDo).

\begin{code}
checkSigMatch binder_names_w_mono_isd []
  = returnTc (error "checkSigMatch")

checkSigMatch binder_names_w_mono_ids tc_ty_sigs 
  = 

  	-- CHECK THAT THE SIGNATURE TYVARS AND TAU_TYPES ARE OK
	-- Doesn't affect substitution
    mapTc check_one_sig tc_ty_sigs	`thenTc_`

	-- CHECK THAT ALL THE SIGNATURE CONTEXTS ARE IDENTICAL
	-- The type signatures on a mutually-recursive group of definitions
	-- must all have the same context (or none).
	-- We have to zonk them first to make their type variables line up
    mapNF_Tc get_zonked_theta tc_ty_sigs		`thenNF_Tc` \ (theta:thetas) ->
    checkTc (all (eqSimpleTheta theta) thetas) 
	    (sigContextsErr tc_ty_sigs)		`thenTc_`

    returnTc theta
  where
    check_one_sig (TySigInfo name id sig_tyvars _ sig_tau src_loc)
      = tcAddSrcLoc src_loc	$
	tcAddErrCtxt (sigCtxt id) $
	unifyTauTy sig_tau mono_id_ty 		`thenTc_`
	checkSigTyVars sig_tyvars sig_tau
      where
	mono_id_ty = idType (assoc "checkSigMatch" binder_names_w_mono_ids name)

    get_zonked_theta (TySigInfo _ _ _ theta _ _)
      = mapNF_Tc (\ (c,t) -> zonkTcType t `thenNF_Tc` \ t' -> returnNF_Tc (c,t')) theta
\end{code}


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
  = tcGetGlobalTyVars			`thenNF_Tc` \ globals ->
    let
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


%************************************************************************
%*									*
\subsection{SPECIALIZE pragmas}
%*									*
%************************************************************************


@tcPragmaSigs@ munches up the "signatures" that arise through *user*
pragmas.  It is convenient for them to appear in the @[RenamedSig]@
part of a binding because then the same machinery can be used for
moving them into place as is done for type signatures.

\begin{code}
tcPragmaSigs :: [RenamedSig]			-- The pragma signatures
	     -> TcM s (Name -> PragmaInfo,	-- Maps name to the appropriate PragmaInfo
		       TcHsBinds s,
		       LIE s)

tcPragmaSigs sigs = returnTc ( \name -> NoPragmaInfo, EmptyBinds, emptyLIE )

{- 
tcPragmaSigs sigs
  = mapAndUnzip3Tc tcPragmaSig sigs	`thenTc` \ (names_w_id_infos, binds, lies) ->
    let
	name_to_info name = foldr ($) noIdInfo
				  [info_fn | (n,info_fn) <- names_w_id_infos, n==name]
    in
    returnTc (name_to_info,
	      foldr ThenBinds EmptyBinds binds,
	      foldr plusLIE emptyLIE lies)
\end{code}

Here are the easy cases for tcPragmaSigs

\begin{code}
tcPragmaSig (DeforestSig name loc)
  = returnTc ((name, addDeforestInfo DoDeforest),EmptyBinds,emptyLIE)
tcPragmaSig (InlineSig name loc)
  = returnTc ((name, addUnfoldInfo (iWantToBeINLINEd UnfoldAlways)), EmptyBinds, emptyLIE)
tcPragmaSig (MagicUnfoldingSig name string loc)
  = returnTc ((name, addUnfoldInfo (mkMagicUnfolding string)), EmptyBinds, emptyLIE)
\end{code}

The interesting case is for SPECIALISE pragmas.  There are two forms.
Here's the first form:
\begin{verbatim}
	f :: Ord a => [a] -> b -> b
	{-# SPECIALIZE f :: [Int] -> b -> b #-}
\end{verbatim}

For this we generate:
\begin{verbatim}
	f* = /\ b -> let d1 = ...
		     in f Int b d1
\end{verbatim}

where f* is a SpecPragmaId.  The **sole** purpose of SpecPragmaIds is to
retain a right-hand-side that the simplifier will otherwise discard as
dead code... the simplifier has a flag that tells it not to discard
SpecPragmaId bindings.

In this case the f* retains a call-instance of the overloaded
function, f, (including appropriate dictionaries) so that the
specialiser will subsequently discover that there's a call of @f@ at
Int, and will create a specialisation for @f@.  After that, the
binding for @f*@ can be discarded.

The second form is this:
\begin{verbatim}
	f :: Ord a => [a] -> b -> b
	{-# SPECIALIZE f :: [Int] -> b -> b = g #-}
\end{verbatim}

Here @g@ is specified as a function that implements the specialised
version of @f@.  Suppose that g has type (a->b->b); that is, g's type
is more general than that required.  For this we generate
\begin{verbatim}
	f@Int = /\b -> g Int b
	f* = f@Int
\end{verbatim}

Here @f@@Int@ is a SpecId, the specialised version of @f@.  It inherits
f's export status etc.  @f*@ is a SpecPragmaId, as before, which just serves
to prevent @f@@Int@ from being discarded prematurely.  After specialisation,
if @f@@Int@ is going to be used at all it will be used explicitly, so the simplifier can
discard the f* binding.

Actually, there is really only point in giving a SPECIALISE pragma on exported things,
and the simplifer won't discard SpecIds for exporte things anyway, so maybe this is
a bit of overkill.

\begin{code}
tcPragmaSig (SpecSig name poly_ty maybe_spec_name src_loc)
  = tcAddSrcLoc src_loc		 		$
    tcAddErrCtxt (valSpecSigCtxt name spec_ty)	$

	-- Get and instantiate its alleged specialised type
    tcHsType poly_ty				`thenTc` \ sig_sigma ->
    tcInstSigType  sig_sigma			`thenNF_Tc` \ sig_ty ->
    let
	(sig_tyvars, sig_theta, sig_tau) = splitSigmaTy sig_ty
	origin = ValSpecOrigin name
    in

	-- Check that the SPECIALIZE pragma had an empty context
    checkTc (null sig_theta)
	    (panic "SPECIALIZE non-empty context (ToDo: msg)") `thenTc_`

	-- Get and instantiate the type of the id mentioned
    tcLookupLocalValueOK "tcPragmaSig" name	`thenNF_Tc` \ main_id ->
    tcInstSigType [] (idType main_id)		`thenNF_Tc` \ main_ty ->
    let
	(main_tyvars, main_rho) = splitForAllTy main_ty
	(main_theta,main_tau)   = splitRhoTy main_rho
	main_arg_tys	        = mkTyVarTys main_tyvars
    in

	-- Check that the specialised type is indeed an instance of
	-- the type of the main function.
    unifyTauTy sig_tau main_tau		`thenTc_`
    checkSigTyVars sig_tyvars sig_tau	`thenTc_`

	-- Check that the type variables of the polymorphic function are
	-- either left polymorphic, or instantiate to ground type.
	-- Also check that the overloaded type variables are instantiated to
	-- ground type; or equivalently that all dictionaries have ground type
    mapTc zonkTcType main_arg_tys	`thenNF_Tc` \ main_arg_tys' ->
    zonkTcThetaType main_theta		`thenNF_Tc` \ main_theta' ->
    tcAddErrCtxt (specGroundnessCtxt main_arg_tys')
	      (checkTc (all isGroundOrTyVarTy main_arg_tys'))      	`thenTc_`
    tcAddErrCtxt (specContextGroundnessCtxt main_theta')
	      (checkTc (and [isGroundTy ty | (_,ty) <- theta']))	`thenTc_`

	-- Build the SpecPragmaId; it is the thing that makes sure we
	-- don't prematurely dead-code-eliminate the binding we are really interested in.
    newSpecPragmaId name sig_ty		`thenNF_Tc` \ spec_pragma_id ->

	-- Build a suitable binding; depending on whether we were given
	-- a value (Maybe Name) to be used as the specialisation.
    case using of
      Nothing ->		-- No implementation function specified

		-- Make a Method inst for the occurrence of the overloaded function
	newMethodWithGivenTy (OccurrenceOf name)
		  (TcId main_id) main_arg_tys main_rho	`thenNF_Tc` \ (lie, meth_id) ->

	let
	    pseudo_bind = VarMonoBind spec_pragma_id pseudo_rhs
	    pseudo_rhs  = mkHsTyLam sig_tyvars (HsVar (TcId meth_id))
	in
	returnTc (pseudo_bind, lie, \ info -> info)

      Just spec_name ->		-- Use spec_name as the specialisation value ...

		-- Type check a simple occurrence of the specialised Id
	tcId spec_name		`thenTc` \ (spec_body, spec_lie, spec_tau) ->

		-- Check that it has the correct type, and doesn't constrain the
		-- signature variables at all
	unifyTauTy sig_tau spec_tau   	 	`thenTc_`
	checkSigTyVars sig_tyvars sig_tau	`thenTc_`

	    -- Make a local SpecId to bind to applied spec_id
	newSpecId main_id main_arg_tys sig_ty	`thenNF_Tc` \ local_spec_id ->

	let
	    spec_rhs   = mkHsTyLam sig_tyvars spec_body
	    spec_binds = VarMonoBind local_spec_id spec_rhs
			   `AndMonoBinds`
	   		 VarMonoBind spec_pragma_id (HsVar (TcId local_spec_id))
	    spec_info  = SpecInfo spec_tys (length main_theta) local_spec_id
	in
	returnTc ((name, addSpecInfo spec_info), spec_binds, spec_lie)
-}
\end{code}


%************************************************************************
%*									*
\subsection[TcBinds-errors]{Error contexts and messages}
%*									*
%************************************************************************


\begin{code}
patMonoBindsCtxt bind sty
  = ppHang (ppPStr SLIT("In a pattern binding:")) 4 (ppr sty bind)

-----------------------------------------------
valSpecSigCtxt v ty sty
  = ppHang (ppPStr SLIT("In a SPECIALIZE pragma for a value:"))
	 4 (ppSep [ppBeside (pprNonSym sty v) (ppPStr SLIT(" ::")),
		  ppr sty ty])



-----------------------------------------------
notAsPolyAsSigErr sig_tau mono_tyvars sty
  = ppHang (ppPStr SLIT("A type signature is more polymorphic than the inferred type"))
	4  (ppAboves [ppStr "Some type variables in the inferred type can't be forall'd, namely:",
		      interpp'SP sty mono_tyvars,
		      ppPStr SLIT("Possible cause: the RHS mentions something subject to the monomorphism restriction")
		     ])

-----------------------------------------------
badMatchErr sig_ty inferred_ty sty
  = ppHang (ppPStr SLIT("Type signature doesn't match inferred type"))
	 4 (ppAboves [ppHang (ppPStr SLIT("Signature:")) 4 (ppr sty sig_ty),
		      ppHang (ppPStr SLIT("Inferred :")) 4 (ppr sty inferred_ty)
	   ])

-----------------------------------------------
sigCtxt id sty 
  = ppSep [ppPStr SLIT("When checking signature for"), ppr sty id]
sigsCtxt ids sty 
  = ppSep [ppPStr SLIT("When checking signature(s) for:"), interpp'SP sty ids]

-----------------------------------------------
sigContextsErr ty_sigs sty
  = ppHang (ppPStr SLIT("A group of type signatures have mismatched contexts"))
	 4 (ppAboves (map ppr_tc_ty_sig ty_sigs))
  where
    ppr_tc_ty_sig (TySigInfo val _ tyvars theta tau_ty _)
      = ppHang (ppBeside (ppr sty val) (ppPStr SLIT(" :: ")))
    	     4 (if null theta
		then ppNil
		else ppBesides [ppChar '(', 
			        ppIntersperse (ppStr ", ") (map (ppr_inst sty) theta), 
				ppStr ") => ..."])
    ppr_inst sty (clas, ty) = ppCat [ppr sty clas, ppr sty ty]

-----------------------------------------------
specGroundnessCtxt
  = panic "specGroundnessCtxt"

--------------------------------------------
specContextGroundnessCtxt -- err_ctxt dicts sty
  = panic "specContextGroundnessCtxt"
{-
  = ppHang (
    	ppSep [ppBesides [ppPStr SLIT("In the SPECIALIZE pragma for `"), ppr sty name, ppChar '\''],
	       ppBesides [ppPStr SLIT(" specialised to the type `"), ppr sty spec_ty,  ppChar '\''],
	       pp_spec_id sty,
	       ppPStr SLIT("... not all overloaded type variables were instantiated"),
	       ppPStr SLIT("to ground types:")])
      4 (ppAboves [ppCat [ppr sty c, ppr sty t]
		  | (c,t) <- map getDictClassAndType dicts])
  where
    (name, spec_ty, locn, pp_spec_id)
      = case err_ctxt of
	  ValSpecSigCtxt    n ty loc      -> (n, ty, loc, \ x -> ppNil)
	  ValSpecSpecIdCtxt n ty spec loc ->
	    (n, ty, loc,
	     \ sty -> ppBesides [ppPStr SLIT("... type of explicit id `"), ppr sty spec, ppChar '\''])
-}
\end{code}




