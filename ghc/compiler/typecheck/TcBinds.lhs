%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcBinds]{TcBinds}

\begin{code}
module TcBinds ( tcBindsAndThen, tcTopBindsAndThen,
	         tcPragmaSigs, checkSigTyVars, tcBindWithSigs, 
		 sigCtxt, TcSigInfo(..) ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcGRHSs ( tcGRHSsAndBinds )

import HsSyn		( HsBinds(..), MonoBinds(..), Sig(..), InPat(..),
			  collectMonoBinders
			)
import RnHsSyn		( RenamedHsBinds, RenamedSig(..), 
			  RenamedMonoBinds
			)
import TcHsSyn		( TcHsBinds, TcMonoBinds,
			  TcIdOcc(..), TcIdBndr, 
			  tcIdType
			)

import TcMonad
import Inst		( Inst, LIE, emptyLIE, plusLIE, plusLIEs, InstOrigin(..),
			  newDicts, tyVarsOfInst, instToId, newMethodWithGivenTy,
			  zonkInst, pprInsts
			)
import TcEnv		( tcExtendLocalValEnv, tcLookupLocalValueOK, newLocalId,
			  tcGetGlobalTyVars, tcExtendGlobalTyVars
			)
import TcMatches	( tcMatchesFun )
import TcSimplify	( tcSimplify, tcSimplifyAndCheck )
import TcMonoType	( tcHsType )
import TcPat		( tcPat )
import TcSimplify	( bindInstsOfLocalFuns )
import TcType		( TcType, TcThetaType, TcTauType, 
			  TcTyVarSet, TcTyVar,
			  newTyVarTy, newTcTyVar, tcInstSigType, tcInstSigTcType,
			  zonkTcType, zonkTcTypes, zonkTcThetaType, zonkTcTyVar
			)
import Unify		( unifyTauTy, unifyTauTyLists )

import Kind		( isUnboxedTypeKind, mkTypeKind, isTypeKind, mkBoxedTypeKind )
import MkId		( mkUserId )
import Id		( idType, idName, idInfo, replaceIdInfo )
import IdInfo		( IdInfo, noIdInfo, setInlinePragInfo, InlinePragInfo(..) )
import Maybes		( maybeToBool, assocMaybe )
import Name		( getOccName, getSrcLoc, Name )
import Type		( mkTyVarTy, mkTyVarTys, isTyVarTy, tyVarsOfTypes,
			  splitSigmaTy, mkForAllTys, mkFunTys, getTyVar, mkDictTy,
			  splitRhoTy, mkForAllTy, splitForAllTys
			)
import TyVar		( TyVar, tyVarKind, mkTyVarSet, minusTyVarSet, emptyTyVarSet,
			  elementOfTyVarSet, unionTyVarSets, tyVarSetToList
			)
import Bag		( bagToList, foldrBag, )
import Util		( isIn, hasNoDups, assoc )
import Unique		( Unique )
import BasicTypes	( TopLevelFlag(..), RecFlag(..) )
import SrcLoc           ( SrcLoc )
import Outputable
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
tcTopBindsAndThen, tcBindsAndThen
	:: (RecFlag -> TcMonoBinds s -> this -> that)		-- Combinator
	-> RenamedHsBinds
	-> TcM s (this, LIE s)
	-> TcM s (that, LIE s)

tcTopBindsAndThen = tc_binds_and_then TopLevel
tcBindsAndThen    = tc_binds_and_then NotTopLevel

tc_binds_and_then top_lvl combiner binds do_next
  = tcBinds top_lvl binds 	`thenTc` \ (mbinds1, binds_lie, env, ids) ->
    tcSetEnv env		$

	-- Now do whatever happens next, in the augmented envt
    do_next			`thenTc` \ (thing, thing_lie) ->

	-- Create specialisations of functions bound here
	-- Nota Bene: we glom the bindings all together in a single
	-- recursive group ("recursive" passed to combiner, below)
	-- so that we can do thsi bindInsts thing once for all the bindings
	-- and the thing inside.  This saves a quadratic-cost algorithm
	-- when there's a long sequence of bindings.
    bindInstsOfLocalFuns (binds_lie `plusLIE` thing_lie) ids	`thenTc` \ (final_lie, mbinds2) ->

	-- All done
    let
	final_mbinds = mbinds1 `AndMonoBinds` mbinds2
    in
    returnTc (combiner Recursive final_mbinds thing, final_lie)

tcBinds :: TopLevelFlag
	-> RenamedHsBinds
	-> TcM s (TcMonoBinds s, LIE s, TcEnv s, [TcIdBndr s])
	   -- The envt is the envt with binders in scope
	   -- The binders are those bound by this group of bindings

tcBinds top_lvl EmptyBinds
  = tcGetEnv		`thenNF_Tc` \ env ->
    returnTc (EmptyMonoBinds, emptyLIE, env, [])

  -- Short-cut for the rather common case of an empty bunch of bindings
tcBinds top_lvl (MonoBind EmptyMonoBinds sigs is_rec)
  = tcGetEnv		`thenNF_Tc` \ env ->
    returnTc (EmptyMonoBinds, emptyLIE, env, [])

tcBinds top_lvl (ThenBinds binds1 binds2)
  = tcBinds top_lvl binds1	  `thenTc` \ (mbinds1, lie1, env1, ids1) ->
    tcSetEnv env1		  $
    tcBinds top_lvl binds2	  `thenTc` \ (mbinds2, lie2, env2, ids2) ->
    returnTc (mbinds1 `AndMonoBinds` mbinds2, lie1 `plusLIE` lie2, env2, ids1++ids2)
    
tcBinds top_lvl (MonoBind bind sigs is_rec)
  = fixTc (\ ~(prag_info_fn, _) ->
	-- This is the usual prag_info fix; the PragmaInfo field of an Id
	-- is not inspected till ages later in the compiler, so there
	-- should be no black-hole problems here.

  	-- TYPECHECK THE SIGNATURES
      mapTc (tcTySig prag_info_fn) ty_sigs		`thenTc` \ tc_ty_sigs ->
  
      tcBindWithSigs top_lvl binder_names bind 
		     tc_ty_sigs is_rec prag_info_fn	`thenTc` \ (poly_binds, poly_lie, poly_ids) ->
  
	  -- Extend the environment to bind the new polymorphic Ids
      tcExtendLocalValEnv binder_names poly_ids $
  
	  -- Build bindings and IdInfos corresponding to user pragmas
      tcPragmaSigs sigs			`thenTc` \ (prag_info_fn, prag_binds, prag_lie) ->
  
	  -- Catch the environment and return
      tcGetEnv			     `thenNF_Tc` \ env ->
      returnTc (prag_info_fn, (poly_binds `AndMonoBinds` prag_binds, 
			       poly_lie `plusLIE` prag_lie, 
			       env, poly_ids)
    ) )					`thenTc` \ (_, result) ->
    returnTc result
  where
    binder_names = map fst (bagToList (collectMonoBinders bind))
    ty_sigs      = [sig  | sig@(Sig name _ _) <- sigs]
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

tcBindsAndThen (ThenBinds binds1 binds2) do_next
  = tcBindsAndThen binds1 (tcBindsAndThen binds2 do_next)
	`thenTc` \ ((binds1', (binds2', thing')), lie1, thing_ty) ->

    returnTc ((binds1' `ThenBinds` binds2', thing'), lie1, thing_ty)

tcBindsAndThen (MonoBind bind sigs is_rec) do_next
  = tcBindAndThen bind sigs do_next
\end{pseudocode}


%************************************************************************
%*									*
\subsection{tcBindWithSigs}
%*									*
%************************************************************************

@tcBindWithSigs@ deals with a single binding group.  It does generalisation,
so all the clever stuff is in here.

* binder_names and mbind must define the same set of Names

* The Names in tc_ty_sigs must be a subset of binder_names

* The Ids in tc_ty_sigs don't necessarily have to have the same name
  as the Name in the tc_ty_sig

\begin{code}
tcBindWithSigs	
	:: TopLevelFlag
	-> [Name]
	-> RenamedMonoBinds
	-> [TcSigInfo s]
	-> RecFlag
	-> (Name -> IdInfo)
	-> TcM s (TcMonoBinds s, LIE s, [TcIdBndr s])

tcBindWithSigs top_lvl binder_names mbind tc_ty_sigs is_rec prag_info_fn
  = recoverTc (
	-- If typechecking the binds fails, then return with each
	-- signature-less binder given type (forall a.a), to minimise subsequent
	-- error messages
	newTcTyVar mkBoxedTypeKind		`thenNF_Tc` \ alpha_tv ->
	let
	  forall_a_a = mkForAllTy alpha_tv (mkTyVarTy alpha_tv)
	  poly_ids   = map mk_dummy binder_names
	  mk_dummy name = case maybeSig tc_ty_sigs name of
			    Just (TySigInfo _ poly_id _ _ _ _) -> poly_id	-- Signature
			    Nothing -> mkUserId name forall_a_a          	-- No signature
	in
	returnTc (EmptyMonoBinds, emptyLIE, poly_ids)
    ) $

      	-- Create a new identifier for each binder, with each being given
	-- a fresh unique, and a type-variable type.
	-- For "mono_lies" see comments about polymorphic recursion at the 
	-- end of the function.
    mapAndUnzipNF_Tc mk_mono_id binder_names	`thenNF_Tc` \ (mono_lies, mono_ids) ->
    let
	mono_lie = plusLIEs mono_lies
	mono_id_tys = map idType mono_ids
    in

	-- TYPECHECK THE BINDINGS
    tcMonoBinds mbind binder_names mono_ids tc_ty_sigs	`thenTc` \ (mbind', lie) ->

	-- CHECK THAT THE SIGNATURES MATCH
	-- (must do this before getTyVarsToGen)
    checkSigMatch tc_ty_sigs				`thenTc` \ sig_theta ->
	
	-- COMPUTE VARIABLES OVER WHICH TO QUANTIFY, namely tyvars_to_gen
	-- The tyvars_not_to_gen are free in the environment, and hence
	-- candidates for generalisation, but sometimes the monomorphism
	-- restriction means we can't generalise them nevertheless
    getTyVarsToGen is_unrestricted mono_id_tys lie	`thenTc` \ (tyvars_not_to_gen, tyvars_to_gen) ->

	-- DEAL WITH TYPE VARIABLE KINDS
	-- **** This step can do unification => keep other zonking after this ****
    mapTc defaultUncommittedTyVar (tyVarSetToList tyvars_to_gen)	`thenTc` \ real_tyvars_to_gen_list ->
    let
	real_tyvars_to_gen = mkTyVarSet real_tyvars_to_gen_list
		-- It's important that the final list 
		-- (real_tyvars_to_gen and real_tyvars_to_gen_list) is fully
		-- zonked, *including boxity*, because they'll be included in the forall types of
		-- the polymorphic Ids, and instances of these Ids will be generated from them.
		-- 
		-- Also NB that tcSimplify takes zonked tyvars as its arg, hence we pass
		-- real_tyvars_to_gen
		--
    in

	-- SIMPLIFY THE LIE
    tcExtendGlobalTyVars (tyVarSetToList tyvars_not_to_gen) (
	if null tc_ty_sigs then
		-- No signatures, so just simplify the lie
		-- NB: no signatures => no polymorphic recursion, so no
		-- need to use mono_lies (which will be empty anyway)
	    tcSimplify (text "tcBinds1" <+> ppr binder_names)
		       top_lvl real_tyvars_to_gen lie	`thenTc` \ (lie_free, dict_binds, lie_bound) ->
	    returnTc (lie_free, dict_binds, map instToId (bagToList lie_bound))

	else
	    zonkTcThetaType sig_theta			`thenNF_Tc` \ sig_theta' ->
	    newDicts SignatureOrigin sig_theta'		`thenNF_Tc` \ (dicts_sig, dict_ids) ->
		-- It's important that sig_theta is zonked, because
		-- dict_id is later used to form the type of the polymorphic thing,
		-- and forall-types must be zonked so far as their bound variables
		-- are concerned

	    let
		-- The "givens" is the stuff available.  We get that from
		-- the context of the type signature, BUT ALSO the mono_lie
		-- so that polymorphic recursion works right (see comments at end of fn)
		givens = dicts_sig `plusLIE` mono_lie
	    in

		-- Check that the needed dicts can be expressed in
		-- terms of the signature ones
	    tcAddErrCtxt  (bindSigsCtxt tysig_names) $
	    tcSimplifyAndCheck
		(ptext SLIT("type signature for") <+> 
		 hsep (punctuate comma (map (quotes . ppr) binder_names)))
	    	real_tyvars_to_gen givens lie		`thenTc` \ (lie_free, dict_binds) ->

	    returnTc (lie_free, dict_binds, dict_ids)

    )						`thenTc` \ (lie_free, dict_binds, dicts_bound) ->

    ASSERT( not (any (isUnboxedTypeKind . tyVarKind) real_tyvars_to_gen_list) )
		-- The instCantBeGeneralised stuff in tcSimplify should have
		-- already raised an error if we're trying to generalise an unboxed tyvar
		-- (NB: unboxed tyvars are always introduced along with a class constraint)
		-- and it's better done there because we have more precise origin information.
		-- That's why we just use an ASSERT here.

    	 -- BUILD THE POLYMORPHIC RESULT IDs
    zonkTcTypes mono_id_tys			`thenNF_Tc` \ zonked_mono_id_types ->
    let
	exports  = zipWith3 mk_export binder_names mono_ids zonked_mono_id_types
	dict_tys = map tcIdType dicts_bound

	mk_export binder_name mono_id zonked_mono_id_ty
	  | maybeToBool maybe_sig = (sig_tyvars,              TcId sig_poly_id, TcId mono_id)
	  | otherwise		  = (real_tyvars_to_gen_list, TcId poly_id,     TcId mono_id)
	  where
	    maybe_sig = maybeSig tc_ty_sigs binder_name
	    Just (TySigInfo _ sig_poly_id sig_tyvars _ _ _) = maybe_sig
	    poly_id = replaceIdInfo (mkUserId binder_name poly_ty) (prag_info_fn binder_name)
	    poly_ty = mkForAllTys real_tyvars_to_gen_list $ mkFunTys dict_tys $ zonked_mono_id_ty
				-- It's important to build a fully-zonked poly_ty, because
				-- we'll slurp out its free type variables when extending the
				-- local environment (tcExtendLocalValEnv); if it's not zonked
				-- it appears to have free tyvars that aren't actually free at all.
    in

	 -- BUILD RESULTS
    returnTc (
	 AbsBinds real_tyvars_to_gen_list
		  dicts_bound
		  exports
		  (dict_binds `AndMonoBinds` mbind'),
	 lie_free,
	 [poly_id | (_, TcId poly_id, _) <- exports]
    )
  where
    no_of_binders = length binder_names

    mk_mono_id binder_name
      |  theres_a_signature	-- There's a signature; and it's overloaded, 
      && not (null sig_theta)	-- so make a Method
      = tcAddSrcLoc sig_loc $
	newMethodWithGivenTy SignatureOrigin 
		(TcId poly_id) (mkTyVarTys sig_tyvars) 
		sig_theta sig_tau			`thenNF_Tc` \ (mono_lie, TcId mono_id) ->
							-- A bit turgid to have to strip the TcId
	returnNF_Tc (mono_lie, mono_id)

      | otherwise		-- No signature or not overloaded; 
      = tcAddSrcLoc (getSrcLoc binder_name) $
	(if theres_a_signature then
		returnNF_Tc sig_tau	-- Non-overloaded signature; use its type
	 else
		newTyVarTy kind		-- No signature; use a new type variable
	)					`thenNF_Tc` \ mono_id_ty ->

	newLocalId (getOccName binder_name) mono_id_ty	`thenNF_Tc` \ mono_id ->
	returnNF_Tc (emptyLIE, mono_id)
      where
	maybe_sig	   = maybeSig tc_ty_sigs binder_name
	theres_a_signature = maybeToBool maybe_sig
	Just (TySigInfo name poly_id sig_tyvars sig_theta sig_tau sig_loc) = maybe_sig

    tysig_names     = [name | (TySigInfo name _ _ _ _ _) <- tc_ty_sigs]
    is_unrestricted = isUnRestrictedGroup tysig_names mbind

    kind = case is_rec of
	     Recursive -> mkBoxedTypeKind	-- Recursive, so no unboxed types
	     NonRecursive -> mkTypeKind		-- Non-recursive, so we permit unboxed types
\end{code}

Polymorphic recursion
~~~~~~~~~~~~~~~~~~~~~
The game plan for polymorphic recursion in the code above is 

	* Bind any variable for which we have a type signature
	  to an Id with a polymorphic type.  Then when type-checking 
	  the RHSs we'll make a full polymorphic call.

This fine, but if you aren't a bit careful you end up with a horrendous
amount of partial application and (worse) a huge space leak. For example:

	f :: Eq a => [a] -> [a]
	f xs = ...f...

If we don't take care, after typechecking we get

	f = /\a -> \d::Eq a -> let f' = f a d
			       in
			       \ys:[a] -> ...f'...

Notice the the stupid construction of (f a d), which is of course
identical to the function we're executing.  In this case, the
polymorphic recursion ins't being used (but that's a very common case).

This can lead to a massive space leak, from the following top-level defn:

	ff :: [Int] -> [Int]
	ff = f dEqInt

Now (f dEqInt) evaluates to a lambda that has f' as a free variable; but
f' is another thunk which evaluates to the same thing... and you end
up with a chain of identical values all hung onto by the CAF ff.

Solution: when typechecking the RHSs we always have in hand the
*monomorphic* Ids for each binding.  So we just need to make sure that
if (Method f a d) shows up in the constraints emerging from (...f...)
we just use the monomorphic Id.  We achieve this by adding monomorphic Ids
to the "givens" when simplifying constraints.  Thats' what the "mono_lies"
is doing.


%************************************************************************
%*									*
\subsection{getTyVarsToGen}
%*									*
%************************************************************************

@getTyVarsToGen@ decides what type variables generalise over.

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
getTyVarsToGen is_unrestricted mono_id_tys lie
  = tcGetGlobalTyVars			`thenNF_Tc` \ free_tyvars ->
    zonkTcTypes mono_id_tys		`thenNF_Tc` \ zonked_mono_id_tys ->
    let
	tyvars_to_gen = tyVarsOfTypes zonked_mono_id_tys `minusTyVarSet` free_tyvars
    in
    if is_unrestricted
    then
	returnTc (emptyTyVarSet, tyvars_to_gen)
    else
	tcSimplify (text "getTVG") NotTopLevel tyvars_to_gen lie    `thenTc` \ (_, _, constrained_dicts) ->
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

@defaultUncommittedTyVar@ checks for generalisation over unboxed
types, and defaults any TypeKind TyVars to BoxedTypeKind.

\begin{code}
defaultUncommittedTyVar tyvar
  | isTypeKind (tyVarKind tyvar)
  = newTcTyVar mkBoxedTypeKind					`thenNF_Tc` \ boxed_tyvar ->
    unifyTauTy (mkTyVarTy boxed_tyvar) (mkTyVarTy tyvar)	`thenTc_`
    returnTc boxed_tyvar

  | otherwise
  = returnTc tyvar
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
	tcAddErrCtxt (patMonoBindsCtxt bind)	$
	tcPat pat	     			`thenTc` \ (pat2, lie_pat, pat_ty) ->

		-- Before checking the RHS, but after the pattern, extend the envt with
		-- bindings for the *polymorphic* Ids from any type signatures
	tcExtendLocalValEnv sig_names sig_ids	$
	tcGRHSsAndBinds pat_ty grhss_and_binds	`thenTc` \ (grhss_and_binds2, lie) ->
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
  = TySigInfo	    
	Name			-- N, the Name in corresponding binding
	(TcIdBndr s)		-- *Polymorphic* binder for this value...
				-- Usually has name = N, but doesn't have to.
	[TcTyVar s]
	(TcThetaType s)
	(TcTauType s)
	SrcLoc


maybeSig :: [TcSigInfo s] -> Name -> Maybe (TcSigInfo s)
	-- Search for a particular signature
maybeSig [] name = Nothing
maybeSig (sig@(TySigInfo sig_name _ _ _ _ _) : sigs) name
  | name == sig_name = Just sig
  | otherwise	     = maybeSig sigs name
\end{code}


\begin{code}
tcTySig :: (Name -> IdInfo)
	-> RenamedSig
	-> TcM s (TcSigInfo s)

tcTySig prag_info_fn (Sig v ty src_loc)
 = tcAddSrcLoc src_loc $
   tcHsType ty			`thenTc` \ sigma_ty ->

	-- Convert from Type to TcType	
   tcInstSigType sigma_ty	`thenNF_Tc` \ sigma_tc_ty ->
   let
     poly_id = replaceIdInfo (mkUserId v sigma_tc_ty) (prag_info_fn v)
   in
	-- Instantiate this type
	-- It's important to do this even though in the error-free case
	-- we could just split the sigma_tc_ty (since the tyvars don't
	-- unified with anything).  But in the case of an error, when
	-- the tyvars *do* get unified with something, we want to carry on
	-- typechecking the rest of the program with the function bound
	-- to a pristine type, namely sigma_tc_ty
   tcInstSigTcType sigma_tc_ty	`thenNF_Tc` \ (tyvars, rho) ->
   let
     (theta, tau) = splitRhoTy rho
	-- This splitSigmaTy tries hard to make sure that tau' is a type synonym
	-- wherever possible, which can improve interface files.
   in
   returnTc (TySigInfo v poly_id tyvars theta tau src_loc)
\end{code}

@checkSigMatch@ does the next step in checking signature matching.
The tau-type part has already been unified.  What we do here is to
check that this unification has not over-constrained the (polymorphic)
type variables of the original signature type.

The error message here is somewhat unsatisfactory, but it'll do for
now (ToDo).

\begin{code}
checkSigMatch []
  = returnTc (error "checkSigMatch")

checkSigMatch tc_ty_sigs@( sig1@(TySigInfo _ id1 _ theta1 _ _) : all_sigs_but_first )
  = 	-- CHECK THAT THE SIGNATURE TYVARS AND TAU_TYPES ARE OK
	-- Doesn't affect substitution
    mapTc check_one_sig tc_ty_sigs	`thenTc_`

	-- CHECK THAT ALL THE SIGNATURE CONTEXTS ARE UNIFIABLE
	-- The type signatures on a mutually-recursive group of definitions
	-- must all have the same context (or none).
	--
	-- We unify them because, with polymorphic recursion, their types
	-- might not otherwise be related.  This is a rather subtle issue.
	-- ToDo: amplify
    mapTc check_one_cxt all_sigs_but_first		`thenTc_`

    returnTc theta1
  where
    sig1_dict_tys	= mk_dict_tys theta1
    n_sig1_dict_tys	= length sig1_dict_tys

    check_one_cxt sig@(TySigInfo _ id _  theta _ src_loc)
       = tcAddSrcLoc src_loc	$
	 tcAddErrCtxt (sigContextsCtxt id1 id) $
	 checkTc (length this_sig_dict_tys == n_sig1_dict_tys)
				sigContextsErr 		`thenTc_`
	 unifyTauTyLists sig1_dict_tys this_sig_dict_tys
      where
	 this_sig_dict_tys = mk_dict_tys theta

    check_one_sig (TySigInfo name id sig_tyvars _ sig_tau src_loc)
      = tcAddSrcLoc src_loc	$
	tcAddErrCtxt (sigCtxt id) $
	checkSigTyVars sig_tyvars sig_tau

    mk_dict_tys theta = [mkDictTy c ts | (c,ts) <- theta]
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

	(c) not mentioned in the environment
		eg the signature for f in this:

			g x = ... where
					f :: a->[a]
					f y = [x,y]

		Here, f is forced to be monorphic by the free occurence of x.

Before doing this, the substitution is applied to the signature type variable.

We used to have the notion of a "DontBind" type variable, which would
only be bound to itself or nothing.  Then points (a) and (b) were 
self-checking.  But it gave rise to bogus consequential error messages.
For example:

   f = (*)	-- Monomorphic

   g :: Num a => a -> a
   g x = f x x

Here, we get a complaint when checking the type signature for g,
that g isn't polymorphic enough; but then we get another one when
dealing with the (Num x) context arising from f's definition;
we try to unify x with Int (to default it), but find that x has already
been unified with the DontBind variable "a" from g's signature.
This is really a problem with side-effecting unification; we'd like to
undo g's effects when its type signature fails, but unification is done
by side effect, so we can't (easily).

So we revert to ordinary type variables for signatures, and try to
give a helpful message in checkSigTyVars.

\begin{code}
checkSigTyVars :: [TcTyVar s]		-- The original signature type variables
	       -> TcType s		-- signature type (for err msg)
	       -> TcM s [TcTyVar s]	-- Zonked signature type variables

checkSigTyVars sig_tyvars sig_tau
  = mapNF_Tc zonkTcTyVar sig_tyvars	`thenNF_Tc` \ sig_tys ->
    let
	sig_tyvars' = map (getTyVar "checkSigTyVars") sig_tys
    in

	-- Check points (a) and (b)
    checkTcM (all isTyVarTy sig_tys && hasNoDups sig_tyvars')
	     (zonkTcType sig_tau 	`thenNF_Tc` \ sig_tau' ->
	      failWithTc (badMatchErr sig_tau sig_tau')
	     )				`thenTc_`

	-- Check point (c)
	-- We want to report errors in terms of the original signature tyvars,
	-- ie sig_tyvars, NOT sig_tyvars'.  sig_tyvars' correspond
	-- 1-1 with sig_tyvars, so we can just map back.
    tcGetGlobalTyVars			`thenNF_Tc` \ globals ->
    let
	mono_tyvars' = [sig_tv' | sig_tv' <- sig_tyvars', 
				  sig_tv' `elementOfTyVarSet` globals]

	mono_tyvars = map (assoc "checkSigTyVars" (sig_tyvars' `zip` sig_tyvars)) mono_tyvars'
    in
    checkTcM (null mono_tyvars')
	     (failWithTc (notAsPolyAsSigErr sig_tau mono_tyvars))	`thenTc_`

    returnTc sig_tyvars'
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
tcPragmaSigs :: [RenamedSig]		-- The pragma signatures
	     -> TcM s (Name -> IdInfo,	-- Maps name to the appropriate IdInfo
		       TcMonoBinds s,
		       LIE s)

-- For now we just deal with INLINE pragmas
tcPragmaSigs sigs = returnTc (prag_fn, EmptyMonoBinds, emptyLIE )
  where
    prag_fn name = info
	      where
		 info | any has_inline sigs = IWantToBeINLINEd `setInlinePragInfo` noIdInfo
		      | otherwise	    = noIdInfo

		 has_inline (InlineSig n _) = (n == name)
		 has_inline other	    = False
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
{-
tcPragmaSig :: RenamedSig -> TcM s ((Name, IdInfo -> IdInfo), TcMonoBinds s, LIE s)
tcPragmaSig (InlineSig name loc)
  = returnTc ((name, setInlinePragInfo IdWantsToBeINLINEd), EmptyBinds, emptyLIE)

tcPragmaSig (SpecSig name poly_ty maybe_spec_name src_loc)
  = tcAddSrcLoc src_loc		 		$
    tcAddErrCtxt (valSpecSigCtxt name spec_ty)	$

	-- Get and instantiate its alleged specialised type
    tcHsType poly_ty				`thenTc` \ sig_sigma ->
    tcInstSigType  sig_sigma			`thenNF_Tc` \ sig_ty ->

	-- Typecheck the RHS 
	--	f :: sig_ty
    tcPolyExpr str (Var name) sig_ty	`thenTc` \ (rhs, lie) ->

	-- If this succeeds, then the signature is indeed less general
	-- than the main function
    let
	(tyvars, tys, template)
	  = case rhs of
		TyLam tyvars (DictLam dicts (HsLet (MonoBind dict_binds
we can take apart the RHS, 
	-- which will be of very specific form
    

    tcLookupLocalValueOK "tcPragmaSig" name	`thenNF_Tc` \ main_id ->

	-- Check that the specialised signature is an instance
	-- of the 
    let
	rhs_name = case maybe_spec_name of
			Just name -> name
			other     -> name
    in
   
	-- Build the SpecPragmaId; it is the thing that makes sure we
	-- don't prematurely dead-code-eliminate the binding we are really interested in.
    newSpecPragmaId name sig_ty		`thenNF_Tc` \ spec_id ->

    returnTc ((name, ...),
	      VarMonoBind spec_id rhs,
	      lie)
-}
\end{code}


%************************************************************************
%*									*
\subsection[TcBinds-errors]{Error contexts and messages}
%*									*
%************************************************************************


\begin{code}
patMonoBindsCtxt bind
  = hang (ptext SLIT("In a pattern binding:")) 4 (ppr bind)

-----------------------------------------------
valSpecSigCtxt v ty
  = sep [ptext SLIT("In a SPECIALIZE pragma for a value:"),
	 nest 4 (ppr v <+> ptext SLIT(" ::") <+> ppr ty)]

-----------------------------------------------
notAsPolyAsSigErr sig_tau mono_tyvars
  = hang (ptext SLIT("A type signature is more polymorphic than the inferred type"))
	4  (vcat [text "Can't for-all the type variable(s)" <+> 
		  pprQuotedList mono_tyvars,
		  text "in the type" <+> quotes (ppr sig_tau)
	   ])

-----------------------------------------------
badMatchErr sig_ty inferred_ty
  = hang (ptext SLIT("Type signature doesn't match inferred type"))
	 4 (vcat [hang (ptext SLIT("Signature:")) 4 (ppr sig_ty),
		      hang (ptext SLIT("Inferred :")) 4 (ppr inferred_ty)
	   ])

-----------------------------------------------
sigCtxt id 
  = sep [ptext SLIT("When checking the type signature for"), quotes (ppr id)]

bindSigsCtxt ids
  = ptext SLIT("When checking the type signature(s) for") <+> pprQuotedList ids

-----------------------------------------------
sigContextsErr
  = ptext SLIT("Mismatched contexts")
sigContextsCtxt s1 s2
  = hang (hsep [ptext SLIT("When matching the contexts of the signatures for"), 
		quotes (ppr s1), ptext SLIT("and"), quotes (ppr s2)])
	 4 (ptext SLIT("(the signature contexts in a mutually recursive group should all be identical)"))

-----------------------------------------------
specGroundnessCtxt
  = panic "specGroundnessCtxt"

--------------------------------------------
specContextGroundnessCtxt -- err_ctxt dicts
  = panic "specContextGroundnessCtxt"
{-
  = hang (
    	sep [hsep [ptext SLIT("In the SPECIALIZE pragma for"), ppr name],
	     hcat [ptext SLIT(" specialised to the type"), ppr spec_ty],
	     pp_spec_id,
	     ptext SLIT("... not all overloaded type variables were instantiated"),
	     ptext SLIT("to ground types:")])
      4 (vcat [hsep [ppr c, ppr t]
		  | (c,t) <- map getDictClassAndType dicts])
  where
    (name, spec_ty, locn, pp_spec_id)
      = case err_ctxt of
	  ValSpecSigCtxt    n ty loc      -> (n, ty, loc, \ x -> empty)
	  ValSpecSpecIdCtxt n ty spec loc ->
	    (n, ty, loc,
	     hsep [ptext SLIT("... type of explicit id"), ppr spec])
-}
\end{code}
