%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcBinds]{TcBinds}

\begin{code}
module TcBinds ( tcBindsAndThen, tcTopBindsAndThen,
	         tcSpecSigs, tcBindWithSigs ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcMatches ( tcGRHSs, tcMatchesFun )
import {-# SOURCE #-} TcExpr  ( tcExpr )

import HsSyn		( HsExpr(..), HsBinds(..), MonoBinds(..), Sig(..), InPat(..), StmtCtxt(..),
			  Match(..), collectMonoBinders, andMonoBindList, andMonoBinds
			)
import RnHsSyn		( RenamedHsBinds, RenamedSig, RenamedMonoBinds )
import TcHsSyn		( TcHsBinds, TcMonoBinds, TcId, zonkId, mkHsLet )

import TcMonad
import Inst		( Inst, LIE, emptyLIE, mkLIE, plusLIE, plusLIEs, InstOrigin(..),
			  newDicts, tyVarsOfInst, instToId,
			  getAllFunDepsOfLIE, getIPsOfLIE, zonkFunDeps
			)
import TcEnv		( tcExtendLocalValEnv,
			  newSpecPragmaId, newLocalId,
			  tcLookupTyConByKey, 
			  tcGetGlobalTyVars, tcExtendGlobalTyVars
			)
import TcSimplify	( tcSimplify, tcSimplifyAndCheck, tcSimplifyToDicts )
import TcImprove	( tcImprove )
import TcMonoType	( tcHsSigType, checkSigTyVars,
			  TcSigInfo(..), tcTySig, maybeSig, sigCtxt
			)
import TcPat		( tcPat )
import TcSimplify	( bindInstsOfLocalFuns )
import TcType		( TcType, TcThetaType,
			  TcTyVar,
			  newTyVarTy, newTyVar, newTyVarTy_OpenKind, tcInstTcType,
			  zonkTcType, zonkTcTypes, zonkTcThetaType, zonkTcTyVarToTyVar
			)
import TcUnify		( unifyTauTy, unifyTauTyLists )

import Id		( Id, mkVanillaId, setInlinePragma, idFreeTyVars )
import Var		( idType, idName )
import IdInfo		( setInlinePragInfo, InlinePragInfo(..) )
import Name		( Name, getName, getOccName, getSrcLoc )
import NameSet
import Type		( mkTyVarTy, tyVarsOfTypes, mkTyConApp,
			  splitSigmaTy, mkForAllTys, mkFunTys, getTyVar, 
			  mkPredTy, splitRhoTy, mkForAllTy, isUnLiftedType, 
			  isUnboxedType, unboxedTypeKind, boxedTypeKind
			)
import FunDeps		( tyVarFunDep, oclose )
import Var		( TyVar, tyVarKind )
import VarSet
import Bag
import Util		( isIn )
import Maybes		( maybeToBool )
import BasicTypes	( TopLevelFlag(..), RecFlag(..), isNotTopLevel )
import FiniteMap	( listToFM, lookupFM )
import Unique		( ioTyConKey, mainKey, hasKey, Uniquable(..) )
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
	:: (RecFlag -> TcMonoBinds -> thing -> thing)		-- Combinator
	-> RenamedHsBinds
	-> TcM s (thing, LIE)
	-> TcM s (thing, LIE)

tcTopBindsAndThen = tc_binds_and_then TopLevel
tcBindsAndThen    = tc_binds_and_then NotTopLevel

tc_binds_and_then top_lvl combiner EmptyBinds do_next
  = do_next
tc_binds_and_then top_lvl combiner (MonoBind EmptyMonoBinds sigs is_rec) do_next
  = do_next

tc_binds_and_then top_lvl combiner (ThenBinds b1 b2) do_next
  = tc_binds_and_then top_lvl combiner b1	$
    tc_binds_and_then top_lvl combiner b2	$
    do_next

tc_binds_and_then top_lvl combiner (MonoBind bind sigs is_rec) do_next
  =   	-- TYPECHECK THE SIGNATURES
      mapTc tcTySig [sig | sig@(Sig name _ _) <- sigs]	`thenTc` \ tc_ty_sigs ->
  
      tcBindWithSigs top_lvl bind tc_ty_sigs
		     sigs is_rec 			`thenTc` \ (poly_binds, poly_lie, poly_ids) ->
  
	  -- Extend the environment to bind the new polymorphic Ids
      tcExtendLocalValEnv [(idName poly_id, poly_id) | poly_id <- poly_ids] $
  
	  -- Build bindings and IdInfos corresponding to user pragmas
      tcSpecSigs sigs		`thenTc` \ (prag_binds, prag_lie) ->

	-- Now do whatever happens next, in the augmented envt
      do_next			`thenTc` \ (thing, thing_lie) ->

	-- Create specialisations of functions bound here
	-- We want to keep non-recursive things non-recursive
	-- so that we desugar unboxed bindings correctly
      case (top_lvl, is_rec) of

		-- For the top level don't bother will all this bindInstsOfLocalFuns stuff
		-- All the top level things are rec'd together anyway, so it's fine to
		-- leave them to the tcSimplifyTop, and quite a bit faster too
	(TopLevel, _)
		-> returnTc (combiner Recursive (poly_binds `andMonoBinds` prag_binds) thing,
			     thing_lie `plusLIE` prag_lie `plusLIE` poly_lie)

	(NotTopLevel, NonRecursive) 
		-> bindInstsOfLocalFuns 
				(thing_lie `plusLIE` prag_lie)
				poly_ids			`thenTc` \ (thing_lie', lie_binds) ->

		   returnTc (
			combiner NonRecursive poly_binds $
			combiner NonRecursive prag_binds $
			combiner Recursive lie_binds  $
				-- NB: the binds returned by tcSimplify and bindInstsOfLocalFuns
				-- aren't guaranteed in dependency order (though we could change
				-- that); hence the Recursive marker.
			thing,

			thing_lie' `plusLIE` poly_lie
		   )

	(NotTopLevel, Recursive)
		-> bindInstsOfLocalFuns 
				(thing_lie `plusLIE` poly_lie `plusLIE` prag_lie) 
				poly_ids			`thenTc` \ (final_lie, lie_binds) ->

		   returnTc (
			combiner Recursive (
				poly_binds `andMonoBinds`
				lie_binds  `andMonoBinds`
				prag_binds) thing,
			final_lie
		   )
\end{code}

An aside.  The original version of @tcBindsAndThen@ which lacks a
combiner function, appears below.  Though it is perfectly well
behaved, it cannot be typed by Haskell, because the recursive call is
at a different type to the definition itself.  There aren't too many
examples of this, which is why I thought it worth preserving! [SLPJ]

\begin{pseudocode}
% tcBindsAndThen
% 	:: RenamedHsBinds
% 	-> TcM s (thing, LIE, thing_ty))
% 	-> TcM s ((TcHsBinds, thing), LIE, thing_ty)
% 
% tcBindsAndThen EmptyBinds do_next
%   = do_next 		`thenTc` \ (thing, lie, thing_ty) ->
%     returnTc ((EmptyBinds, thing), lie, thing_ty)
% 
% tcBindsAndThen (ThenBinds binds1 binds2) do_next
%   = tcBindsAndThen binds1 (tcBindsAndThen binds2 do_next)
% 	`thenTc` \ ((binds1', (binds2', thing')), lie1, thing_ty) ->
% 
%     returnTc ((binds1' `ThenBinds` binds2', thing'), lie1, thing_ty)
% 
% tcBindsAndThen (MonoBind bind sigs is_rec) do_next
%   = tcBindAndThen bind sigs do_next
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
	-> RenamedMonoBinds
	-> [TcSigInfo]
	-> [RenamedSig]		-- Used solely to get INLINE, NOINLINE sigs
	-> RecFlag
	-> TcM s (TcMonoBinds, LIE, [TcId])

tcBindWithSigs top_lvl mbind tc_ty_sigs inline_sigs is_rec
  = recoverTc (
	-- If typechecking the binds fails, then return with each
	-- signature-less binder given type (forall a.a), to minimise subsequent
	-- error messages
	newTyVar boxedTypeKind		`thenNF_Tc` \ alpha_tv ->
	let
	  forall_a_a    = mkForAllTy alpha_tv (mkTyVarTy alpha_tv)
          binder_names  = map fst (bagToList (collectMonoBinders mbind))
	  poly_ids      = map mk_dummy binder_names
	  mk_dummy name = case maybeSig tc_ty_sigs name of
			    Just (TySigInfo _ poly_id _ _ _ _ _ _) -> poly_id	-- Signature
			    Nothing -> mkVanillaId name forall_a_a          	-- No signature
	in
	returnTc (EmptyMonoBinds, emptyLIE, poly_ids)
    ) $

	-- TYPECHECK THE BINDINGS
    tcMonoBinds mbind tc_ty_sigs is_rec		`thenTc` \ (mbind', lie_req, binder_names, mono_ids) ->

	-- CHECK THAT THE SIGNATURES MATCH
	-- (must do this before getTyVarsToGen)
    checkSigMatch top_lvl binder_names mono_ids tc_ty_sigs	`thenTc` \ maybe_sig_theta ->	

	-- IMPROVE the LIE
	-- Force any unifications dictated by functional dependencies.
	-- Because unification may happen, it's important that this step
	-- come before:
	--   - computing vars over which to quantify
	--   - zonking the generalized type vars
    let lie_avail = case maybe_sig_theta of
		      Nothing	   -> emptyLIE
		      Just (_, la) -> la in
    tcImprove (lie_avail `plusLIE` lie_req)			`thenTc_`

	-- COMPUTE VARIABLES OVER WHICH TO QUANTIFY, namely tyvars_to_gen
	-- The tyvars_not_to_gen are free in the environment, and hence
	-- candidates for generalisation, but sometimes the monomorphism
	-- restriction means we can't generalise them nevertheless
    let
	mono_id_tys = map idType mono_ids
    in
    getTyVarsToGen is_unrestricted mono_id_tys lie_req	`thenNF_Tc` \ (tyvars_not_to_gen, tyvars_to_gen) ->

	-- Finally, zonk the generalised type variables to real TyVars
	-- This commits any unbound kind variables to boxed kind
	-- I'm a little worried that such a kind variable might be
	-- free in the environment, but I don't think it's possible for
	-- this to happen when the type variable is not free in the envt
	-- (which it isn't).		SLPJ Nov 98
    mapTc zonkTcTyVarToTyVar (varSetElems tyvars_to_gen)	`thenTc` \ real_tyvars_to_gen_list ->
    let
	real_tyvars_to_gen = mkVarSet real_tyvars_to_gen_list
		-- It's important that the final list 
		-- (real_tyvars_to_gen and real_tyvars_to_gen_list) is fully
		-- zonked, *including boxity*, because they'll be included in the forall types of
		-- the polymorphic Ids, and instances of these Ids will be generated from them.
		-- 
		-- Also NB that tcSimplify takes zonked tyvars as its arg, hence we pass
		-- real_tyvars_to_gen
    in

	-- SIMPLIFY THE LIE
    tcExtendGlobalTyVars tyvars_not_to_gen (
	let ips = getIPsOfLIE lie_req in
	if null real_tyvars_to_gen_list && (null ips || not is_unrestricted) then
		-- No polymorphism, and no IPs, so no need to simplify context
	    returnTc (lie_req, EmptyMonoBinds, [])
	else
	case maybe_sig_theta of
	  Nothing ->
		-- No signatures, so just simplify the lie
		-- NB: no signatures => no polymorphic recursion, so no
		-- need to use lie_avail (which will be empty anyway)
	    tcSimplify (text "tcBinds1" <+> ppr binder_names)
		       real_tyvars_to_gen lie_req	`thenTc` \ (lie_free, dict_binds, lie_bound) ->
	    returnTc (lie_free, dict_binds, map instToId (bagToList lie_bound))

	  Just (sig_theta, lie_avail) ->
		-- There are signatures, and their context is sig_theta
		-- Furthermore, lie_avail is an LIE containing the 'method insts'
		-- for the things bound here

	    zonkTcThetaType sig_theta			`thenNF_Tc` \ sig_theta' ->
	    newDicts SignatureOrigin sig_theta'		`thenNF_Tc` \ (dicts_sig, dict_ids) ->
		-- It's important that sig_theta is zonked, because
		-- dict_id is later used to form the type of the polymorphic thing,
		-- and forall-types must be zonked so far as their bound variables
		-- are concerned

	    let
		-- The "givens" is the stuff available.  We get that from
		-- the context of the type signature, BUT ALSO the lie_avail
		-- so that polymorphic recursion works right (see comments at end of fn)
		givens = dicts_sig `plusLIE` lie_avail
	    in

		-- Check that the needed dicts can be expressed in
		-- terms of the signature ones
	    tcAddErrCtxt  (bindSigsCtxt tysig_names) $
	    tcSimplifyAndCheck
		(ptext SLIT("type signature for") <+> pprQuotedList binder_names)
	    	real_tyvars_to_gen givens lie_req	`thenTc` \ (lie_free, dict_binds) ->

	    returnTc (lie_free, dict_binds, dict_ids)

    )						`thenTc` \ (lie_free, dict_binds, dicts_bound) ->

	-- GET THE FINAL MONO_ID_TYS
    zonkTcTypes mono_id_tys			`thenNF_Tc` \ zonked_mono_id_types ->


	-- CHECK FOR BOGUS UNPOINTED BINDINGS
    (if any isUnLiftedType zonked_mono_id_types then
		-- Unlifted bindings must be non-recursive,
		-- not top level, and non-polymorphic
	checkTc (isNotTopLevel top_lvl)
		(unliftedBindErr "Top-level" mbind)		`thenTc_`
	checkTc (case is_rec of {Recursive -> False; NonRecursive -> True})
		(unliftedBindErr "Recursive" mbind)		`thenTc_`
	checkTc (null real_tyvars_to_gen_list)
		(unliftedBindErr "Polymorphic" mbind)
     else
	returnTc ()
    )							`thenTc_`

    ASSERT( not (any ((== unboxedTypeKind) . tyVarKind) real_tyvars_to_gen_list) )
		-- The instCantBeGeneralised stuff in tcSimplify should have
		-- already raised an error if we're trying to generalise an 
		-- unboxed tyvar (NB: unboxed tyvars are always introduced 
		-- along with a class constraint) and it's better done there 
		-- because we have more precise origin information.
		-- That's why we just use an ASSERT here.


    	 -- BUILD THE POLYMORPHIC RESULT IDs
    mapNF_Tc zonkId mono_ids		`thenNF_Tc` \ zonked_mono_ids ->
    let
	exports  = zipWith mk_export binder_names zonked_mono_ids
	dict_tys = map idType dicts_bound

	inlines    = mkNameSet [name | InlineSig name _ loc <- inline_sigs]
        no_inlines = listToFM ([(name, IMustNotBeINLINEd False phase) | NoInlineSig name phase loc <- inline_sigs] ++
			       [(name, IMustNotBeINLINEd True  phase) | InlineSig   name phase loc <- inline_sigs, maybeToBool phase])
		-- "INLINE n foo" means inline foo, but not until at least phase n
		-- "NOINLINE n foo" means don't inline foo until at least phase n, and even 
		--		    then only if it is small enough etc.
		-- "NOINLINE foo" means don't inline foo ever, which we signal with a (IMustNotBeINLINEd Nothing)
		-- See comments in CoreUnfold.blackListed for the Authorised Version

	mk_export binder_name zonked_mono_id
	  = (tyvars, 
	     attachNoInlinePrag no_inlines poly_id,
	     zonked_mono_id)
	  where
	    (tyvars, poly_id) = 
		case maybeSig tc_ty_sigs binder_name of
		  Just (TySigInfo _ sig_poly_id sig_tyvars _ _ _ _ _) -> 
			(sig_tyvars, sig_poly_id)
		  Nothing -> (real_tyvars_to_gen_list, new_poly_id)

	    new_poly_id = mkVanillaId binder_name poly_ty
	    poly_ty = mkForAllTys real_tyvars_to_gen_list 
			$ mkFunTys dict_tys 
			$ idType (zonked_mono_id)
		-- It's important to build a fully-zonked poly_ty, because
		-- we'll slurp out its free type variables when extending the
		-- local environment (tcExtendLocalValEnv); if it's not zonked
		-- it appears to have free tyvars that aren't actually free 
		-- at all.
	
	pat_binders :: [Name]
	pat_binders = map fst $ bagToList $ collectMonoBinders $ 
		      (justPatBindings mbind EmptyMonoBinds)
    in
	-- CHECK FOR UNBOXED BINDERS IN PATTERN BINDINGS
    mapTc (\id -> checkTc (not (idName id `elem` pat_binders
				&& isUnboxedType (idType id)))
			  (unboxedPatBindErr id)) zonked_mono_ids
				`thenTc_`

	 -- BUILD RESULTS
    returnTc (
	 -- pprTrace "binding.." (ppr ((dicts_bound, dict_binds), exports, [idType poly_id | (_, poly_id, _) <- exports])) $
	 AbsBinds real_tyvars_to_gen_list
		  dicts_bound
		  exports
		  inlines
		  (dict_binds `andMonoBinds` mbind'),
	 lie_free,
	 [poly_id | (_, poly_id, _) <- exports]
    )
  where
    tysig_names     = [name | (TySigInfo name _ _ _ _ _ _ _) <- tc_ty_sigs]
    is_unrestricted = isUnRestrictedGroup tysig_names mbind

justPatBindings bind@(PatMonoBind _ _ _) binds = bind `andMonoBinds` binds
justPatBindings (AndMonoBinds b1 b2) binds = 
	justPatBindings b1 (justPatBindings b2 binds) 
justPatBindings other_bind binds = binds

attachNoInlinePrag no_inlines bndr
  = case lookupFM no_inlines (idName bndr) of
	Just prag -> bndr `setInlinePragma` prag
	Nothing   -> bndr
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
polymorphic recursion isn't being used (but that's a very common case).
We'd prefer

	f = /\a -> \d::Eq a -> letrec
				 fm = \ys:[a] -> ...fm...
			       in
			       fm

This can lead to a massive space leak, from the following top-level defn
(post-typechecking)

	ff :: [Int] -> [Int]
	ff = f Int dEqInt

Now (f dEqInt) evaluates to a lambda that has f' as a free variable; but
f' is another thunk which evaluates to the same thing... and you end
up with a chain of identical values all hung onto by the CAF ff.

	ff = f Int dEqInt

	   = let f' = f Int dEqInt in \ys. ...f'...

	   = let f' = let f' = f Int dEqInt in \ys. ...f'...
		      in \ys. ...f'...

Etc.
Solution: when typechecking the RHSs we always have in hand the
*monomorphic* Ids for each binding.  So we just need to make sure that
if (Method f a d) shows up in the constraints emerging from (...f...)
we just use the monomorphic Id.  We achieve this by adding monomorphic Ids
to the "givens" when simplifying constraints.  That's what the "lies_avail"
is doing.


%************************************************************************
%*									*
\subsection{getTyVarsToGen}
%*									*
%************************************************************************

@getTyVarsToGen@ decides what type variables to generalise over.

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
	body_tyvars = tyVarsOfTypes zonked_mono_id_tys `minusVarSet` free_tyvars
	fds	    = getAllFunDepsOfLIE lie
    in
    if is_unrestricted
    then
	  -- We need to augment the type variables that appear explicitly in
	  -- the type by those that are determined by the functional dependencies.
	  -- e.g. suppose our type is	C a b => a -> a
	  --	with the fun-dep  a->b
	  -- Then we should generalise over b too; otherwise it will be
	  -- reported as ambiguous.
	zonkFunDeps fds		`thenNF_Tc` \ fds' ->
	let tvFundep 	    = tyVarFunDep fds'
	    extended_tyvars = oclose tvFundep body_tyvars
 	in
	returnNF_Tc (emptyVarSet, extended_tyvars)
    else
	-- This recover and discard-errs is to avoid duplicate error
	-- messages; this, after all, is an "extra" call to tcSimplify
	recoverNF_Tc (returnNF_Tc (emptyVarSet, body_tyvars))		$
	discardErrsTc							$

	tcSimplify (text "getTVG") body_tyvars lie    `thenTc` \ (_, _, constrained_dicts) ->
	let
	  -- ASSERT: dicts_sig is already zonked!
	    constrained_tyvars    = foldrBag (unionVarSet . tyVarsOfInst) emptyVarSet constrained_dicts
	    reduced_tyvars_to_gen = body_tyvars `minusVarSet` constrained_tyvars
        in
        returnTc (constrained_tyvars, reduced_tyvars_to_gen)
\end{code}


\begin{code}
isUnRestrictedGroup :: [Name]		-- Signatures given for these
		    -> RenamedMonoBinds
		    -> Bool

is_elem v vs = isIn "isUnResMono" v vs

isUnRestrictedGroup sigs (PatMonoBind other        _ _) = False
isUnRestrictedGroup sigs (VarMonoBind v _)	        = v `is_elem` sigs
isUnRestrictedGroup sigs (FunMonoBind v _ matches _)	= any isUnRestrictedMatch matches || 
							  v `is_elem` sigs
isUnRestrictedGroup sigs (AndMonoBinds mb1 mb2)		= isUnRestrictedGroup sigs mb1 &&
							  isUnRestrictedGroup sigs mb2
isUnRestrictedGroup sigs EmptyMonoBinds			= True

isUnRestrictedMatch (Match _ [] Nothing _) = False	-- No args, no signature
isUnRestrictedMatch other		   = True	-- Some args or a signature
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
	    -> [TcSigInfo]
	    -> RecFlag
	    -> TcM s (TcMonoBinds, 
		      LIE,		-- LIE required
		      [Name],		-- Bound names
		      [TcId])	-- Corresponding monomorphic bound things

tcMonoBinds mbinds tc_ty_sigs is_rec
  = tc_mb_pats mbinds		`thenTc` \ (complete_it, lie_req_pat, tvs, ids, lie_avail) ->
    let
	tv_list		  = bagToList tvs
	id_list 	  = bagToList ids
	(names, mono_ids) = unzip id_list

		-- This last defn is the key one:
		-- extend the val envt with bindings for the 
		-- things bound in this group, overriding the monomorphic
		-- ids with the polymorphic ones from the pattern
	extra_val_env = case is_rec of
			  Recursive    -> map mk_bind id_list
			  NonRecursive -> []
    in
	-- Don't know how to deal with pattern-bound existentials yet
    checkTc (isEmptyBag tvs && isEmptyBag lie_avail) 
	    (existentialExplode mbinds)			`thenTc_` 

	-- *Before* checking the RHSs, but *after* checking *all* the patterns,
	-- extend the envt with bindings for all the bound ids;
	--   and *then* override with the polymorphic Ids from the signatures
	-- That is the whole point of the "complete_it" stuff.
	--
	-- There's a further wrinkle: we have to delay extending the environment
	-- until after we've dealt with any pattern-bound signature type variables
	-- Consider  f (x::a) = ...f...
	-- We're going to check that a isn't unified with anything in the envt, 
	-- so f itself had better not be!  So we pass the envt binding f into
	-- complete_it, which extends the actual envt in TcMatches.tcMatch, after
	-- dealing with the signature tyvars

    complete_it extra_val_env				`thenTc` \ (mbinds', lie_req_rhss) ->

    returnTc (mbinds', lie_req_pat `plusLIE` lie_req_rhss, names, mono_ids)
  where

	-- This function is used when dealing with a LHS binder; we make a monomorphic
	-- version of the Id.  We check for type signatures
    tc_pat_bndr name pat_ty
	= case maybeSig tc_ty_sigs name of
	    Nothing
		-> newLocalId (getOccName name) pat_ty (getSrcLoc name)

	    Just (TySigInfo _ _ _ _ _ mono_id _ _)
		-> tcAddSrcLoc (getSrcLoc name)				$
		   unifyTauTy (idType mono_id) pat_ty 	`thenTc_`
		   returnTc mono_id

    mk_bind (name, mono_id) = case maybeSig tc_ty_sigs name of
				Nothing 				  -> (name, mono_id)
				Just (TySigInfo name poly_id _ _ _ _ _ _) -> (name, poly_id)

    tc_mb_pats EmptyMonoBinds
      = returnTc (\ xve -> returnTc (EmptyMonoBinds, emptyLIE), emptyLIE, emptyBag, emptyBag, emptyLIE)

    tc_mb_pats (AndMonoBinds mb1 mb2)
      = tc_mb_pats mb1		`thenTc` \ (complete_it1, lie_req1, tvs1, ids1, lie_avail1) ->
        tc_mb_pats mb2		`thenTc` \ (complete_it2, lie_req2, tvs2, ids2, lie_avail2) ->
	let
	   complete_it xve = complete_it1 xve	`thenTc` \ (mb1', lie1) ->
			     complete_it2 xve	`thenTc` \ (mb2', lie2) ->
			     returnTc (AndMonoBinds mb1' mb2', lie1 `plusLIE` lie2)
	in
	returnTc (complete_it,
		  lie_req1 `plusLIE` lie_req2,
		  tvs1 `unionBags` tvs2,
		  ids1 `unionBags` ids2,
		  lie_avail1 `plusLIE` lie_avail2)

    tc_mb_pats (FunMonoBind name inf matches locn)
      = new_lhs_ty 			`thenNF_Tc` \ bndr_ty ->
	tc_pat_bndr name bndr_ty	`thenTc` \ bndr_id ->
	let
	   complete_it xve = tcAddSrcLoc locn				$
			     tcMatchesFun xve name bndr_ty  matches	`thenTc` \ (matches', lie) ->
			     returnTc (FunMonoBind bndr_id inf matches' locn, lie)
	in
	returnTc (complete_it, emptyLIE, emptyBag, unitBag (name, bndr_id), emptyLIE)

    tc_mb_pats bind@(PatMonoBind pat grhss locn)
      = tcAddSrcLoc locn	 	$
	new_lhs_ty			`thenNF_Tc` \ pat_ty -> 

		-- 	Now typecheck the pattern
		-- We don't support binding fresh type variables in the
		-- pattern of a pattern binding.  For example, this is illegal:
		--	(x::a, y::b) = e
		-- whereas this is ok
		--	(x::Int, y::Bool) = e
		--
		-- We don't check explicitly for this problem.  Instead, we simply
		-- type check the pattern with tcPat.  If the pattern mentions any
		-- fresh tyvars we simply get an out-of-scope type variable error
	tcPat tc_pat_bndr pat pat_ty		`thenTc` \ (pat', lie_req, tvs, ids, lie_avail) ->
	let
	   complete_it xve = tcAddSrcLoc locn		 		$
			     tcAddErrCtxt (patMonoBindsCtxt bind)	$
			     tcExtendLocalValEnv xve			$
			     tcGRHSs grhss pat_ty PatBindRhs		`thenTc` \ (grhss', lie) ->
			     returnTc (PatMonoBind pat' grhss' locn, lie)
	in
	returnTc (complete_it, lie_req, tvs, ids, lie_avail)

	-- Figure out the appropriate kind for the pattern,
	-- and generate a suitable type variable 
    new_lhs_ty = case is_rec of
		     Recursive    -> newTyVarTy boxedTypeKind	-- Recursive, so no unboxed types
		     NonRecursive -> newTyVarTy_OpenKind 	-- Non-recursive, so we permit unboxed types
\end{code}

%************************************************************************
%*									*
\subsection{Signatures}
%*									*
%************************************************************************

@checkSigMatch@ does the next step in checking signature matching.
The tau-type part has already been unified.  What we do here is to
check that this unification has not over-constrained the (polymorphic)
type variables of the original signature type.

The error message here is somewhat unsatisfactory, but it'll do for
now (ToDo).

\begin{code}
checkSigMatch :: TopLevelFlag -> [Name] -> [TcId] -> [TcSigInfo] -> TcM s (Maybe (TcThetaType, LIE))
checkSigMatch top_lvl binder_names mono_ids sigs
  | main_bound_here
  =	-- First unify the main_id with IO t, for any old t
    tcSetErrCtxt mainTyCheckCtxt (
	tcLookupTyConByKey ioTyConKey		`thenTc`    \ ioTyCon ->
	newTyVarTy boxedTypeKind		`thenNF_Tc` \ t_tv ->
	unifyTauTy ((mkTyConApp ioTyCon [t_tv]))
		   (idType main_mono_id)
    )						`thenTc_`

	-- Now check the signatures
	-- Must do this after the unification with IO t, 
	-- in case of a silly signature like
	-- 	main :: forall a. a
	-- The unification to IO t will bind the type variable 'a',
	-- which is just waht check_one_sig looks for
    mapTc check_one_sig sigs			`thenTc_`
    mapTc check_main_ctxt sigs			`thenTc_` 

	    returnTc (Just ([], emptyLIE))

  | not (null sigs)
  = mapTc check_one_sig sigs			`thenTc_`
    mapTc check_one_ctxt all_sigs_but_first	`thenTc_`
    returnTc (Just (theta1, sig_lie))

  | otherwise
  = returnTc Nothing		-- No constraints from type sigs

  where
    (TySigInfo _ id1 _ theta1 _ _ _ _ : all_sigs_but_first) = sigs

    sig1_dict_tys	= mk_dict_tys theta1
    n_sig1_dict_tys	= length sig1_dict_tys
    sig_lie 		= mkLIE (concat [insts | TySigInfo _ _ _ _ _ _ insts _ <- sigs])

    maybe_main        = find_main top_lvl binder_names mono_ids
    main_bound_here   = maybeToBool maybe_main
    Just main_mono_id = maybe_main
		      
   	-- CHECK THAT THE SIGNATURE TYVARS AND TAU_TYPES ARE OK
	-- Doesn't affect substitution
    check_one_sig (TySigInfo _ id sig_tyvars sig_theta sig_tau _ _ src_loc)
      = tcAddSrcLoc src_loc					$
	tcAddErrCtxtM (sigCtxt (sig_msg id) sig_tyvars sig_theta sig_tau)	$
	checkSigTyVars sig_tyvars (idFreeTyVars id)


	-- CHECK THAT ALL THE SIGNATURE CONTEXTS ARE UNIFIABLE
	-- The type signatures on a mutually-recursive group of definitions
	-- must all have the same context (or none).
	--
	-- We unify them because, with polymorphic recursion, their types
	-- might not otherwise be related.  This is a rather subtle issue.
	-- ToDo: amplify
    check_one_ctxt sig@(TySigInfo _ id _ theta _ _ _ src_loc)
       = tcAddSrcLoc src_loc	$
	 tcAddErrCtxt (sigContextsCtxt id1 id) $
	 checkTc (length this_sig_dict_tys == n_sig1_dict_tys)
				sigContextsErr 		`thenTc_`
	 unifyTauTyLists sig1_dict_tys this_sig_dict_tys
      where
	 this_sig_dict_tys = mk_dict_tys theta

	-- CHECK THAT FOR A GROUP INVOLVING Main.main, all 
	-- the signature contexts are empty (what a bore)
    check_main_ctxt sig@(TySigInfo _ id _ theta _ _ _ src_loc)
	= tcAddSrcLoc src_loc	$
	  checkTc (null theta) (mainContextsErr id)

    mk_dict_tys theta = map mkPredTy theta

    sig_msg id = ptext SLIT("When checking the type signature for") <+> quotes (ppr id)

	-- Search for Main.main in the binder_names, return corresponding mono_id
    find_main NotTopLevel binder_names mono_ids = Nothing
    find_main TopLevel    binder_names mono_ids = go binder_names mono_ids
    go [] [] = Nothing
    go (n:ns) (m:ms) | n `hasKey` mainKey = Just m
		     | otherwise          = go ns ms
\end{code}


%************************************************************************
%*									*
\subsection{SPECIALIZE pragmas}
%*									*
%************************************************************************

@tcSpecSigs@ munches up the specialisation "signatures" that arise through *user*
pragmas.  It is convenient for them to appear in the @[RenamedSig]@
part of a binding because then the same machinery can be used for
moving them into place as is done for type signatures.

They look like this:

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

We used to have a form
	{-# SPECIALISE f :: <type> = g #-}
which promised that g implemented f at <type>, but we do that with 
a RULE now:
	{-# SPECIALISE (f::<type) = g #-}

\begin{code}
tcSpecSigs :: [RenamedSig] -> TcM s (TcMonoBinds, LIE)
tcSpecSigs (SpecSig name poly_ty src_loc : sigs)
  = 	-- SPECIALISE f :: forall b. theta => tau  =  g
    tcAddSrcLoc src_loc		 		$
    tcAddErrCtxt (valSpecSigCtxt name poly_ty)	$

	-- Get and instantiate its alleged specialised type
    tcHsSigType poly_ty				`thenTc` \ sig_ty ->

	-- Check that f has a more general type, and build a RHS for
	-- the spec-pragma-id at the same time
    tcExpr (HsVar name) sig_ty			`thenTc` \ (spec_expr, spec_lie) ->

	-- Squeeze out any Methods (see comments with tcSimplifyToDicts)
    tcSimplifyToDicts spec_lie			`thenTc` \ (spec_lie1, spec_binds) ->

    	-- Just specialise "f" by building a SpecPragmaId binding
	-- It is the thing that makes sure we don't prematurely 
	-- dead-code-eliminate the binding we are really interested in.
    newSpecPragmaId name sig_ty		`thenNF_Tc` \ spec_id ->

	-- Do the rest and combine
    tcSpecSigs sigs			`thenTc` \ (binds_rest, lie_rest) ->
    returnTc (binds_rest `andMonoBinds` VarMonoBind spec_id (mkHsLet spec_binds spec_expr),
	      lie_rest   `plusLIE`      spec_lie1)

tcSpecSigs (other_sig : sigs) = tcSpecSigs sigs
tcSpecSigs []		      = returnTc (EmptyMonoBinds, emptyLIE)
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
	 nest 4 (ppr v <+> dcolon <+> ppr ty)]

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
unboxedPatBindErr id
  = ptext SLIT("variable in a lazy pattern binding has unboxed type: ")
	 <+> quotes (ppr id)

-----------------------------------------------
bindSigsCtxt ids
  = ptext SLIT("When checking the type signature(s) for") <+> pprQuotedList ids

-----------------------------------------------
sigContextsErr
  = ptext SLIT("Mismatched contexts")

sigContextsCtxt s1 s2
  = hang (hsep [ptext SLIT("When matching the contexts of the signatures for"), 
		quotes (ppr s1), ptext SLIT("and"), quotes (ppr s2)])
	 4 (ptext SLIT("(the signature contexts in a mutually recursive group should all be identical)"))

mainContextsErr id
  | id `hasKey` mainKey = ptext SLIT("Main.main cannot be overloaded")
  | otherwise
  = quotes (ppr id) <+> ptext SLIT("cannot be overloaded") <> char ',' <> -- sigh; workaround for cpp's inability to deal
    ptext SLIT("because it is mutually recursive with Main.main")         -- with commas inside SLIT strings.

mainTyCheckCtxt
  = hsep [ptext SLIT("When checking that"), quotes (ptext SLIT("main")),
	  ptext SLIT("has the required type")]

-----------------------------------------------
unliftedBindErr flavour mbind
  = hang (text flavour <+> ptext SLIT("bindings for unlifted types aren't allowed"))
	 4 (ppr mbind)

existentialExplode mbinds
  = hang (vcat [text "My brain just exploded.",
	        text "I can't handle pattern bindings for existentially-quantified constructors.",
		text "In the binding group"])
	4 (ppr mbinds)
\end{code}
