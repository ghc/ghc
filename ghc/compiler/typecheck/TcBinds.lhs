%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcBinds]{TcBinds}

\begin{code}
module TcBinds ( tcBindsAndThen, tcTopBinds, tcMonoBinds, tcSpecSigs ) where

#include "HsVersions.h"

import {-# SOURCE #-} TcMatches ( tcGRHSsPat, tcMatchesFun )
import {-# SOURCE #-} TcExpr  ( tcCheckSigma, tcCheckRho )

import CmdLineOpts	( DynFlag(Opt_NoMonomorphismRestriction) )
import HsSyn		( HsExpr(..), HsBinds(..), MonoBinds(..), Sig(..), 
			  Match(..), mkMonoBind,
			  collectMonoBinders, andMonoBinds,
			  collectSigTysFromMonoBinds
			)
import RnHsSyn		( RenamedHsBinds, RenamedSig, RenamedMonoBinds )
import TcHsSyn		( TcHsBinds, TcMonoBinds, TcId, zonkId, mkHsLet )

import TcRnMonad
import Inst		( InstOrigin(..), newDicts, newIPDict, instToId )
import TcEnv		( tcExtendLocalValEnv, tcExtendLocalValEnv2, newLocalName )
import TcUnify		( Expected(..), newHole, unifyTauTyLists, checkSigTyVarsWrt, sigCtxt )
import TcSimplify	( tcSimplifyInfer, tcSimplifyInferCheck, tcSimplifyRestricted, 
			  tcSimplifyToDicts, tcSimplifyIPs )
import TcMonoType	( tcHsSigType, UserTypeCtxt(..), TcSigInfo(..), 
			  tcTySig, maybeSig, tcSigPolyId, tcSigMonoId, tcAddScopedTyVars
			)
import TcPat		( tcPat, tcSubPat, tcMonoPatBndr )
import TcSimplify	( bindInstsOfLocalFuns )
import TcMType		( newTyVar, newTyVarTy, zonkTcTyVarToTyVar )
import TcType		( TcTyVar, mkTyVarTy, mkForAllTys, mkFunTys, tyVarsOfType, 
			  mkPredTy, mkForAllTy, isUnLiftedType, 
			  unliftedTypeKind, liftedTypeKind, openTypeKind, eqKind
			)

import CoreFVs		( idFreeTyVars )
import Id		( mkLocalId, mkSpecPragmaId, setInlinePragma )
import Var		( idType, idName )
import Name		( Name, getSrcLoc )
import NameSet
import Var		( tyVarKind )
import VarSet
import Bag
import Util		( isIn, equalLength )
import BasicTypes	( TopLevelFlag(..), RecFlag(..), isNonRec, isRec, 
			  isNotTopLevel, isAlwaysActive )
import FiniteMap	( listToFM, lookupFM )
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
tcTopBinds :: RenamedHsBinds -> TcM (TcMonoBinds, TcLclEnv)
	-- Note: returning the TcLclEnv is more than we really
	--       want.  The bit we care about is the local bindings
	--	 and the free type variables thereof
tcTopBinds binds
  = tc_binds_and_then TopLevel glue binds	$
    getLclEnv					`thenM` \ env ->
    returnM (EmptyMonoBinds, env)
  where
	-- The top level bindings are flattened into a giant 
	-- implicitly-mutually-recursive MonoBinds
    glue binds1 (binds2, env) = (flatten binds1 `AndMonoBinds` binds2, env)
    flatten EmptyBinds 	        = EmptyMonoBinds
    flatten (b1 `ThenBinds` b2) = flatten b1 `AndMonoBinds` flatten b2
    flatten (MonoBind b _ _)	= b
	-- Can't have a IPBinds at top level


tcBindsAndThen
	:: (TcHsBinds -> thing -> thing)		-- Combinator
	-> RenamedHsBinds
	-> TcM thing
	-> TcM thing

tcBindsAndThen = tc_binds_and_then NotTopLevel

tc_binds_and_then top_lvl combiner EmptyBinds do_next
  = do_next
tc_binds_and_then top_lvl combiner (MonoBind EmptyMonoBinds sigs is_rec) do_next
  = do_next

tc_binds_and_then top_lvl combiner (ThenBinds b1 b2) do_next
  = tc_binds_and_then top_lvl combiner b1	$
    tc_binds_and_then top_lvl combiner b2	$
    do_next

tc_binds_and_then top_lvl combiner (IPBinds binds) do_next
  = getLIE do_next			`thenM` \ (result, expr_lie) ->
    mapAndUnzipM tc_ip_bind binds	`thenM` \ (avail_ips, binds') ->

	-- If the binding binds ?x = E, we  must now 
	-- discharge any ?x constraints in expr_lie
    tcSimplifyIPs avail_ips expr_lie	`thenM` \ dict_binds ->

    returnM (combiner (IPBinds binds') $
	     combiner (mkMonoBind Recursive dict_binds) result)
  where
	-- I wonder if we should do these one at at time
	-- Consider	?x = 4
	--		?y = ?x + 1
    tc_ip_bind (ip, expr)
      = newTyVarTy openTypeKind		`thenM` \ ty ->
  	getSrcLocM			`thenM` \ loc ->
  	newIPDict (IPBind ip) ip ty	`thenM` \ (ip', ip_inst) ->
  	tcCheckRho expr ty		`thenM` \ expr' ->
  	returnM (ip_inst, (ip', expr'))

tc_binds_and_then top_lvl combiner (MonoBind bind sigs is_rec) do_next
  =   	-- BRING ANY SCOPED TYPE VARIABLES INTO SCOPE
	-- Notice that they scope over 
	--	a) the type signatures in the binding group
	--	b) the bindings in the group
	--	c) the scope of the binding group (the "in" part)
      tcAddScopedTyVars (collectSigTysFromMonoBinds bind)	$

      tcBindWithSigs top_lvl bind sigs is_rec	`thenM` \ (poly_binds, poly_ids) ->
  
      case top_lvl of
	TopLevel	-- For the top level don't bother will all this
			--  bindInstsOfLocalFuns stuff. All the top level 
			-- things are rec'd together anyway, so it's fine to
			-- leave them to the tcSimplifyTop, and quite a bit faster too
			--
			-- Subtle (and ugly) point: furthermore at top level we
			-- return the TcLclEnv, which contains the LIE var; we
			-- don't want to return the wrong one!
		-> tc_body poly_ids			`thenM` \ (prag_binds, thing) ->
		   returnM (combiner (mkMonoBind Recursive (poly_binds `andMonoBinds` prag_binds)) 
				     thing)

	NotTopLevel	-- For nested bindings we must do teh bindInstsOfLocalFuns thing
		-> getLIE (tc_body poly_ids)		`thenM` \ ((prag_binds, thing), lie) ->

			-- Create specialisations of functions bound here
		    bindInstsOfLocalFuns lie poly_ids	`thenM` \ lie_binds ->

			-- We want to keep non-recursive things non-recursive
			-- so that we desugar unlifted bindings correctly
		   if isRec is_rec then
		     returnM (
			combiner (mkMonoBind Recursive (
				poly_binds `andMonoBinds`
				lie_binds  `andMonoBinds`
				prag_binds)) thing
		     )
		   else
		     returnM (
			combiner (mkMonoBind NonRecursive poly_binds) $
			combiner (mkMonoBind NonRecursive prag_binds) $
			combiner (mkMonoBind Recursive lie_binds)     $
				-- NB: the binds returned by tcSimplify and bindInstsOfLocalFuns
				-- aren't guaranteed in dependency order (though we could change
				-- that); hence the Recursive marker.
			thing)
  where
    tc_body poly_ids 	-- Type check the pragmas and "thing inside"
      =   -- Extend the environment to bind the new polymorphic Ids
	  tcExtendLocalValEnv poly_ids	$
  
	  -- Build bindings and IdInfos corresponding to user pragmas
	  tcSpecSigs sigs		`thenM` \ prag_binds ->

	  -- Now do whatever happens next, in the augmented envt
	  do_next			`thenM` \ thing ->

	  returnM (prag_binds, thing)
\end{code}


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
	-> [RenamedSig]		-- Used solely to get INLINE, NOINLINE sigs
	-> RecFlag
	-> TcM (TcMonoBinds, [TcId])

tcBindWithSigs top_lvl mbind sigs is_rec
  = 	-- TYPECHECK THE SIGNATURES
     recoverM (returnM []) (
	mappM tcTySig [sig | sig@(Sig name _ _) <- sigs]
     )						`thenM` \ tc_ty_sigs ->

	-- SET UP THE MAIN RECOVERY; take advantage of any type sigs
   recoverM (
	-- If typechecking the binds fails, then return with each
	-- signature-less binder given type (forall a.a), to minimise subsequent
	-- error messages
	newTyVar liftedTypeKind		`thenM` \ alpha_tv ->
	let
	  forall_a_a    = mkForAllTy alpha_tv (mkTyVarTy alpha_tv)
          binder_names  = collectMonoBinders mbind
	  poly_ids      = map mk_dummy binder_names
	  mk_dummy name = case maybeSig tc_ty_sigs name of
			    Just sig -> tcSigPolyId sig			-- Signature
			    Nothing  -> mkLocalId name forall_a_a      	-- No signature
	in
	traceTc (text "tcBindsWithSigs: error recovery" <+> ppr binder_names) 	`thenM_`
	returnM (EmptyMonoBinds, poly_ids)
    )						$

	-- TYPECHECK THE BINDINGS
    getLIE (tcMonoBinds mbind tc_ty_sigs is_rec)	`thenM` \ ((mbind', bndr_names_w_ids), lie_req) ->
    let
	(binder_names, mono_ids) = unzip (bagToList bndr_names_w_ids)
	tau_tvs = foldr (unionVarSet . tyVarsOfType . idType) emptyVarSet mono_ids
    in

	-- GENERALISE
	--	(it seems a bit crude to have to do getLIE twice,
	--	 but I can't see a better way just now)
    addSrcLoc  (minimum (map getSrcLoc binder_names))		$
    addErrCtxt (genCtxt binder_names)				$
    getLIE (generalise binder_names mbind tau_tvs lie_req tc_ty_sigs)
			`thenM` \ ((tc_tyvars_to_gen, dict_binds, dict_ids), lie_free) ->


	-- ZONK THE GENERALISED TYPE VARIABLES TO REAL TyVars
	-- This commits any unbound kind variables to boxed kind, by unification
	-- It's important that the final quanfified type variables
	-- are fully zonked, *including boxity*, because they'll be 
	-- included in the forall types of the polymorphic Ids.
	-- At calls of these Ids we'll instantiate fresh type variables from
	-- them, and we use their boxity then.
    mappM zonkTcTyVarToTyVar tc_tyvars_to_gen	`thenM` \ real_tyvars_to_gen ->

	-- ZONK THE Ids
	-- It's important that the dict Ids are zonked, including the boxity set
	-- in the previous step, because they are later used to form the type of 
	-- the polymorphic thing, and forall-types must be zonked so far as 
	-- their bound variables are concerned
    mappM zonkId dict_ids				`thenM` \ zonked_dict_ids ->
    mappM zonkId mono_ids				`thenM` \ zonked_mono_ids ->

	-- BUILD THE POLYMORPHIC RESULT IDs
    let
	exports  = zipWith mk_export binder_names zonked_mono_ids
	poly_ids = [poly_id | (_, poly_id, _) <- exports]
	dict_tys = map idType zonked_dict_ids

	inlines    = mkNameSet [name | InlineSig True name _ loc <- sigs]
			-- Any INLINE sig (regardless of phase control) 
			-- makes the RHS look small
        inline_phases = listToFM [(name, phase) | InlineSig _ name phase _ <- sigs, 
						  not (isAlwaysActive phase)]
			-- Set the IdInfo field to control the inline phase
			-- AlwaysActive is the default, so don't bother with them

	mk_export binder_name zonked_mono_id
	  = (tyvars, 
	     attachInlinePhase inline_phases poly_id,
	     zonked_mono_id)
	  where
	    (tyvars, poly_id) = 
		case maybeSig tc_ty_sigs binder_name of
		  Just (TySigInfo sig_poly_id sig_tyvars _ _ _ _ _) -> 
			(sig_tyvars, sig_poly_id)
		  Nothing -> (real_tyvars_to_gen, new_poly_id)

	    new_poly_id = mkLocalId binder_name poly_ty
	    poly_ty = mkForAllTys real_tyvars_to_gen
		    $ mkFunTys dict_tys 
		    $ idType zonked_mono_id
		-- It's important to build a fully-zonked poly_ty, because
		-- we'll slurp out its free type variables when extending the
		-- local environment (tcExtendLocalValEnv); if it's not zonked
		-- it appears to have free tyvars that aren't actually free 
		-- at all.
    in

    traceTc (text "binding:" <+> ppr ((zonked_dict_ids, dict_binds),
				      exports, map idType poly_ids)) `thenM_`

	-- Check for an unlifted, non-overloaded group
	-- In that case we must make extra checks
    if any (isUnLiftedType . idType) zonked_mono_ids && null zonked_dict_ids 
    then	-- Some bindings are unlifted
	checkUnliftedBinds top_lvl is_rec real_tyvars_to_gen mbind	`thenM_` 
	
	extendLIEs lie_req			`thenM_`
	returnM (
	    AbsBinds [] [] exports inlines mbind',
		-- Do not generate even any x=y bindings
	    poly_ids
        )

    else	-- The normal case
    extendLIEs lie_free				`thenM_`
    returnM (
	AbsBinds real_tyvars_to_gen
		 zonked_dict_ids
		 exports
		 inlines
		 (dict_binds `andMonoBinds` mbind'),
	poly_ids
    )

attachInlinePhase inline_phases bndr
  = case lookupFM inline_phases (idName bndr) of
	Just prag -> bndr `setInlinePragma` prag
	Nothing   -> bndr

-- Check that non-overloaded unlifted bindings are
-- 	a) non-recursive,
--	b) not top level, 
--	c) non-polymorphic
--	d) not a multiple-binding group (more or less implied by (a))

checkUnliftedBinds top_lvl is_rec real_tyvars_to_gen mbind
  = ASSERT( not (any ((eqKind unliftedTypeKind) . tyVarKind) real_tyvars_to_gen) )
		-- The instCantBeGeneralised stuff in tcSimplify should have
		-- already raised an error if we're trying to generalise an 
		-- unboxed tyvar (NB: unboxed tyvars are always introduced 
		-- along with a class constraint) and it's better done there 
		-- because we have more precise origin information.
		-- That's why we just use an ASSERT here.

    checkTc (isNotTopLevel top_lvl)
	    (unliftedBindErr "Top-level" mbind)		`thenM_`
    checkTc (isNonRec is_rec)
	    (unliftedBindErr "Recursive" mbind)		`thenM_`
    checkTc (single_bind mbind)
    	    (unliftedBindErr "Multiple" mbind)		`thenM_`
    checkTc (null real_tyvars_to_gen)
    	    (unliftedBindErr "Polymorphic" mbind)

  where
    single_bind (PatMonoBind _ _ _)   = True
    single_bind (FunMonoBind _ _ _ _) = True
    single_bind other		      = False
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

\begin{code}
generalise binder_names mbind tau_tvs lie_req sigs =

  -- check for -fno-monomorphism-restriction
  doptM Opt_NoMonomorphismRestriction		`thenM` \ no_MR ->
  let is_unrestricted | no_MR 	  = True
		      | otherwise = isUnRestrictedGroup tysig_names mbind
  in

  if not is_unrestricted then 	-- RESTRICTED CASE
   	-- Check signature contexts are empty 
    checkTc (all is_mono_sig sigs)
	    (restrictedBindCtxtErr binder_names)	`thenM_`

	-- Now simplify with exactly that set of tyvars
	-- We have to squash those Methods
    tcSimplifyRestricted doc tau_tvs lie_req		`thenM` \ (qtvs, binds) ->

   	-- Check that signature type variables are OK
    checkSigsTyVars qtvs sigs				`thenM` \ final_qtvs ->

    returnM (final_qtvs, binds, [])

  else if null sigs then	-- UNRESTRICTED CASE, NO TYPE SIGS
    tcSimplifyInfer doc tau_tvs lie_req

  else 				-- UNRESTRICTED CASE, WITH TYPE SIGS
   	-- CHECKING CASE: Unrestricted group, there are type signatures
	-- Check signature contexts are identical
    checkSigsCtxts sigs			`thenM` \ (sig_avails, sig_dicts) ->
    
	-- Check that the needed dicts can be
	-- expressed in terms of the signature ones
    tcSimplifyInferCheck doc tau_tvs sig_avails lie_req	`thenM` \ (forall_tvs, dict_binds) ->
	
   	-- Check that signature type variables are OK
    checkSigsTyVars forall_tvs sigs			`thenM` \ final_qtvs ->

    returnM (final_qtvs, dict_binds, sig_dicts)

  where
    tysig_names = map (idName . tcSigPolyId) sigs
    is_mono_sig (TySigInfo _ _ theta _ _ _ _) = null theta

    doc = ptext SLIT("type signature(s) for") <+> pprBinders binder_names

-----------------------
	-- CHECK THAT ALL THE SIGNATURE CONTEXTS ARE UNIFIABLE
	-- The type signatures on a mutually-recursive group of definitions
	-- must all have the same context (or none).
	--
	-- We unify them because, with polymorphic recursion, their types
	-- might not otherwise be related.  This is a rather subtle issue.
	-- ToDo: amplify
checkSigsCtxts sigs@(TySigInfo id1 sig_tvs theta1 _ _ _ src_loc : other_sigs)
  = addSrcLoc src_loc			$
    mappM_ check_one other_sigs		`thenM_` 
    if null theta1 then
	returnM ([], [])		-- Non-overloaded type signatures
    else
    newDicts SignatureOrigin theta1	`thenM` \ sig_dicts ->
    let
	-- The "sig_avails" is the stuff available.  We get that from
	-- the context of the type signature, BUT ALSO the lie_avail
	-- so that polymorphic recursion works right (see comments at end of fn)
	sig_avails = sig_dicts ++ sig_meths
    in
    returnM (sig_avails, map instToId sig_dicts)
  where
    sig1_dict_tys = map mkPredTy theta1
    sig_meths 	  = concat [insts | TySigInfo _ _ _ _ _ insts _ <- sigs]

    check_one sig@(TySigInfo id _ theta _ _ _ _)
       = addErrCtxt (sigContextsCtxt id1 id)			$
	 checkTc (equalLength theta theta1) sigContextsErr	`thenM_`
	 unifyTauTyLists sig1_dict_tys (map mkPredTy theta)

checkSigsTyVars :: [TcTyVar] -> [TcSigInfo] -> TcM [TcTyVar]
checkSigsTyVars qtvs sigs 
  = mappM check_one sigs	`thenM` \ sig_tvs_s ->
    let
	-- Sigh.  Make sure that all the tyvars in the type sigs
	-- appear in the returned ty var list, which is what we are
	-- going to generalise over.  Reason: we occasionally get
	-- silly types like
	--	type T a = () -> ()
	--	f :: T a
	--	f () = ()
	-- Here, 'a' won't appear in qtvs, so we have to add it

 	sig_tvs = foldr (unionVarSet . mkVarSet) emptyVarSet sig_tvs_s
	all_tvs = mkVarSet qtvs `unionVarSet` sig_tvs
    in
    returnM (varSetElems all_tvs)
  where
    check_one (TySigInfo id sig_tyvars sig_theta sig_tau _ _ src_loc)
      = addSrcLoc src_loc						$
	addErrCtxt (ptext SLIT("When checking the type signature for") 
		      <+> quotes (ppr id))				$
	addErrCtxtM (sigCtxt id sig_tyvars sig_theta sig_tau)		$
	checkSigTyVarsWrt (idFreeTyVars id) sig_tyvars
\end{code}

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
  [NOTE: Jan 2001: I don't understand the problem here so I'm doing 
	the simple thing instead]

 (b) On the other hand, we mustn't generalise tyvars which are constrained,
	because we are going to pass on out the unmodified LIE, with those
	tyvars in it.  They won't be in scope if we've generalised them.

So we are careful, and do a complete simplification just to find the
constrained tyvars. We don't use any of the results, except to
find which tyvars are constrained.

\begin{code}
isUnRestrictedGroup :: [Name]		-- Signatures given for these
		    -> RenamedMonoBinds
		    -> Bool

is_elem v vs = isIn "isUnResMono" v vs

isUnRestrictedGroup sigs (PatMonoBind other        _ _) = False
isUnRestrictedGroup sigs (VarMonoBind v _)	        = v `is_elem` sigs
isUnRestrictedGroup sigs (FunMonoBind v _ matches _)	= isUnRestrictedMatch matches || 
							  v `is_elem` sigs
isUnRestrictedGroup sigs (AndMonoBinds mb1 mb2)		= isUnRestrictedGroup sigs mb1 &&
							  isUnRestrictedGroup sigs mb2
isUnRestrictedGroup sigs EmptyMonoBinds			= True

isUnRestrictedMatch (Match [] _ _ : _) = False	-- No args => like a pattern binding
isUnRestrictedMatch other	       = True	-- Some args => a function binding
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
	    -> [TcSigInfo] -> RecFlag
	    -> TcM (TcMonoBinds, 
		    Bag (Name,		-- Bound names
		  	 TcId))		-- Corresponding monomorphic bound things

tcMonoBinds mbinds tc_ty_sigs is_rec
   	-- Three stages: 
	-- 1. Check the patterns, building up an environment binding
	--    the variables in this group (in the recursive case)
	-- 2. Extend the environment
	-- 3. Check the RHSs
  = tc_mb_pats mbinds		`thenM` \ (complete_it, xve) ->
    tcExtendLocalValEnv2 (bagToList xve) complete_it
  where
    tc_mb_pats EmptyMonoBinds 
      = returnM (returnM (EmptyMonoBinds, emptyBag), emptyBag)

    tc_mb_pats (AndMonoBinds mb1 mb2)
      = tc_mb_pats mb1		`thenM` \ (complete_it1, xve1) ->
        tc_mb_pats mb2		`thenM` \ (complete_it2, xve2) ->
	let
	   complete_it = complete_it1 	`thenM` \ (mb1', bs1) ->
			 complete_it2 	`thenM` \ (mb2', bs2) ->
			 returnM (AndMonoBinds mb1' mb2', bs1 `unionBags` bs2)
	in
	returnM (complete_it, xve1 `unionBags` xve2)

    tc_mb_pats (FunMonoBind name inf matches locn)
		-- Three cases:
		--	a) Type sig supplied
		--	b) No type sig and recursive
		--	c) No type sig and non-recursive

      | Just sig <- maybeSig tc_ty_sigs name 
      = let	-- (a) There is a type signature
		-- Use it for the environment extension, and check
		-- the RHS has the appropriate type (with outer for-alls stripped off)
	   mono_id = tcSigMonoId sig
	   mono_ty = idType mono_id
	   complete_it = addSrcLoc locn					$
			 tcMatchesFun name matches (Check mono_ty)	`thenM` \ matches' ->
			 returnM (FunMonoBind mono_id inf matches' locn, 
				  unitBag (name, mono_id))
	in
	returnM (complete_it, if isRec is_rec then unitBag (name,tcSigPolyId sig) 
					      else emptyBag)

      | isRec is_rec
      =		-- (b) No type signature, and recursive
      		-- So we must use an ordinary H-M type variable
		-- which means the variable gets an inferred tau-type
	newLocalName name		`thenM` \ mono_name ->
	newTyVarTy openTypeKind		`thenM` \ mono_ty ->
	let
	   mono_id     = mkLocalId mono_name mono_ty
	   complete_it = addSrcLoc locn					$
			 tcMatchesFun name matches (Check mono_ty)	`thenM` \ matches' ->
			 returnM (FunMonoBind mono_id inf matches' locn, 
				  unitBag (name, mono_id))
	in
	returnM (complete_it, unitBag (name, mono_id))

      | otherwise	-- (c) No type signature, and non-recursive
      =	let		-- So we can use a 'hole' type to infer a higher-rank type
	   complete_it 
		= addSrcLoc locn				$
		  newHole	 				`thenM` \ hole -> 
		  tcMatchesFun name matches (Infer hole)	`thenM` \ matches' ->
		  readMutVar hole				`thenM` \ fun_ty ->
	  	  newLocalName name				`thenM` \ mono_name ->
		  let
		     mono_id = mkLocalId mono_name fun_ty
		  in
		  returnM (FunMonoBind mono_id inf matches' locn, 
			   unitBag (name, mono_id))
	in
	returnM (complete_it, emptyBag)
	
    tc_mb_pats bind@(PatMonoBind pat grhss locn)
      = addSrcLoc locn	 	$

		-- 	Now typecheck the pattern
		-- We do now support binding fresh (not-already-in-scope) scoped 
		-- type variables in the pattern of a pattern binding.  
		-- For example, this is now legal:
		--	(x::a, y::b) = e
		-- The type variables are brought into scope in tc_binds_and_then,
		-- so we don't have to do anything here.

	newHole 				`thenM` \ hole -> 
	tcPat tc_pat_bndr pat (Infer hole)	`thenM` \ (pat', tvs, ids, lie_avail) ->
	readMutVar hole				`thenM` \ pat_ty ->

	-- Don't know how to deal with pattern-bound existentials yet
        checkTc (isEmptyBag tvs && null lie_avail) 
	        (existentialExplode bind)	`thenM_` 

	let
	   complete_it = addSrcLoc locn		 			$
			 addErrCtxt (patMonoBindsCtxt bind)		$
			 tcGRHSsPat grhss (Check pat_ty)	`thenM` \ grhss' ->
			 returnM (PatMonoBind pat' grhss' locn, ids)
	in
	returnM (complete_it, if isRec is_rec then ids else emptyBag)

	-- tc_pat_bndr is used when dealing with a LHS binder in a pattern.
	-- If there was a type sig for that Id, we want to make it much
	-- as if that type signature had been on the binder as a SigPatIn.
	-- We check for a type signature; if there is one, we use the mono_id
	-- from the signature.  This is how we make sure the tau part of the
	-- signature actually matches the type of the LHS; then tc_mb_pats
	-- ensures the LHS and RHS have the same type
	
    tc_pat_bndr name pat_ty
	= case maybeSig tc_ty_sigs name of
	    Nothing  -> newLocalName name			`thenM` \ bndr_name ->
		        tcMonoPatBndr bndr_name pat_ty

	    Just sig -> addSrcLoc (getSrcLoc name)		$
			tcSubPat (idType mono_id) pat_ty	`thenM` \ co_fn ->
			returnM (co_fn, mono_id)
		     where
			mono_id = tcSigMonoId sig
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
tcSpecSigs :: [RenamedSig] -> TcM TcMonoBinds
tcSpecSigs (SpecSig name poly_ty src_loc : sigs)
  = 	-- SPECIALISE f :: forall b. theta => tau  =  g
    addSrcLoc src_loc		 		$
    addErrCtxt (valSpecSigCtxt name poly_ty)	$

	-- Get and instantiate its alleged specialised type
    tcHsSigType (FunSigCtxt name) poly_ty	`thenM` \ sig_ty ->

	-- Check that f has a more general type, and build a RHS for
	-- the spec-pragma-id at the same time
    getLIE (tcCheckSigma (HsVar name) sig_ty)	`thenM` \ (spec_expr, spec_lie) ->

	-- Squeeze out any Methods (see comments with tcSimplifyToDicts)
    tcSimplifyToDicts spec_lie			`thenM` \ spec_binds ->

    	-- Just specialise "f" by building a SpecPragmaId binding
	-- It is the thing that makes sure we don't prematurely 
	-- dead-code-eliminate the binding we are really interested in.
    newLocalName name			`thenM` \ spec_name ->
    let
	spec_bind = VarMonoBind (mkSpecPragmaId spec_name sig_ty)
		   		(mkHsLet spec_binds spec_expr)
    in

	-- Do the rest and combine
    tcSpecSigs sigs			`thenM` \ binds_rest ->
    returnM (binds_rest `andMonoBinds` spec_bind)

tcSpecSigs (other_sig : sigs) = tcSpecSigs sigs
tcSpecSigs []		      = returnM EmptyMonoBinds
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
sigContextsErr = ptext SLIT("Mismatched contexts")

sigContextsCtxt s1 s2
  = vcat [ptext SLIT("When matching the contexts of the signatures for"), 
	  nest 2 (vcat [ppr s1 <+> dcolon <+> ppr (idType s1),
			ppr s2 <+> dcolon <+> ppr (idType s2)]),
	  ptext SLIT("The signature contexts in a mutually recursive group should all be identical")]

-----------------------------------------------
unliftedBindErr flavour mbind
  = hang (text flavour <+> ptext SLIT("bindings for unlifted types aren't allowed:"))
	 4 (ppr mbind)

-----------------------------------------------
existentialExplode mbinds
  = hang (vcat [text "My brain just exploded.",
	        text "I can't handle pattern bindings for existentially-quantified constructors.",
		text "In the binding group"])
	4 (ppr mbinds)

-----------------------------------------------
restrictedBindCtxtErr binder_names
  = hang (ptext SLIT("Illegal overloaded type signature(s)"))
       4 (vcat [ptext SLIT("in a binding group for") <+> pprBinders binder_names,
		ptext SLIT("that falls under the monomorphism restriction")])

genCtxt binder_names
  = ptext SLIT("When generalising the type(s) for") <+> pprBinders binder_names

-- Used in error messages
-- Use quotes for a single one; they look a bit "busy" for several
pprBinders [bndr] = quotes (ppr bndr)
pprBinders bndrs  = pprWithCommas ppr bndrs
\end{code}
