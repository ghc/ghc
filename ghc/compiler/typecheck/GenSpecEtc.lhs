%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[GenSpecEtc]{Code for GEN, SPEC, PRED, and REL}

\begin{code}
#include "HsVersions.h"

module GenSpecEtc (
	genBinds, SignatureInfo(..),
	checkSigTyVars,

	-- and to make the interface self-sufficient...
	Bag, E, Bind, Binds, TypecheckedPat, Id, Inst,
	LIE, TcResult, Name, SrcLoc, Subst, TyVar, UniType,
	UniqueSupply, Error(..), Pretty(..), PprStyle,
	PrettyRep
    ) where

import TcMonad		-- typechecker monadery
import TcMonadFns	( applyTcSubstAndCollectTyVars,
			  mkIdsWithGivenTys
			)
import AbsSyn

import AbsUniType
import E		( tvOfE, E, LVE(..), TCE(..), UniqFM, CE(..) )
			-- TCE and CE for pragmas only
import Errors
import Id		( getIdUniType, mkInstId, Id, DictVar(..) )
import IdInfo
import Inst
import LIE		( mkLIE, unMkLIE, LIE )
import ListSetOps	( minusList, unionLists, intersectLists )
import Maybes		( assocMaybe, Maybe(..) )
import Name		( Name(..) )	-- ToDo: a HACK
import TcSimplify	( tcSimplify, tcSimplifyAndCheck )
import Util

IMPORT_Trace		-- ToDo: rm (debugging)
import Pretty		-- 
\end{code}

%************************************************************************
%*									*
\subsection[Gen-SignatureInfo]{The @SignatureInfo@ type}
%*									*
%************************************************************************

A type signature (or user-pragma) is typechecked to produce a
@SignatureInfo@.

\begin{code}
data SignatureInfo
  = TySigInfo	    Id	-- for this value...
		    [TyVar] [Inst] TauType
		    SrcLoc

  | ValSpecInfo	    Name	-- we'd rather have the Name than Id...
		    UniType
		    (Maybe Name)
		    SrcLoc

  | ValInlineInfo   Name
		    UnfoldingGuidance
		    SrcLoc

  | ValDeforestInfo Name
                    SrcLoc

  | ValMagicUnfoldingInfo
		    Name
		    FAST_STRING
		    SrcLoc

  -- ToDo: perhaps add more (for other user pragmas)
\end{code}


%************************************************************************
%*									*
\subsection[Gen-GEN]{Generalising bindings}
%*									*
%************************************************************************

\begin{code}
genBinds :: Bool				-- True <=> top level
	 -> E 
	 -> TypecheckedBind 
	 -> LIE 				-- LIE from typecheck of binds
	 -> LVE					-- Local types
	 -> [SignatureInfo]			-- Signatures, if any
	 -> TcM (TypecheckedBinds, LIE, LVE)	-- Generalised binds, reduced LIE,
						--	polymorphic LVE
						-- The LVE and LIE are fixed points
						-- of the substitution
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
genBinds top_level e bind lie lve sigs
  = getSrcLocTc					`thenNF_Tc` \ locn ->

	-- GET TYPE VARIABLES FREE IN ENV
    applyTcSubstAndCollectTyVars (tvOfE e)	`thenNF_Tc` \ free_tyvars ->

	-- CHECK THAT THE SIGNATURES MATCH
	-- Doesn't affect substitution
    mapTc (checkSigMatch free_tyvars) sigs	`thenTc_`

	 -- UNPACK THE LVE
    let
	(bound_var_names, bound_var_locals) = unzip lve
	bound_var_types = map getIdUniType bound_var_locals
    in
    applyTcSubstToTys bound_var_types `thenNF_Tc`	\ bound_var_types' ->
    let
	mentioned_tyvars' = extractTyVarsFromTys bound_var_types'

	 -- COMPUTE VARIABLES OVER WHICH TO QUANTIFY, namely tyvars_to_gen
	tyvars_to_gen = mentioned_tyvars' `minusList` free_tyvars

	-- UNSCRAMBLE "sigs" INTO VARIOUS FLAVOURS
	-- AND SNAFFLE ANY "IdInfos" FOR VARS HERE

	(ty_sigs, upragmas) = partition is_tysig_info sigs
	inline_sigs 	    = filter is_inline_info   upragmas
	deforest_sigs 	    = filter is_deforest_info upragmas
	magic_uf_sigs	    = filter is_magic_uf_info upragmas
	spec_sigs   	    = filter is_spec_info     upragmas

	unfold_me_fn n
	  = case [ x | x@(ValInlineInfo v _ _) <- inline_sigs, v == n ] of
	      (ValInlineInfo _ guide _ :_) -> iWantToBeINLINEd guide
	      [] ->
		case [ x | x@(ValMagicUnfoldingInfo v _ _) <- magic_uf_sigs, v == n ] of
		  (ValMagicUnfoldingInfo _ str _:_) -> mkMagicUnfolding str
		  []			        -> noInfo_UF

        deforest_me_fn n
          = case [ x | x@(ValDeforestInfo v _) <- deforest_sigs, v == n ] of
            (ValDeforestInfo _ _ : _) -> DoDeforest
            [] -> Don'tDeforest

	id_info_for n
	  = noIdInfo
	      `addInfo_UF` (unfold_me_fn   n)
              `addInfo`    (deforest_me_fn n)

	id_infos = [ id_info_for n | n <- bound_var_names ]
    in
    resolveOverloading top_level e free_tyvars tyvars_to_gen lie bind ty_sigs
		 `thenTc` \ (lie', reduced_tyvars_to_gen, dict_binds, dicts_bound) ->

	 -- BUILD THE NEW LOCALS
    let
	dict_tys = map getInstUniType dicts_bound

	envs_and_new_locals_types
	  = map (quantifyTy reduced_tyvars_to_gen . glueTyArgs dict_tys) bound_var_types'

	(_, new_locals_types) = unzip envs_and_new_locals_types
    in
	 -- The new_locals function is passed into genBinds
	 -- so it can generate top-level or non-top-level locals
    let
	lve_of_new_ids = mkIdsWithGivenTys bound_var_names new_locals_types id_infos
	new_ids = map snd lve_of_new_ids
    in
	 -- BUILD RESULTS
    returnTc (
--	pprTrace "Gen: " (ppSep [ppr PprDebug new_ids, 
--				 ppStr "; to gen ", ppr PprDebug tyvars_to_gen,
--				 ppStr "; reduced ", ppr PprDebug reduced_tyvars_to_gen
--		]) $
	 AbsBinds reduced_tyvars_to_gen (map mkInstId dicts_bound)
		 (bound_var_locals `zip` new_ids)
		 dict_binds bind,
	 lie',
	 lve_of_new_ids
    )
  where
    is_tysig_info (TySigInfo _ _ _ _ _) = True
    is_tysig_info _ 	    	    	= False

    is_inline_info (ValInlineInfo _ _ _) = True
    is_inline_info _ = False

    is_deforest_info (ValDeforestInfo _ _) = True
    is_deforest_info _ = False
 
    is_magic_uf_info (ValMagicUnfoldingInfo _ _ _) = True
    is_magic_uf_info _ = False

    is_spec_info (ValSpecInfo _ _ _ _) = True
    is_spec_info _ = False
\end{code}


\begin{code}
resolveOverloading 
	:: Bool			-- True <=> top level
	-> E
	-> [TyVar]		-- Tyvars free in E
	-> [TyVar]		-- Tyvars over which we are going to generalise
	-> LIE			-- The LIE to deal with
	-> TypecheckedBind	-- The binding group
	-> [SignatureInfo]	-- And its real type-signature information
	-> TcM (LIE,			-- LIE to pass up the way; a fixed point of
					-- the current substitution
	    	[TyVar],		-- Revised tyvars to generalise
	    	[(Inst, TypecheckedExpr)],-- Dict bindings
	    	[Inst])			-- List of dicts to bind here
		       
resolveOverloading top_level e free_tyvars tyvars_to_gen lie bind ty_sigs
  = let
	dicts = unMkLIE lie
    in
	 -- DEAL WITH MONOMORPHISM RESTRICTION
    if (not (isUnRestrictedGroup tysig_vars bind)) then 

	-- Restricted group, so bind no dictionaries, and
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

	-- Find all the type variables involved in overloading 
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

	tcSimplify top_level free_tyvars tyvars_to_gen dicts
				    `thenTc` \ (_, _, dicts_sig) ->

-- ASSERT: tcSimplify has already applied subst to its results
-- (WDP/SLPJ 95/07)
--	applyTcSubstToInsts dicts_sig	`thenNF_Tc` 	\ dicts_sig' ->
	let
	  constrained_tyvars
	    = foldr (unionLists . extractTyVarsFromInst) [] dicts_sig

	  reduced_tyvars_to_gen = tyvars_to_gen `minusList` constrained_tyvars

	  increased_free_tyvars = free_tyvars `unionLists` constrained_tyvars
	in

	-- Do it again, but with increased_free_tyvars/reduced_tyvars_to_gen:

	tcSimplify top_level increased_free_tyvars reduced_tyvars_to_gen dicts
				    `thenTc` \ (dicts_free, dicts_binds, dicts_sig2) ->
--NB: still no applyTcSubstToInsts

--	pprTrace "resolve:" (ppCat [ppr PprDebug free_tyvars, ppr PprDebug tyvars_to_gen, ppr PprDebug constrained_tyvars, ppr PprDebug reduced_tyvars_to_gen, ppr PprDebug bind]) $
	returnTc (mkLIE (dicts_free++dicts_sig2), -- All these are left unbound
		  reduced_tyvars_to_gen, 
		  dicts_binds, 			-- Local dict binds
		  [])				-- No lambda-bound dicts

		-- The returned LIE should be a fixed point of the substitution

    else -- Unrestricted group
       case ty_sigs of
	 [] ->	-- NO TYPE SIGNATURES

	    tcSimplify top_level free_tyvars tyvars_to_gen dicts
				    `thenTc` \ (dicts_free, dict_binds, dicts_sig) ->
	    returnTc (mkLIE dicts_free, tyvars_to_gen, dict_binds, dicts_sig)

	 other -> -- TYPE SIGNATURES PRESENT!

		-- Check that all the signature contexts are identical
		-- "tysig_dicts_s" is a list (one for each id declared
		-- in this group) of lists of dicts (the list
		-- corresponds to the context in the sig).
		-- "dicts_sig" is just the first such list; we match
		-- it against all the others.

	    mapNF_Tc applyTcSubstToInsts tysig_dicts_s
				`thenNF_Tc` \ (dicts_sig : other_dicts_s) ->

	    checkTc (not (all (same_dicts dicts_sig) other_dicts_s))
		-- The type signatures on a mutually-recursive group of definitions
		-- must all have the same context (or none).  See Errors.lhs.
		(sigContextsErr ty_sigs) `thenTc_`

		    -- Check that the needed dicts can be expressed in
		    -- terms of the signature ones
	    tcSimplifyAndCheck
		top_level
		free_tyvars 	-- Vars free in the environment
		tyvars_to_gen 	-- Type vars over which we will quantify
		dicts_sig	-- Available dicts
		dicts		-- Want bindings for these dicts
		(BindSigCtxt tysig_vars)

				    `thenTc` \ (dicts_free, dict_binds) ->

	    returnTc (mkLIE dicts_free, tyvars_to_gen, dict_binds, dicts_sig)
  where
    tysig_dicts_s = [dicts   | (TySigInfo _       _ dicts _ _) <- ty_sigs]
    tysig_vars    = [sig_var | (TySigInfo sig_var _ _     _ _) <- ty_sigs] 

	-- same_dicts checks that (post substitution) all the type signatures
	-- constrain the same type variables in the same way
    same_dicts []   	[]  	 = True
    same_dicts []   	_   	 = False
    same_dicts _	[]  	 = False
    same_dicts (d1:d1s) (d2:d2s) = matchesInst d1 d2 && same_dicts d1s d2s

    -- don't use the old version, because zipWith will truncate
    -- the longer one!
    --OLD: same_dicts dicts1 dicts2 = and (zipWith matchesInst dicts1 dicts2)
\end{code}

@checkSigMatch@ does the next step in checking signature matching.
The tau-type part has already been unified.  What we do here is to
check that this unification has not over-constrained the (polymorphic)
type variables of the original signature type.

The error message here is somewhat unsatisfactory, but it'll do for
now (ToDo).

\begin{code}
checkSigMatch :: [TyVar]	-- Free in environment
	      -> SignatureInfo
	      -> TcM [TyVar]

checkSigMatch env_tyvars (TySigInfo name sig_tyvars _ tau_ty src_loc)
  = let
	inferred_ty = getIdUniType name
    in
    addSrcLocTc src_loc	(
    checkSigTyVars env_tyvars sig_tyvars tau_ty inferred_ty
		   (SigCtxt name tau_ty)
    )

checkSigMatch _ other_not_really_a_sig = returnTc []
\end{code}


%************************************************************************
%*									*
\subsection[GenEtc-monomorphism]{The monomorphism restriction}
%*									*
%************************************************************************

Not exported:

\begin{code}
isUnRestrictedGroup :: [Id]		-- Signatures given for these
		     -> TypecheckedBind
		     -> Bool

isUnRestrictedGroup sigs EmptyBind              = True
isUnRestrictedGroup sigs (NonRecBind monobinds) = isUnResMono sigs monobinds
isUnRestrictedGroup sigs (RecBind monobinds)    = isUnResMono sigs monobinds

is_elem = isIn "isUnResMono"

isUnResMono sigs EmptyMonoBinds = True
isUnResMono sigs (AndMonoBinds mb1 mb2) = isUnResMono sigs mb1 && isUnResMono sigs mb2
isUnResMono sigs (PatMonoBind (VarPat v) _ _)	= v `is_elem` sigs
isUnResMono sigs (PatMonoBind other      _ _)	= False
isUnResMono sigs (VarMonoBind v _)		= v `is_elem` sigs
isUnResMono sigs (FunMonoBind _ _ _)		= True
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

	(c) not mentioned in the environment
		eg the signature for f in this:

			g x = ... where
					f :: a->[a]
					f y = [x,y]
	
		Here, f is forced to be monorphic by the free occurence of x.

Before doing this, the substitution is applied to the signature type variable.

It's {\em assumed} that the substitution has already been applied to the 
environment type variables.

\begin{code}
checkSigTyVars :: [TyVar]	-- Tyvars free in environment; 
				-- fixed points of substitution
	       -> [TyVar]	-- The original signature type variables
	       -> UniType	-- signature type (for err msg)
	       -> UniType	-- inferred type (for err msg)
	       -> UnifyErrContext -- also for error msg
	       -> TcM [TyVar]	-- Post-substitution signature type variables

checkSigTyVars env_tyvars sig_tyvars sig_tau inferred_tau err_ctxt
  = getSrcLocTc					`thenNF_Tc` \ locn ->
    applyTcSubstToTy inferred_tau		`thenNF_Tc` \ inferred_tau' ->
    let
	match_err = badMatchErr sig_tau inferred_tau' err_ctxt locn
    in
    applyTcSubstToTyVars sig_tyvars	`thenNF_Tc` \ sig_tys ->

	 -- Check point (a) above
    checkMaybesTc (map getTyVarMaybe sig_tys) match_err	`thenTc` \ sig_tyvars' ->

	 -- Check point (b)
    checkTc (not (hasNoDups sig_tyvars')) match_err 	`thenTc_`

	-- Check point (c)
	-- We want to report errors in terms of the original signature tyvars, 
	-- ie sig_tyvars, NOT sig_tyvars'.  sig_tys and sig_tyvars' correspond
	-- 1-1 with sig_tyvars, so we can just map back.
    let
	is_elem = isIn "checkSigTyVars"

	mono_tyvars = [ sig_tyvar 
		      | (sig_tyvar,sig_tyvar') <- zipEqual sig_tyvars sig_tyvars',
			sig_tyvar' `is_elem` env_tyvars
		      ]
    in
    checkTc (not (null mono_tyvars))
	    (notAsPolyAsSigErr sig_tau mono_tyvars err_ctxt locn) `thenTc_`

    returnTc sig_tyvars'
\end{code}
