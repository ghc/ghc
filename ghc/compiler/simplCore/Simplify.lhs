%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[Simplify]{The main module of the simplifier}

\begin{code}
module Simplify ( simplTopBinds, simplExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( switchIsOn, opt_SimplDoEtaReduction,
			  opt_SimplNoPreInlining, 
			  dopt, DynFlag(Opt_D_dump_inlinings),
			  SimplifierSwitch(..)
			)
import SimplMonad
import SimplUtils	( mkCase, tryRhsTyLam, tryEtaExpansion,
			  simplBinder, simplBinders, simplRecIds, simplLetId,
			  SimplCont(..), DupFlag(..), mkStop, mkRhsStop,
			  contResultType, discardInline, countArgs, contIsDupable,
			  getContArgs, interestingCallContext, interestingArg, isStrictType
			)
import Var		( mkSysTyVar, tyVarKind, mustHaveLocalBinding )
import VarEnv
import Literal		( Literal )
import Id		( Id, idType, idInfo, isDataConId, hasNoBinding,
			  idUnfolding, setIdUnfolding, isExportedId, isDeadBinder,
			  idDemandInfo, setIdInfo,
			  idOccInfo, setIdOccInfo, 
			  zapLamIdInfo, setOneShotLambda, 
			)
import IdInfo		( OccInfo(..), isDeadOcc, isLoopBreaker,
			  setArityInfo, 
			  setUnfoldingInfo, atLeastArity,
			  occInfo
			)
import Demand		( isStrict )
import DataCon		( dataConNumInstArgs, dataConRepStrictness,
			  dataConSig, dataConArgTys
			)
import CoreSyn
import PprCore		( pprParendExpr, pprCoreExpr )
import CoreUnfold	( mkOtherCon, mkUnfolding, otherCons,
			  callSiteInline
			)
import CoreUtils	( cheapEqExpr, exprIsDupable, exprIsTrivial, 
			  exprIsConApp_maybe, mkPiType, findAlt, findDefault,
			  exprType, coreAltsType, exprIsValue, 
			  exprOkForSpeculation, exprArity, exprIsCheap,
			  mkCoerce, mkSCC, mkInlineMe, mkAltExpr
			)
import Rules		( lookupRule )
import CostCentre	( currentCCS )
import Type		( mkTyVarTys, isUnLiftedType, seqType,
			  mkFunTy, splitTyConApp_maybe, tyConAppArgs,
			  funResultTy, splitFunTy_maybe, splitFunTy
			)
import Subst		( mkSubst, substTy, substEnv, substExpr,
			  isInScope, lookupIdSubst, simplIdInfo
			)
import TyCon		( isDataTyCon, tyConDataConsIfAvailable	)
import TysPrim		( realWorldStatePrimTy )
import PrelInfo		( realWorldPrimId )
import OrdList
import Maybes		( maybeToBool )
import Util		( zipWithEqual )
import Outputable
\end{code}


The guts of the simplifier is in this module, but the driver
loop for the simplifier is in SimplCore.lhs.


-----------------------------------------
	*** IMPORTANT NOTE ***
-----------------------------------------
The simplifier used to guarantee that the output had no shadowing, but
it does not do so any more.   (Actually, it never did!)  The reason is
documented with simplifyArgs.




%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
simplTopBinds :: [InBind] -> SimplM [OutBind]

simplTopBinds binds
  = 	-- Put all the top-level binders into scope at the start
	-- so that if a transformation rule has unexpectedly brought
	-- anything into scope, then we don't get a complaint about that.
	-- It's rather as if the top-level binders were imported.
    simplRecIds (bindersOfBinds binds)	$ \ bndrs' -> 
    simpl_binds binds bndrs'		`thenSmpl` \ (binds', _) ->
    freeTick SimplifierDone		`thenSmpl_`
    returnSmpl (fromOL binds')
  where

	-- We need to track the zapped top-level binders, because
	-- they should have their fragile IdInfo zapped (notably occurrence info)
    simpl_binds []			  bs     = ASSERT( null bs ) returnSmpl (nilOL, panic "simplTopBinds corner")
    simpl_binds (NonRec bndr rhs : binds) (b:bs) = simplLazyBind True bndr  b rhs	(simpl_binds binds bs)
    simpl_binds (Rec pairs       : binds) bs     = simplRecBind  True pairs (take n bs) (simpl_binds binds (drop n bs))
					         where 
						   n = length pairs

simplRecBind :: Bool -> [(InId, InExpr)] -> [OutId]
	     -> SimplM (OutStuff a) -> SimplM (OutStuff a)
simplRecBind top_lvl pairs bndrs' thing_inside
  = go pairs bndrs'		`thenSmpl` \ (binds', (_, (binds'', res))) ->
    returnSmpl (unitOL (Rec (flattenBinds (fromOL binds'))) `appOL` binds'', res)
  where
    go [] _ = thing_inside 	`thenSmpl` \ stuff ->
	      returnOutStuff stuff
	
    go ((bndr, rhs) : pairs) (bndr' : bndrs')
	= simplLazyBind top_lvl bndr bndr' rhs (go pairs bndrs')
		-- Don't float unboxed bindings out,
		-- because we can't "rec" them
\end{code}


%************************************************************************
%*									*
\subsection[Simplify-simplExpr]{The main function: simplExpr}
%*									*
%************************************************************************

The reason for this OutExprStuff stuff is that we want to float *after*
simplifying a RHS, not before.  If we do so naively we get quadratic
behaviour as things float out.

To see why it's important to do it after, consider this (real) example:

	let t = f x
	in fst t
==>
	let t = let a = e1
		    b = e2
		in (a,b)
	in fst t
==>
	let a = e1
	    b = e2
	    t = (a,b)
	in
	a	-- Can't inline a this round, cos it appears twice
==>
	e1

Each of the ==> steps is a round of simplification.  We'd save a
whole round if we float first.  This can cascade.  Consider

	let f = g d
	in \x -> ...f...
==>
	let f = let d1 = ..d.. in \y -> e
	in \x -> ...f...
==>
	let d1 = ..d..
	in \x -> ...(\y ->e)...

Only in this second round can the \y be applied, and it 
might do the same again.


\begin{code}
simplExpr :: CoreExpr -> SimplM CoreExpr
simplExpr expr = getSubst	`thenSmpl` \ subst ->
		 simplExprC expr (mkStop (substTy subst (exprType expr)))
	-- The type in the Stop continuation is usually not used
	-- It's only needed when discarding continuations after finding
	-- a function that returns bottom.
	-- Hence the lazy substitution

simplExprC :: CoreExpr -> SimplCont -> SimplM CoreExpr
	-- Simplify an expression, given a continuation

simplExprC expr cont = simplExprF expr cont	`thenSmpl` \ (floats, (_, body)) ->
  		       returnSmpl (wrapFloats floats body)

simplExprF :: InExpr -> SimplCont -> SimplM OutExprStuff
	-- Simplify an expression, returning floated binds

simplExprF (Var v)	    cont = simplVar v cont
simplExprF (Lit lit)	    cont = simplLit lit cont
simplExprF expr@(Lam _ _)   cont = simplLam expr cont
simplExprF (Note note expr) cont = simplNote note expr cont

simplExprF (App fun arg) cont
  = getSubstEnv		`thenSmpl` \ se ->
    simplExprF fun (ApplyTo NoDup arg se cont)

simplExprF (Type ty) cont
  = ASSERT( case cont of { Stop _ _ -> True; ArgOf _ _ _ -> True; other -> False } )
    simplType ty	`thenSmpl` \ ty' ->
    rebuild (Type ty') cont

simplExprF (Case scrut bndr alts) cont
  = getSubstEnv			`thenSmpl` \ subst_env ->
    getSwitchChecker 		`thenSmpl` \ chkr ->
    if not (switchIsOn chkr NoCaseOfCase) then
	-- Simplify the scrutinee with a Select continuation
	simplExprF scrut (Select NoDup bndr alts subst_env cont)

    else
	-- If case-of-case is off, simply simplify the case expression
	-- in a vanilla Stop context, and rebuild the result around it
	simplExprC scrut (Select NoDup bndr alts subst_env 
				 (mkStop (contResultType cont)))	`thenSmpl` \ case_expr' ->
	rebuild case_expr' cont

simplExprF (Let (Rec pairs) body) cont
  = simplRecIds (map fst pairs) 		$ \ bndrs' -> 
	-- NB: bndrs' don't have unfoldings or spec-envs
	-- We add them as we go down, using simplPrags

    simplRecBind False pairs bndrs' (simplExprF body cont)

-- A non-recursive let is dealt with by simplNonRecBind
simplExprF (Let (NonRec bndr rhs) body) cont
  = getSubstEnv			`thenSmpl` \ se ->
    simplNonRecBind bndr rhs se (contResultType cont)	$
    simplExprF body cont


---------------------------------
simplType :: InType -> SimplM OutType
simplType ty
  = getSubst	`thenSmpl` \ subst ->
    let
	new_ty = substTy subst ty
    in
    seqType new_ty `seq`  
    returnSmpl new_ty

---------------------------------
simplLit :: Literal -> SimplCont -> SimplM OutExprStuff

simplLit lit (Select _ bndr alts se cont)
  = knownCon (Lit lit) (LitAlt lit) [] bndr alts se cont

simplLit lit cont = rebuild (Lit lit) cont
\end{code}


%************************************************************************
%*									*
\subsection{Lambdas}
%*									*
%************************************************************************

\begin{code}
simplLam fun cont
  = go fun cont
  where
    zap_it  = mkLamBndrZapper fun cont
    cont_ty = contResultType cont

      	-- Type-beta reduction
    go (Lam bndr body) (ApplyTo _ (Type ty_arg) arg_se body_cont)
      =	ASSERT( isTyVar bndr )
	tick (BetaReduction bndr)	`thenSmpl_`
	simplTyArg ty_arg arg_se	`thenSmpl` \ ty_arg' ->
	extendSubst bndr (DoneTy ty_arg')
	(go body body_cont)

	-- Ordinary beta reduction
    go (Lam bndr body) cont@(ApplyTo _ arg arg_se body_cont)
      = tick (BetaReduction bndr)			`thenSmpl_`
	simplNonRecBind zapped_bndr arg arg_se cont_ty
	(go body body_cont)
      where
	zapped_bndr = zap_it bndr

	-- Not enough args
    go lam@(Lam _ _) cont = completeLam [] lam cont

	-- Exactly enough args
    go expr cont = simplExprF expr cont

-- completeLam deals with the case where a lambda doesn't have an ApplyTo
-- continuation, so there are real lambdas left to put in the result

-- We try for eta reduction here, but *only* if we get all the 
-- way to an exprIsTrivial expression.    
-- We don't want to remove extra lambdas unless we are going 
-- to avoid allocating this thing altogether

completeLam rev_bndrs (Lam bndr body) cont
  = simplBinder bndr			$ \ bndr' ->
    completeLam (bndr':rev_bndrs) body cont

completeLam rev_bndrs body cont
  = simplExpr body 			`thenSmpl` \ body' ->
    case try_eta body' of
	Just etad_lam -> tick (EtaReduction (head rev_bndrs)) 	`thenSmpl_`
			 rebuild etad_lam cont

	Nothing	      -> rebuild (foldl (flip Lam) body' rev_bndrs) cont
  where
	-- We don't use CoreUtils.etaReduce, because we can be more
	-- efficient here:
	--  (a) we already have the binders,
	--  (b) we can do the triviality test before computing the free vars
	--	[in fact I take the simple path and look for just a variable]
	--  (c) we don't want to eta-reduce a data con worker or primop
	--      because we only have to eta-expand them later when we saturate
    try_eta body | not opt_SimplDoEtaReduction = Nothing
		 | otherwise		       = go rev_bndrs body

    go (b : bs) (App fun arg) | ok_arg b arg = go bs fun	-- Loop round
    go []       body          | ok_body body = Just body	-- Success!
    go _        _			     = Nothing		-- Failure!

    ok_body (Var v) = not (v `elem` rev_bndrs) && not (hasNoBinding v)
    ok_body other   = False
    ok_arg b arg    = varToCoreExpr b `cheapEqExpr` arg

mkLamBndrZapper :: CoreExpr 	-- Function
		-> SimplCont	-- The context
		-> Id -> Id	-- Use this to zap the binders
mkLamBndrZapper fun cont
  | n_args >= n_params fun = \b -> b		-- Enough args
  | otherwise		   = \b -> zapLamIdInfo b
  where
	-- NB: we count all the args incl type args
	-- so we must count all the binders (incl type lambdas)
    n_args = countArgs cont

    n_params (Note _ e) = n_params e
    n_params (Lam b e)  = 1 + n_params e
    n_params other	= 0::Int
\end{code}


%************************************************************************
%*									*
\subsection{Notes}
%*									*
%************************************************************************

\begin{code}
simplNote (Coerce to from) body cont
  = getInScope			`thenSmpl` \ in_scope ->
    let
	addCoerce s1 k1 (CoerceIt t1 cont)
		-- 	coerce T1 S1 (coerce S1 K1 e)
		-- ==>
		--	e, 			if T1=K1
		--	coerce T1 K1 e,		otherwise
		--
		-- For example, in the initial form of a worker
		-- we may find 	(coerce T (coerce S (\x.e))) y
		-- and we'd like it to simplify to e[y/x] in one round 
		-- of simplification
	  | t1 == k1  = cont		 	-- The coerces cancel out
	  | otherwise = CoerceIt t1 cont	-- They don't cancel, but 
						-- the inner one is redundant

	addCoerce t1t2 s1s2 (ApplyTo dup arg arg_se cont)
	  | Just (s1, s2) <- splitFunTy_maybe s1s2
		--	(coerce (T1->T2) (S1->S2) F) E
		-- ===> 
		--	coerce T2 S2 (F (coerce S1 T1 E))
		--
		-- t1t2 must be a function type, T1->T2
		-- but s1s2 might conceivably not be
		--
		-- When we build the ApplyTo we can't mix the out-types
		-- with the InExpr in the argument, so we simply substitute
		-- to make it all consistent.  This isn't a common case.
	  = let 
		(t1,t2) = splitFunTy t1t2
		new_arg = mkCoerce s1 t1 (substExpr (mkSubst in_scope arg_se) arg)
	    in
	    ApplyTo dup new_arg emptySubstEnv (addCoerce t2 s2 cont)
			
	addCoerce to' _ cont = CoerceIt to' cont
    in
    simplType to		`thenSmpl` \ to' ->
    simplType from		`thenSmpl` \ from' ->
    simplExprF body (addCoerce to' from' cont)

		
-- Hack: we only distinguish subsumed cost centre stacks for the purposes of
-- inlining.  All other CCCSs are mapped to currentCCS.
simplNote (SCC cc) e cont
  = setEnclosingCC currentCCS $
    simplExpr e 	`thenSmpl` \ e ->
    rebuild (mkSCC cc e) cont

simplNote InlineCall e cont
  = simplExprF e (InlinePlease cont)

--	 Comments about the InlineMe case 
--	 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Don't inline in the RHS of something that has an
-- inline pragma.  But be careful that the InScopeEnv that
-- we return does still have inlinings on!
-- 
-- It really is important to switch off inlinings.  This function
-- may be inlinined in other modules, so we don't want to remove
-- (by inlining) calls to functions that have specialisations, or
-- that may have transformation rules in an importing scope.
-- E.g. 	{-# INLINE f #-}
-- 		f x = ...g...
-- and suppose that g is strict *and* has specialisations.
-- If we inline g's wrapper, we deny f the chance of getting
-- the specialised version of g when f is inlined at some call site
-- (perhaps in some other module).

-- It's also important not to inline a worker back into a wrapper.
-- A wrapper looks like
--	wraper = inline_me (\x -> ...worker... )
-- Normally, the inline_me prevents the worker getting inlined into
-- the wrapper (initially, the worker's only call site!).  But,
-- if the wrapper is sure to be called, the strictness analyser will
-- mark it 'demanded', so when the RHS is simplified, it'll get an ArgOf
-- continuation.  That's why the keep_inline predicate returns True for
-- ArgOf continuations.  It shouldn't do any harm not to dissolve the
-- inline-me note under these circumstances

simplNote InlineMe e cont
  | keep_inline cont		-- Totally boring continuation
  =				-- Don't inline inside an INLINE expression
    setBlackList noInlineBlackList (simplExpr e)	`thenSmpl` \ e' ->
    rebuild (mkInlineMe e') cont

  | otherwise  	-- Dissolve the InlineMe note if there's
		-- an interesting context of any kind to combine with
		-- (even a type application -- anything except Stop)
  = simplExprF e cont
  where
    keep_inline (Stop _ _)    = True		-- See notes above
    keep_inline (ArgOf _ _ _) = True		-- about this predicate
    keep_inline other	      = False
\end{code}


%************************************************************************
%*									*
\subsection{Binding}
%*									*
%************************************************************************

@simplNonRecBind@ is used for non-recursive lets in expressions, 
as well as true beta reduction.

Very similar to @simplLazyBind@, but not quite the same.

\begin{code}
simplNonRecBind :: InId 		-- Binder
	  -> InExpr -> SubstEnv		-- Arg, with its subst-env
	  -> OutType			-- Type of thing computed by the context
	  -> SimplM OutExprStuff	-- The body
	  -> SimplM OutExprStuff
#ifdef DEBUG
simplNonRecBind bndr rhs rhs_se cont_ty thing_inside
  | isTyVar bndr
  = pprPanic "simplNonRecBind" (ppr bndr <+> ppr rhs)
#endif

simplNonRecBind bndr rhs rhs_se cont_ty thing_inside
  | preInlineUnconditionally False {- not black listed -} bndr
  = tick (PreInlineUnconditionally bndr)		`thenSmpl_`
    extendSubst bndr (ContEx rhs_se rhs) thing_inside

  | otherwise
  =  	-- Simplify the binder.
	-- Don't use simplBinder because that doesn't keep 
	-- fragile occurrence in the substitution
    simplLetId bndr					$ \ bndr' ->
    getSubst						`thenSmpl` \ bndr_subst ->
    let
	-- Substitute its IdInfo (which simplLetId does not)
	-- The appropriate substitution env is the one right here,
	-- not rhs_se.  Often they are the same, when all this 
	-- has arisen from an application (\x. E) RHS, perhaps they aren't
	bndr''    = simplIdInfo bndr_subst (idInfo bndr) bndr'
	bndr_ty'  = idType bndr'
	is_strict = isStrict (idDemandInfo bndr) || isStrictType bndr_ty'
    in
    modifyInScope bndr'' bndr''				$

	-- Simplify the argument
    simplValArg bndr_ty' is_strict rhs rhs_se cont_ty	$ \ rhs' ->

	-- Now complete the binding and simplify the body
    if needsCaseBinding bndr_ty' rhs' then
	addCaseBind bndr'' rhs' thing_inside
    else
	completeBinding bndr bndr'' False False rhs' thing_inside
\end{code}


\begin{code}
simplTyArg :: InType -> SubstEnv -> SimplM OutType
simplTyArg ty_arg se
  = getInScope		`thenSmpl` \ in_scope ->
    let
	ty_arg' = substTy (mkSubst in_scope se) ty_arg
    in
    seqType ty_arg'	`seq`
    returnSmpl ty_arg'

simplValArg :: OutType		-- rhs_ty: Type of arg; used only occasionally
	    -> Bool		-- True <=> evaluate eagerly
	    -> InExpr -> SubstEnv
	    -> OutType		-- cont_ty: Type of thing computed by the context
	    -> (OutExpr -> SimplM OutExprStuff)	
				-- Takes an expression of type rhs_ty, 
				-- returns an expression of type cont_ty
	    -> SimplM OutExprStuff	-- An expression of type cont_ty

simplValArg arg_ty is_strict arg arg_se cont_ty thing_inside
  | is_strict
  = getEnv 				`thenSmpl` \ env ->
    setSubstEnv arg_se 				$
    simplExprF arg (ArgOf NoDup cont_ty 	$ \ rhs' ->
    setAllExceptInScope env			$
    thing_inside rhs')

  | otherwise
  = simplRhs False {- Not top level -} 
	     True {- OK to float unboxed -}
	     arg_ty arg arg_se 
	     thing_inside
\end{code}


completeBinding
	- deals only with Ids, not TyVars
	- take an already-simplified RHS

It does *not* attempt to do let-to-case.  Why?  Because they are used for

	- top-level bindings
		(when let-to-case is impossible) 

	- many situations where the "rhs" is known to be a WHNF
		(so let-to-case is inappropriate).

\begin{code}
completeBinding :: InId 		-- Binder
		-> OutId		-- New binder
		-> Bool			-- True <=> top level
		-> Bool			-- True <=> black-listed; don't inline
	        -> OutExpr		-- Simplified RHS
		-> SimplM (OutStuff a)	-- Thing inside
	   	-> SimplM (OutStuff a)

completeBinding old_bndr new_bndr top_lvl black_listed new_rhs thing_inside
  |  isDeadOcc occ_info 	-- This happens; for example, the case_bndr during case of
				-- known constructor:  case (a,b) of x { (p,q) -> ... }
				-- Here x isn't mentioned in the RHS, so we don't want to
				-- create the (dead) let-binding  let x = (a,b) in ...
  =  thing_inside

  | trivial_rhs && not must_keep_binding
	-- We're looking at a binding with a trivial RHS, so
	-- perhaps we can discard it altogether!
	--
	-- NB: a loop breaker has must_keep_binding = True
	-- and non-loop-breakers only have *forward* references
	-- Hence, it's safe to discard the binding
	--	
	-- NOTE: This isn't our last opportunity to inline.
	-- We're at the binding site right now, and
	-- we'll get another opportunity when we get to the ocurrence(s)

	-- Note that we do this unconditional inlining only for trival RHSs.
	-- Don't inline even WHNFs inside lambdas; doing so may
	-- simply increase allocation when the function is called
	-- This isn't the last chance; see NOTE above.
	--
	-- NB: Even inline pragmas (e.g. IMustBeINLINEd) are ignored here
	-- Why?  Because we don't even want to inline them into the
	-- RHS of constructor arguments. See NOTE above
	--
	-- NB: Even NOINLINEis ignored here: if the rhs is trivial
	-- it's best to inline it anyway.  We often get a=E; b=a
	-- from desugaring, with both a and b marked NOINLINE.
  = 		-- Drop the binding
    extendSubst old_bndr (DoneEx new_rhs)	$
		-- Use the substitution to make quite, quite sure that the substitution
		-- will happen, since we are going to discard the binding
    tick (PostInlineUnconditionally old_bndr)	`thenSmpl_`
    thing_inside

  | Note coercion@(Coerce _ inner_ty) inner_rhs <- new_rhs,
    not trivial_rhs && not (isUnLiftedType inner_ty)
	-- x = coerce t e  ==>  c = e; x = inline_me (coerce t c)
	-- Now x can get inlined, which moves the coercion
	-- to the usage site.  This is a bit like worker/wrapper stuff,
	-- but it's useful to do it very promptly, so that
	--	x = coerce T (I# 3)
	-- get's w/wd to
	--	c = I# 3
	--	x = coerce T c
	-- This in turn means that
	--	case (coerce Int x) of ...
	-- will inline x.  
	-- Also the full-blown w/w thing isn't set up for non-functions
	--
	-- The (not (isUnLiftedType inner_ty)) avoids the nasty case of
	--	x::Int = coerce Int Int# (foo y)
	-- ==>
	--	v::Int# = foo y
	--	x::Int  = coerce Int Int# v
	-- which would be bogus because then v will be evaluated strictly.
	-- How can this arise?  Via 
	--	x::Int = case (foo y) of { ... }
	-- followed by case elimination.
	--
	-- The inline_me note is so that the simplifier doesn't 
	-- just substitute c back inside x's rhs!  (Typically, x will
	-- get substituted away, but not if it's exported.)
  = newId SLIT("c") inner_ty 					$ \ c_id ->
    completeBinding c_id c_id top_lvl False inner_rhs		$
    completeBinding old_bndr new_bndr top_lvl black_listed
		    (Note InlineMe (Note coercion (Var c_id)))	$
    thing_inside

  |  otherwise
  = let
		-- We make new IdInfo for the new binder by starting from the old binder, 
		-- doing appropriate substitutions.
		-- Then we add arity and unfolding info to get the new binder
  	new_bndr_info = idInfo new_bndr `setArityInfo` arity_info

		-- Add the unfolding *only* for non-loop-breakers
		-- Making loop breakers not have an unfolding at all 
		-- means that we can avoid tests in exprIsConApp, for example.
		-- This is important: if exprIsConApp says 'yes' for a recursive
		-- thing, then we can get into an infinite loop
        info_w_unf | loop_breaker = new_bndr_info
		   | otherwise    = new_bndr_info `setUnfoldingInfo` mkUnfolding top_lvl new_rhs

	final_id = new_bndr `setIdInfo` info_w_unf
    in
		-- These seqs forces the Id, and hence its IdInfo,
		-- and hence any inner substitutions
    final_id				`seq`
    addLetBind (NonRec final_id new_rhs) 	$
    modifyInScope new_bndr final_id thing_inside

  where
    old_info          = idInfo old_bndr
    occ_info          = occInfo old_info
    loop_breaker      = isLoopBreaker occ_info
    trivial_rhs	      = exprIsTrivial new_rhs
    must_keep_binding = black_listed || loop_breaker || isExportedId old_bndr
    arity_info	      = atLeastArity (exprArity new_rhs)
\end{code}    



%************************************************************************
%*									*
\subsection{simplLazyBind}
%*									*
%************************************************************************

simplLazyBind basically just simplifies the RHS of a let(rec).
It does two important optimisations though:

	* It floats let(rec)s out of the RHS, even if they
	  are hidden by big lambdas

	* It does eta expansion

\begin{code}
simplLazyBind :: Bool			-- True <=> top level
	      -> InId -> OutId
	      -> InExpr 		-- The RHS
	      -> SimplM (OutStuff a)	-- The body of the binding
	      -> SimplM (OutStuff a)
-- When called, the subst env is correct for the entire let-binding
-- and hence right for the RHS.
-- Also the binder has already been simplified, and hence is in scope

simplLazyBind top_lvl bndr bndr' rhs thing_inside
  = getBlackList		`thenSmpl` \ black_list_fn ->
    let
	black_listed = black_list_fn bndr
    in

    if preInlineUnconditionally black_listed bndr then
	-- Inline unconditionally
	tick (PreInlineUnconditionally bndr)	`thenSmpl_`
	getSubstEnv 				`thenSmpl` \ rhs_se ->
	(extendSubst bndr (ContEx rhs_se rhs) thing_inside)
    else

  	-- Simplify the RHS
    getSubst 					`thenSmpl` \ rhs_subst ->
    let
	-- Substitute IdInfo on binder, in the light of earlier
	-- substitutions in this very letrec, and extend the in-scope
	-- env so that it can see the new thing
	bndr'' = simplIdInfo rhs_subst (idInfo bndr) bndr'
    in
    modifyInScope bndr'' bndr''				$

    simplRhs top_lvl False {- Not ok to float unboxed (conservative) -}
	     (idType bndr')
	     rhs (substEnv rhs_subst)			$ \ rhs' ->

	-- Now compete the binding and simplify the body
    completeBinding bndr bndr'' top_lvl black_listed rhs' thing_inside
\end{code}



\begin{code}
simplRhs :: Bool		-- True <=> Top level
	 -> Bool		-- True <=> OK to float unboxed (speculative) bindings
				--	    False for (a) recursive and (b) top-level bindings
	 -> OutType 		-- Type of RHS; used only occasionally
	 -> InExpr -> SubstEnv
	 -> (OutExpr -> SimplM (OutStuff a))
	 -> SimplM (OutStuff a)
simplRhs top_lvl float_ubx rhs_ty rhs rhs_se thing_inside
  =	-- Simplify it
    setSubstEnv rhs_se (simplExprF rhs (mkRhsStop rhs_ty))	`thenSmpl` \ (floats1, (rhs_in_scope, rhs1)) ->
    let
	(floats2, rhs2) = splitFloats float_ubx floats1 rhs1
    in
		-- There's a subtlety here.  There may be a binding (x* = e) in the
		-- floats, where the '*' means 'will be demanded'.  So is it safe
		-- to float it out?  Answer no, but it won't matter because
		-- we only float if arg' is a WHNF,
		-- and so there can't be any 'will be demanded' bindings in the floats.
		-- Hence the assert
    WARN( any demanded_float (fromOL floats2), ppr (fromOL floats2) )

	-- 			Transform the RHS
	-- It's important that we do eta expansion on function *arguments* (which are
	-- simplified with simplRhs), as well as let-bound right-hand sides.  
	-- Otherwise we find that things like
	--	f (\x -> case x of I# x' -> coerce T (\ y -> ...))
	-- get right through to the code generator as two separate lambdas, 
	-- which is a Bad Thing
    tryRhsTyLam rhs2		`thenSmpl` \ (floats3, rhs3) ->
    tryEtaExpansion rhs3 rhs_ty	`thenSmpl` \ (floats4, rhs4) ->

 	-- Float lets if (a) we're at the top level
	-- or 		 (b) the resulting RHS is one we'd like to expose
    if (top_lvl || exprIsCheap rhs4) then
	(if (isNilOL floats2 && null floats3 && null floats4) then
		returnSmpl ()
	 else
		tick LetFloatFromLet)			`thenSmpl_`

	addFloats floats2 rhs_in_scope	$
	addAuxiliaryBinds floats3 	$
	addAuxiliaryBinds floats4 	$
	thing_inside rhs4
    else	
		-- Don't do the float
	thing_inside (wrapFloats floats1 rhs1)

demanded_float (NonRec b r) = isStrict (idDemandInfo b) && not (isUnLiftedType (idType b))
		-- Unlifted-type (cheap-eagerness) lets may well have a demanded flag on them
demanded_float (Rec _)	    = False

-- If float_ubx is true we float all the bindings, otherwise
-- we just float until we come across an unlifted one.
-- Remember that the unlifted bindings in the floats are all for
-- guaranteed-terminating non-exception-raising unlifted things,
-- which we are happy to do speculatively.  However, we may still
-- not be able to float them out, because the context
-- is either a Rec group, or the top level, neither of which
-- can tolerate them.
splitFloats float_ubx floats rhs
  | float_ubx = (floats, rhs)		-- Float them all
  | otherwise = go (fromOL floats)
  where
    go []		    = (nilOL, rhs)
    go (f:fs) | must_stay f = (nilOL, mkLets (f:fs) rhs)
	      | otherwise   = case go fs of
				   (out, rhs') -> (f `consOL` out, rhs')

    must_stay (Rec prs)    = False	-- No unlifted bindings in here
    must_stay (NonRec b r) = isUnLiftedType (idType b)
\end{code}



%************************************************************************
%*									*
\subsection{Variables}
%*									*
%************************************************************************

\begin{code}
simplVar var cont
  = getSubst		`thenSmpl` \ subst ->
    case lookupIdSubst subst var of
	DoneEx e	-> zapSubstEnv (simplExprF e cont)
	ContEx env1 e   -> setSubstEnv env1 (simplExprF e cont)
	DoneId var1 occ -> WARN( not (isInScope var1 subst) && mustHaveLocalBinding var1,
				 text "simplVar:" <+> ppr var )
			   zapSubstEnv (completeCall var1 occ cont)
		-- The template is already simplified, so don't re-substitute.
		-- This is VITAL.  Consider
		--	let x = e in
		--	let y = \z -> ...x... in
		--	\ x -> ...y...
		-- We'll clone the inner \x, adding x->x' in the id_subst
		-- Then when we inline y, we must *not* replace x by x' in
		-- the inlined copy!!

---------------------------------------------------------
--	Dealing with a call

completeCall var occ_info cont
  = getBlackList		`thenSmpl` \ black_list_fn ->
    getInScope			`thenSmpl` \ in_scope ->
    getContArgs var cont	`thenSmpl` \ (args, call_cont, inline_call) ->
    getDOptsSmpl		`thenSmpl` \ dflags ->
    let
	black_listed       = black_list_fn var
	arg_infos	   = [ interestingArg in_scope arg subst 
			     | (arg, subst, _) <- args, isValArg arg]

	interesting_cont = interestingCallContext (not (null args)) 
						  (not (null arg_infos))
						  call_cont

	inline_cont | inline_call = discardInline cont
		    | otherwise   = cont

	maybe_inline = callSiteInline dflags black_listed inline_call occ_info
				      var arg_infos interesting_cont
    in
	-- First, look for an inlining
    case maybe_inline of {
	Just unfolding  	-- There is an inlining!
	  ->  tick (UnfoldingDone var)		`thenSmpl_`
	      simplExprF unfolding inline_cont

	;
	Nothing -> 		-- No inlining!


    simplifyArgs (isDataConId var) args (contResultType call_cont)  $ \ args' ->

	-- Next, look for rules or specialisations that match
	--
	-- It's important to simplify the args first, because the rule-matcher
	-- doesn't do substitution as it goes.  We don't want to use subst_args
	-- (defined in the 'where') because that throws away useful occurrence info,
	-- and perhaps-very-important specialisations.
	--
	-- Some functions have specialisations *and* are strict; in this case,
	-- we don't want to inline the wrapper of the non-specialised thing; better
	-- to call the specialised thing instead.
	-- But the black-listing mechanism means that inlining of the wrapper
	-- won't occur for things that have specialisations till a later phase, so
	-- it's ok to try for inlining first.
	--
	-- You might think that we shouldn't apply rules for a loop breaker: 
	-- doing so might give rise to an infinite loop, because a RULE is
	-- rather like an extra equation for the function:
	--	RULE:		f (g x) y = x+y
	--	Eqn:		f a     y = a-y
	--
	-- But it's too drastic to disable rules for loop breakers.  
	-- Even the foldr/build rule would be disabled, because foldr 
	-- is recursive, and hence a loop breaker:
	--	foldr k z (build g) = g k z
	-- So it's up to the programmer: rules can cause divergence

    getSwitchChecker 	`thenSmpl` \ chkr ->
    let
	maybe_rule | switchIsOn chkr DontApplyRules = Nothing
		   | otherwise			    = lookupRule in_scope var args' 
    in
    case maybe_rule of {
	Just (rule_name, rule_rhs) -> 
		tick (RuleFired rule_name)			`thenSmpl_`
#ifdef DEBUG
		(if dopt Opt_D_dump_inlinings dflags then
		   pprTrace "Rule fired" (vcat [
			text "Rule:" <+> ptext rule_name,
			text "Before:" <+> ppr var <+> sep (map pprParendExpr args'),
			text "After: " <+> pprCoreExpr rule_rhs])
		 else
			id)		$
#endif
		simplExprF rule_rhs call_cont ;
	
	Nothing -> 		-- No rules

	-- Done
    rebuild (mkApps (Var var) args') call_cont
    }}


---------------------------------------------------------
--	Simplifying the arguments of a call

simplifyArgs :: Bool 				-- It's a data constructor
	     -> [(InExpr, SubstEnv, Bool)]	-- Details of the arguments
	     -> OutType				-- Type of the continuation
	     -> ([OutExpr] -> SimplM OutExprStuff)
	     -> SimplM OutExprStuff

-- Simplify the arguments to a call.
-- This part of the simplifier may break the no-shadowing invariant
-- Consider
--	f (...(\a -> e)...) (case y of (a,b) -> e')
-- where f is strict in its second arg
-- If we simplify the innermost one first we get (...(\a -> e)...)
-- Simplifying the second arg makes us float the case out, so we end up with
--	case y of (a,b) -> f (...(\a -> e)...) e'
-- So the output does not have the no-shadowing invariant.  However, there is
-- no danger of getting name-capture, because when the first arg was simplified
-- we used an in-scope set that at least mentioned all the variables free in its
-- static environment, and that is enough.
--
-- We can't just do innermost first, or we'd end up with a dual problem:
--	case x of (a,b) -> f e (...(\a -> e')...)
--
-- I spent hours trying to recover the no-shadowing invariant, but I just could
-- not think of an elegant way to do it.  The simplifier is already knee-deep in
-- continuations.  We have to keep the right in-scope set around; AND we have
-- to get the effect that finding (error "foo") in a strict arg position will
-- discard the entire application and replace it with (error "foo").  Getting
-- all this at once is TOO HARD!

simplifyArgs is_data_con args cont_ty thing_inside
  | not is_data_con
  = go args thing_inside

  | otherwise	-- It's a data constructor, so we want 
		-- to switch off inlining in the arguments
		-- If we don't do this, consider:
		--	let x = +# p q in C {x}
		-- Even though x get's an occurrence of 'many', its RHS looks cheap,
		-- and there's a good chance it'll get inlined back into C's RHS. Urgh!
  = getBlackList				`thenSmpl` \ old_bl ->
    setBlackList noInlineBlackList		$
    go args					$ \ args' ->
    setBlackList old_bl				$
    thing_inside args'

  where
    go []	  thing_inside = thing_inside []
    go (arg:args) thing_inside = simplifyArg is_data_con arg cont_ty 	$ \ arg' ->
				 go args 				$ \ args' ->
				 thing_inside (arg':args')

simplifyArg is_data_con (Type ty_arg, se, _) cont_ty thing_inside
  = simplTyArg ty_arg se	`thenSmpl` \ new_ty_arg ->
    thing_inside (Type new_ty_arg)

simplifyArg is_data_con (val_arg, se, is_strict) cont_ty thing_inside
  = getInScope		`thenSmpl` \ in_scope ->
    let
	arg_ty = substTy (mkSubst in_scope se) (exprType val_arg)
    in
    if not is_data_con then
	-- An ordinary function
	simplValArg arg_ty is_strict val_arg se cont_ty thing_inside
    else
	-- A data constructor
	-- simplifyArgs has already switched off inlining, so 
	-- all we have to do here is to let-bind any non-trivial argument

	-- It's not always the case that new_arg will be trivial
	-- Consider		f x
	-- where, in one pass, f gets substituted by a constructor,
	-- but x gets substituted by an expression (assume this is the
	-- unique occurrence of x).  It doesn't really matter -- it'll get
	-- fixed up next pass.  And it happens for dictionary construction,
	-- which mentions the wrapper constructor to start with.
	simplValArg arg_ty is_strict val_arg se cont_ty 	$ \ arg' ->
	
	if exprIsTrivial arg' then
	     thing_inside arg'
	else
	newId SLIT("a") (exprType arg')		$ \ arg_id ->
	addNonRecBind arg_id arg'		$
	thing_inside (Var arg_id)
\end{code}		   


%************************************************************************
%*									*
\subsection{Decisions about inlining}
%*									*
%************************************************************************

NB: At one time I tried not pre/post-inlining top-level things,
even if they occur exactly once.  Reason: 
	(a) some might appear as a function argument, so we simply
		replace static allocation with dynamic allocation:
		   l = <...>
		   x = f l
	becomes
		   x = f <...>

	(b) some top level things might be black listed

HOWEVER, I found that some useful foldr/build fusion was lost (most
notably in spectral/hartel/parstof) because the foldr didn't see the build.

Doing the dynamic allocation isn't a big deal, in fact, but losing the
fusion can be.

\begin{code}
preInlineUnconditionally :: Bool {- Black listed -} -> InId -> Bool
	-- Examines a bndr to see if it is used just once in a 
	-- completely safe way, so that it is safe to discard the binding
	-- inline its RHS at the (unique) usage site, REGARDLESS of how
	-- big the RHS might be.  If this is the case we don't simplify
	-- the RHS first, but just inline it un-simplified.
	--
	-- This is much better than first simplifying a perhaps-huge RHS
	-- and then inlining and re-simplifying it.
	--
	-- NB: we don't even look at the RHS to see if it's trivial
	-- We might have
	--			x = y
	-- where x is used many times, but this is the unique occurrence
	-- of y.  We should NOT inline x at all its uses, because then
	-- we'd do the same for y -- aargh!  So we must base this
	-- pre-rhs-simplification decision solely on x's occurrences, not
	-- on its rhs.
	-- 
	-- Evne RHSs labelled InlineMe aren't caught here, because
	-- there might be no benefit from inlining at the call site.

preInlineUnconditionally black_listed bndr
  | black_listed || opt_SimplNoPreInlining = False
  | otherwise = case idOccInfo bndr of
	  	  OneOcc in_lam once -> not in_lam && once
			-- Not inside a lambda, one occurrence ==> safe!
		  other 	     -> False
\end{code}



%************************************************************************
%*									*
\subsection{The main rebuilder}
%*									*
%************************************************************************

\begin{code}
-------------------------------------------------------------------
-- Finish rebuilding
rebuild_done expr = returnOutStuff expr

---------------------------------------------------------
rebuild :: OutExpr -> SimplCont -> SimplM OutExprStuff

--	Stop continuation
rebuild expr (Stop _ _) = rebuild_done expr

--	ArgOf continuation
rebuild expr (ArgOf _ _ cont_fn) = cont_fn expr

--	ApplyTo continuation
rebuild expr cont@(ApplyTo _ arg se cont')
  = setSubstEnv se (simplExpr arg)	`thenSmpl` \ arg' ->
    rebuild (App expr arg') cont'

--	Coerce continuation
rebuild expr (CoerceIt to_ty cont)
  = rebuild (mkCoerce to_ty (exprType expr) expr) cont

--	Inline continuation
rebuild expr (InlinePlease cont)
  = rebuild (Note InlineCall expr) cont

rebuild scrut (Select _ bndr alts se cont)
  = rebuild_case scrut bndr alts se cont
\end{code}

Case elimination [see the code above]
~~~~~~~~~~~~~~~~
Start with a simple situation:

	case x# of	===>   e[x#/y#]
	  y# -> e

(when x#, y# are of primitive type, of course).  We can't (in general)
do this for algebraic cases, because we might turn bottom into
non-bottom!

Actually, we generalise this idea to look for a case where we're
scrutinising a variable, and we know that only the default case can
match.  For example:
\begin{verbatim}
	case x of
	  0#    -> ...
	  other -> ...(case x of
			 0#    -> ...
			 other -> ...) ...
\end{code}
Here the inner case can be eliminated.  This really only shows up in
eliminating error-checking code.

We also make sure that we deal with this very common case:

 	case e of 
	  x -> ...x...

Here we are using the case as a strict let; if x is used only once
then we want to inline it.  We have to be careful that this doesn't 
make the program terminate when it would have diverged before, so we
check that 
	- x is used strictly, or
	- e is already evaluated (it may so if e is a variable)

Lastly, we generalise the transformation to handle this:

	case e of	===> r
	   True  -> r
	   False -> r

We only do this for very cheaply compared r's (constructors, literals
and variables).  If pedantic bottoms is on, we only do it when the
scrutinee is a PrimOp which can't fail.

We do it *here*, looking at un-simplified alternatives, because we
have to check that r doesn't mention the variables bound by the
pattern in each alternative, so the binder-info is rather useful.

So the case-elimination algorithm is:

	1. Eliminate alternatives which can't match

	2. Check whether all the remaining alternatives
		(a) do not mention in their rhs any of the variables bound in their pattern
	   and  (b) have equal rhss

	3. Check we can safely ditch the case:
		   * PedanticBottoms is off,
		or * the scrutinee is an already-evaluated variable
		or * the scrutinee is a primop which is ok for speculation
			-- ie we want to preserve divide-by-zero errors, and
			-- calls to error itself!

		or * [Prim cases] the scrutinee is a primitive variable

		or * [Alg cases] the scrutinee is a variable and
		     either * the rhs is the same variable
			(eg case x of C a b -> x  ===>   x)
		     or     * there is only one alternative, the default alternative,
				and the binder is used strictly in its scope.
				[NB this is helped by the "use default binder where
				 possible" transformation; see below.]


If so, then we can replace the case with one of the rhss.


Blob of helper functions for the "case-of-something-else" situation.

\begin{code}
---------------------------------------------------------
-- 	Eliminate the case if possible

rebuild_case scrut bndr alts se cont
  | maybeToBool maybe_con_app
  = knownCon scrut (DataAlt con) args bndr alts se cont

  | canEliminateCase scrut bndr alts
  = tick (CaseElim bndr)			`thenSmpl_` (
    setSubstEnv se				$			
    simplBinder bndr				$ \ bndr' ->
	-- Remember to bind the case binder!
    completeBinding bndr bndr' False False scrut 	$
    simplExprF (head (rhssOfAlts alts)) cont)

  | otherwise
  = complete_case scrut bndr alts se cont

  where
    maybe_con_app    = exprIsConApp_maybe scrut
    Just (con, args) = maybe_con_app

   	-- See if we can get rid of the case altogether
	-- See the extensive notes on case-elimination above
canEliminateCase scrut bndr alts
  = 	-- Check that the RHSs are all the same, and
	-- don't use the binders in the alternatives
	-- This test succeeds rapidly in the common case of
	-- a single DEFAULT alternative
    all (cheapEqExpr rhs1) other_rhss && all binders_unused alts

	-- Check that the scrutinee can be let-bound instead of case-bound
    && (   exprOkForSpeculation scrut
		-- OK not to evaluate it
		-- This includes things like (==# a# b#)::Bool
		-- so that we simplify 
		-- 	case ==# a# b# of { True -> x; False -> x }
		-- to just
		--	x
		-- This particular example shows up in default methods for
		-- comparision operations (e.g. in (>=) for Int.Int32)
	|| exprIsValue scrut			-- It's already evaluated
	|| var_demanded_later scrut		-- It'll be demanded later

--      || not opt_SimplPedanticBottoms)	-- Or we don't care!
--	We used to allow improving termination by discarding cases, unless -fpedantic-bottoms was on,
-- 	but that breaks badly for the dataToTag# primop, which relies on a case to evaluate
-- 	its argument:  case x of { y -> dataToTag# y }
--	Here we must *not* discard the case, because dataToTag# just fetches the tag from
--	the info pointer.  So we'll be pedantic all the time, and see if that gives any
-- 	other problems
       )

  where
    (rhs1:other_rhss)		 = rhssOfAlts alts
    binders_unused (_, bndrs, _) = all isDeadBinder bndrs

    var_demanded_later (Var v) = isStrict (idDemandInfo bndr)	-- It's going to be evaluated later
    var_demanded_later other   = False


---------------------------------------------------------
-- 	Case of something else

complete_case scrut case_bndr alts se cont
  = 	-- Prepare case alternatives
    prepareCaseAlts case_bndr (splitTyConApp_maybe (idType case_bndr))
		    impossible_cons alts		`thenSmpl` \ better_alts ->
    
	-- Set the new subst-env in place (before dealing with the case binder)
    setSubstEnv se				$

	-- Deal with the case binder, and prepare the continuation;
	-- The new subst_env is in place
    prepareCaseCont better_alts cont		$ \ cont' ->
	

	-- Deal with variable scrutinee
    (	
        getSwitchChecker 				`thenSmpl` \ chkr ->
	simplCaseBinder (switchIsOn chkr NoCaseOfCase)
			scrut case_bndr 		$ \ case_bndr' zap_occ_info ->

	-- Deal with the case alternatives
	simplAlts zap_occ_info impossible_cons
	          case_bndr' better_alts cont'	`thenSmpl` \ alts' ->

	mkCase scrut case_bndr' alts'
    )						`thenSmpl` \ case_expr ->

	-- Notice that the simplBinder, prepareCaseCont, etc, do *not* scope
	-- over the rebuild_done; rebuild_done returns the in-scope set, and
	-- that should not include these chaps!
    rebuild_done case_expr	
  where
    impossible_cons = case scrut of
			    Var v -> otherCons (idUnfolding v)
			    other -> []


knownCon :: OutExpr -> AltCon -> [OutExpr]
	 -> InId -> [InAlt] -> SubstEnv -> SimplCont
	 -> SimplM OutExprStuff

knownCon expr con args bndr alts se cont
  = 	-- Arguments should be atomic;
	-- yell if not
    WARN( not (all exprIsTrivial args), 
	  text "knownCon" <+> ppr expr )
    tick (KnownBranch bndr)	`thenSmpl_`
    setSubstEnv se		(
    simplBinder bndr		$ \ bndr' ->
    completeBinding bndr bndr' False False expr $
	-- Don't use completeBeta here.  The expr might be
	-- an unboxed literal, like 3, or a variable
	-- whose unfolding is an unboxed literal... and
	-- completeBeta will just construct another case
					-- expression!
    case findAlt con alts of
	(DEFAULT, bs, rhs)     -> ASSERT( null bs )
			          simplExprF rhs cont

	(LitAlt lit, bs, rhs) ->  ASSERT( null bs )
				  simplExprF rhs cont

	(DataAlt dc, bs, rhs)  -> ASSERT( length bs == length real_args )
				  extendSubstList bs (map mk real_args) $
			          simplExprF rhs cont
			       where
				  real_args    = drop (dataConNumInstArgs dc) args
				  mk (Type ty) = DoneTy ty
				  mk other     = DoneEx other
    )
\end{code}

\begin{code}
prepareCaseCont :: [InAlt] -> SimplCont
		-> (SimplCont -> SimplM (OutStuff a))
		-> SimplM (OutStuff a)
	-- Polymorphic recursion here!

prepareCaseCont [alt] cont thing_inside = thing_inside cont
prepareCaseCont alts  cont thing_inside = simplType (coreAltsType alts)		`thenSmpl` \ alts_ty ->
					  mkDupableCont alts_ty cont thing_inside
	-- At one time I passed in the un-simplified type, and simplified
	-- it only if we needed to construct a join binder, but that	
	-- didn't work because we have to decompse function types
	-- (using funResultTy) in mkDupableCont.
\end{code}

simplCaseBinder checks whether the scrutinee is a variable, v.  If so,
try to eliminate uses of v in the RHSs in favour of case_bndr; that
way, there's a chance that v will now only be used once, and hence
inlined.

There is a time we *don't* want to do that, namely when
-fno-case-of-case is on.  This happens in the first simplifier pass,
and enhances full laziness.  Here's the bad case:
	f = \ y -> ...(case x of I# v -> ...(case x of ...) ... )
If we eliminate the inner case, we trap it inside the I# v -> arm,
which might prevent some full laziness happening.  I've seen this
in action in spectral/cichelli/Prog.hs:
	 [(m,n) | m <- [1..max], n <- [1..max]]
Hence the no_case_of_case argument


If we do this, then we have to nuke any occurrence info (eg IAmDead)
in the case binder, because the case-binder now effectively occurs
whenever v does.  AND we have to do the same for the pattern-bound
variables!  Example:

	(case x of { (a,b) -> a }) (case x of { (p,q) -> q })

Here, b and p are dead.  But when we move the argment inside the first
case RHS, and eliminate the second case, we get

	case x or { (a,b) -> a b }

Urk! b is alive!  Reason: the scrutinee was a variable, and case elimination
happened.  Hence the zap_occ_info function returned by simplCaseBinder

\begin{code}
simplCaseBinder no_case_of_case (Var v) case_bndr thing_inside
  | not no_case_of_case
  = simplBinder (zap case_bndr)					$ \ case_bndr' ->
    modifyInScope v case_bndr'					$
	-- We could extend the substitution instead, but it would be
	-- a hack because then the substitution wouldn't be idempotent
	-- any more (v is an OutId).  And this just just as well.
    thing_inside case_bndr' zap
  where
    zap b = b `setIdOccInfo` NoOccInfo
	    
simplCaseBinder add_eval_info other_scrut case_bndr thing_inside
  = simplBinder case_bndr 		$ \ case_bndr' ->
    thing_inside case_bndr' (\ bndr -> bndr)	-- NoOp on bndr
\end{code}

prepareCaseAlts does two things:

1.  Remove impossible alternatives

2.  If the DEFAULT alternative can match only one possible constructor,
    then make that constructor explicit.
    e.g.
	case e of x { DEFAULT -> rhs }
     ===>
	case e of x { (a,b) -> rhs }
    where the type is a single constructor type.  This gives better code
    when rhs also scrutinises x or e.

\begin{code}
prepareCaseAlts bndr (Just (tycon, inst_tys)) scrut_cons alts
  | isDataTyCon tycon
  = case (findDefault filtered_alts, missing_cons) of

	((alts_no_deflt, Just rhs), [data_con]) 	-- Just one missing constructor!
		-> tick (FillInCaseDefault bndr)	`thenSmpl_`
		   let
			(_,_,ex_tyvars,_,_,_) = dataConSig data_con
		   in
		   getUniquesSmpl (length ex_tyvars)				`thenSmpl` \ tv_uniqs ->
		   let
			ex_tyvars' = zipWithEqual "simpl_alt" mk tv_uniqs ex_tyvars
			mk uniq tv = mkSysTyVar uniq (tyVarKind tv)
			arg_tys    = dataConArgTys data_con
					  	   (inst_tys ++ mkTyVarTys ex_tyvars')
		   in
    		   newIds SLIT("a") arg_tys		$ \ bndrs ->
		   returnSmpl ((DataAlt data_con, ex_tyvars' ++ bndrs, rhs) : alts_no_deflt)

	other -> returnSmpl filtered_alts
  where
	-- Filter out alternatives that can't possibly match
    filtered_alts = case scrut_cons of
			[]    -> alts
			other -> [alt | alt@(con,_,_) <- alts, not (con `elem` scrut_cons)]

    missing_cons = [data_con | data_con <- tyConDataConsIfAvailable tycon, 
			       not (data_con `elem` handled_data_cons)]
    handled_data_cons = [data_con | DataAlt data_con         <- scrut_cons] ++
			[data_con | (DataAlt data_con, _, _) <- filtered_alts]

-- The default case
prepareCaseAlts _ _ scrut_cons alts
  = returnSmpl alts			-- Functions


----------------------
simplAlts zap_occ_info scrut_cons case_bndr' alts cont'
  = mapSmpl simpl_alt alts
  where
    inst_tys' = tyConAppArgs (idType case_bndr')

	-- handled_cons is all the constructors that are dealt
	-- with, either by being impossible, or by there being an alternative
    handled_cons = scrut_cons ++ [con | (con,_,_) <- alts, con /= DEFAULT]

    simpl_alt (DEFAULT, _, rhs)
	=	-- In the default case we record the constructors that the
		-- case-binder *can't* be.
		-- We take advantage of any OtherCon info in the case scrutinee
	  modifyInScope case_bndr' (case_bndr' `setIdUnfolding` mkOtherCon handled_cons)	$ 
	  simplExprC rhs cont'							`thenSmpl` \ rhs' ->
	  returnSmpl (DEFAULT, [], rhs')

    simpl_alt (con, vs, rhs)
	= 	-- Deal with the pattern-bound variables
		-- Mark the ones that are in ! positions in the data constructor
		-- as certainly-evaluated.
		-- NB: it happens that simplBinders does *not* erase the OtherCon
		--     form of unfolding, so it's ok to add this info before 
		--     doing simplBinders
	  simplBinders (add_evals con vs)					$ \ vs' ->

		-- Bind the case-binder to (con args)
	  let
		unfolding = mkUnfolding False (mkAltExpr con vs' inst_tys')
	  in
	  modifyInScope case_bndr' (case_bndr' `setIdUnfolding` unfolding)	$
	  simplExprC rhs cont'		`thenSmpl` \ rhs' ->
	  returnSmpl (con, vs', rhs')


	-- add_evals records the evaluated-ness of the bound variables of
	-- a case pattern.  This is *important*.  Consider
	--	data T = T !Int !Int
	--
	--	case x of { T a b -> T (a+1) b }
	--
	-- We really must record that b is already evaluated so that we don't
	-- go and re-evaluate it when constructing the result.

    add_evals (DataAlt dc) vs = cat_evals vs (dataConRepStrictness dc)
    add_evals other_con    vs = vs

    cat_evals [] [] = []
    cat_evals (v:vs) (str:strs)
	| isTyVar v    = v				     : cat_evals vs (str:strs)
	| isStrict str = (v' `setIdUnfolding` mkOtherCon []) : cat_evals vs strs
	| otherwise    = v'				     : cat_evals vs strs
	where
	  v' = zap_occ_info v
\end{code}


%************************************************************************
%*									*
\subsection{Duplicating continuations}
%*									*
%************************************************************************

\begin{code}
mkDupableCont :: OutType		-- Type of the thing to be given to the continuation
	      -> SimplCont 
	      -> (SimplCont -> SimplM (OutStuff a))
	      -> SimplM (OutStuff a)
mkDupableCont ty cont thing_inside 
  | contIsDupable cont
  = thing_inside cont

mkDupableCont _ (CoerceIt ty cont) thing_inside
  = mkDupableCont ty cont		$ \ cont' ->
    thing_inside (CoerceIt ty cont')

mkDupableCont ty (InlinePlease cont) thing_inside
  = mkDupableCont ty cont		$ \ cont' ->
    thing_inside (InlinePlease cont')

mkDupableCont join_arg_ty (ArgOf _ cont_ty cont_fn) thing_inside
  = 	-- Build the RHS of the join point
    newId SLIT("a") join_arg_ty				( \ arg_id ->
	cont_fn (Var arg_id)				`thenSmpl` \ (floats, (_, rhs)) ->
	returnSmpl (Lam (setOneShotLambda arg_id) (wrapFloats floats rhs))
    )							`thenSmpl` \ join_rhs ->
   
	-- Build the join Id and continuation
	-- We give it a "$j" name just so that for later amusement
	-- we can identify any join points that don't end up as let-no-escapes
	-- [NOTE: the type used to be exprType join_rhs, but this seems more elegant.]
    newId SLIT("$j") (mkFunTy join_arg_ty cont_ty)	$ \ join_id ->
    let
	new_cont = ArgOf OkToDup cont_ty
			 (\arg' -> rebuild_done (App (Var join_id) arg'))
    in

    tick (CaseOfCase join_id)						`thenSmpl_`
	-- Want to tick here so that we go round again,
	-- and maybe copy or inline the code;
	-- not strictly CaseOf Case
    addLetBind (NonRec join_id join_rhs)	$
    thing_inside new_cont

mkDupableCont ty (ApplyTo _ arg se cont) thing_inside
  = mkDupableCont (funResultTy ty) cont 		$ \ cont' ->
    setSubstEnv se (simplExpr arg)			`thenSmpl` \ arg' ->
    if exprIsDupable arg' then
	thing_inside (ApplyTo OkToDup arg' emptySubstEnv cont')
    else
    newId SLIT("a") (exprType arg')			$ \ bndr ->

    tick (CaseOfCase bndr)				`thenSmpl_`
	-- Want to tick here so that we go round again,
	-- and maybe copy or inline the code;
	-- not strictly CaseOf Case

     addLetBind (NonRec bndr arg')		$
	-- But what if the arg should be case-bound?  We can't use
	-- addNonRecBind here because its type is too specific.
	-- This has been this way for a long time, so I'll leave it,
	-- but I can't convince myself that it's right.

     thing_inside (ApplyTo OkToDup (Var bndr) emptySubstEnv cont')


mkDupableCont ty (Select _ case_bndr alts se cont) thing_inside
  = tick (CaseOfCase case_bndr)						`thenSmpl_`
    setSubstEnv se (
	simplBinder case_bndr						$ \ case_bndr' ->
	prepareCaseCont alts cont					$ \ cont' ->
	mkDupableAlts case_bndr case_bndr' cont' alts			$ \ alts' ->
	returnOutStuff alts'
    )					`thenSmpl` \ (alt_binds, (in_scope, alts')) ->

    addFloats alt_binds in_scope 		$

	-- NB that the new alternatives, alts', are still InAlts, using the original
	-- binders.  That means we can keep the case_bndr intact. This is important
	-- because another case-of-case might strike, and so we want to keep the
	-- info that the case_bndr is dead (if it is, which is often the case).
	-- This is VITAL when the type of case_bndr is an unboxed pair (often the
	-- case in I/O rich code.  We aren't allowed a lambda bound
	-- arg of unboxed tuple type, and indeed such a case_bndr is always dead
    thing_inside (Select OkToDup case_bndr alts' se (mkStop (contResultType cont)))

mkDupableAlts :: InId -> OutId -> SimplCont -> [InAlt] 
	     -> ([InAlt] -> SimplM (OutStuff a))
	     -> SimplM (OutStuff a)
mkDupableAlts case_bndr case_bndr' cont [] thing_inside
  = thing_inside []
mkDupableAlts case_bndr case_bndr' cont (alt:alts) thing_inside
  = mkDupableAlt  case_bndr case_bndr' cont alt		$ \ alt' -> 
    mkDupableAlts case_bndr case_bndr' cont alts	$ \ alts' ->
    thing_inside (alt' : alts')

mkDupableAlt case_bndr case_bndr' cont alt@(con, bndrs, rhs) thing_inside
  = simplBinders bndrs					$ \ bndrs' ->
    simplExprC rhs cont					`thenSmpl` \ rhs' ->

    if (case cont of { Stop _ _ -> exprIsDupable rhs'; other -> False}) then
   	-- It is worth checking for a small RHS because otherwise we
	-- get extra let bindings that may cause an extra iteration of the simplifier to
	-- inline back in place.  Quite often the rhs is just a variable or constructor.
	-- The Ord instance of Maybe in PrelMaybe.lhs, for example, took several extra
	-- iterations because the version with the let bindings looked big, and so wasn't
	-- inlined, but after the join points had been inlined it looked smaller, and so
	-- was inlined.
	--
	-- But since the continuation is absorbed into the rhs, we only do this
	-- for a Stop continuation.
	--
	-- NB: we have to check the size of rhs', not rhs. 
	-- Duplicating a small InAlt might invalidate occurrence information
	-- However, if it *is* dupable, we return the *un* simplified alternative,
	-- because otherwise we'd need to pair it up with an empty subst-env.
	-- (Remember we must zap the subst-env before re-simplifying something).
	-- Rather than do this we simply agree to re-simplify the original (small) thing later.
	thing_inside alt

    else
    let
	rhs_ty' = exprType rhs'
        (used_bndrs, used_bndrs')
	   = unzip [pr | pr@(bndr,bndr') <- zip (case_bndr  : bndrs)
						(case_bndr' : bndrs'),
			 not (isDeadBinder bndr)]
		-- The new binders have lost their occurrence info,
		-- so we have to extract it from the old ones
    in
    ( if null used_bndrs' 
	-- If we try to lift a primitive-typed something out
	-- for let-binding-purposes, we will *caseify* it (!),
	-- with potentially-disastrous strictness results.  So
	-- instead we turn it into a function: \v -> e
	-- where v::State# RealWorld#.  The value passed to this function
	-- is realworld#, which generates (almost) no code.

	-- There's a slight infelicity here: we pass the overall 
	-- case_bndr to all the join points if it's used in *any* RHS,
	-- because we don't know its usage in each RHS separately

	-- We used to say "&& isUnLiftedType rhs_ty'" here, but now
	-- we make the join point into a function whenever used_bndrs'
	-- is empty.  This makes the join-point more CPR friendly. 
	-- Consider:	let j = if .. then I# 3 else I# 4
	--		in case .. of { A -> j; B -> j; C -> ... }
	--
	-- Now CPR should not w/w j because it's a thunk, so
	-- that means that the enclosing function can't w/w either,
	-- which is a lose.  Here's the example that happened in practice:
	--	kgmod :: Int -> Int -> Int
	--	kgmod x y = if x > 0 && y < 0 || x < 0 && y > 0
	--	            then 78
	--		    else 5

	then newId SLIT("w") realWorldStatePrimTy  $ \ rw_id ->
	     returnSmpl ([rw_id], [Var realWorldPrimId])
	else 
	     returnSmpl (used_bndrs', map varToCoreExpr used_bndrs)
    )
	`thenSmpl` \ (final_bndrs', final_args) ->

	-- See comment about "$j" name above
    newId SLIT("$j") (foldr mkPiType rhs_ty' final_bndrs')	$ \ join_bndr ->
	-- Notice the funky mkPiType.  If the contructor has existentials
	-- it's possible that the join point will be abstracted over
	-- type varaibles as well as term variables.
	--  Example:  Suppose we have
	--	data T = forall t.  C [t]
	--  Then faced with
	--	case (case e of ...) of
	--	    C t xs::[t] -> rhs
	--  We get the join point
	--	let j :: forall t. [t] -> ...
	--	    j = /\t \xs::[t] -> rhs
	--	in
	--	case (case e of ...) of
	--	    C t xs::[t] -> j t xs

    let 
	-- We make the lambdas into one-shot-lambdas.  The
	-- join point is sure to be applied at most once, and doing so
	-- prevents the body of the join point being floated out by
	-- the full laziness pass
	really_final_bndrs = map one_shot final_bndrs'
	one_shot v | isId v    = setOneShotLambda v
		   | otherwise = v
    in
    addLetBind (NonRec join_bndr (mkLams really_final_bndrs rhs'))	$
    thing_inside (con, bndrs, mkApps (Var join_bndr) final_args)
\end{code}
