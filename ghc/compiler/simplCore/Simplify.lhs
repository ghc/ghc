
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[Simplify]{The main module of the simplifier}

\begin{code}
module Simplify ( simplTopBinds, simplExpr ) where

#include "HsVersions.h"

import CmdLineOpts	( intSwitchSet, switchIsOn,
			  opt_SccProfilingOn, opt_PprStyle_Debug, opt_SimplDoEtaReduction,
			  opt_SimplNoPreInlining, opt_DictsStrict, opt_SimplPedanticBottoms,
			  SimplifierSwitch(..)
			)
import SimplMonad
import SimplUtils	( mkCase, transformRhs, findAlt, etaCoreExpr,
			  simplBinder, simplBinders, simplIds, findDefault, mkCoerce
			)
import Var		( TyVar, mkSysTyVar, tyVarKind, maybeModifyIdInfo )
import VarEnv
import VarSet
import Id		( Id, idType, idInfo, idUnique,
			  getIdUnfolding, setIdUnfolding, isExportedId, 
			  getIdSpecialisation, setIdSpecialisation,
			  getIdDemandInfo, setIdDemandInfo,
			  setIdInfo,
			  getIdOccInfo, setIdOccInfo,
			  zapLamIdInfo, zapFragileIdInfo,
			  getIdStrictness, 
			  setInlinePragma, mayHaveNoBinding,
			  setOneShotLambda, maybeModifyIdInfo
			)
import IdInfo		( InlinePragInfo(..), OccInfo(..), StrictnessInfo(..), 
		 	  ArityInfo(..), atLeastArity, arityLowerBound, unknownArity,
			  specInfo, inlinePragInfo, setArityInfo, setInlinePragInfo, setUnfoldingInfo
			)
import Demand		( Demand, isStrict, wwLazy )
import Const		( isWHNFCon, conOkForAlt )
import ConFold		( tryPrimOp )
import PrimOp		( PrimOp, primOpStrictness, primOpType )
import DataCon		( DataCon, dataConNumInstArgs, dataConRepStrictness, dataConSig, dataConArgTys )
import Const		( Con(..) )
import Name		( isLocallyDefined )
import CoreSyn
import CoreFVs		( exprFreeVars )
import CoreUnfold	( Unfolding, mkOtherCon, mkUnfolding, otherCons,
			  callSiteInline, hasSomeUnfolding
			)
import CoreUtils	( cheapEqExpr, exprIsDupable, exprIsCheap, exprIsTrivial,
			  coreExprType, coreAltsType, exprArity, exprIsValue,
			  exprOkForSpeculation
			)
import Rules		( lookupRule )
import CostCentre	( isSubsumedCCS, currentCCS, isEmptyCC )
import Type		( Type, mkTyVarTy, mkTyVarTys, isUnLiftedType, seqType,
			  mkFunTy, splitFunTys, splitTyConApp_maybe, splitFunTy_maybe,
			  funResultTy, isDictTy, isDataType, applyTy, applyTys, mkFunTys
			)
import Subst		( Subst, mkSubst, emptySubst, substTy, substExpr,
			  substEnv, isInScope, lookupInScope, lookupIdSubst, substIdInfo
			)
import TyCon		( isDataTyCon, tyConDataCons, tyConClass_maybe, tyConArity, isDataTyCon )
import TysPrim		( realWorldStatePrimTy )
import PrelInfo		( realWorldPrimId )
import BasicTypes	( TopLevelFlag(..), isTopLevel )
import Maybes		( maybeToBool )
import Util		( zipWithEqual, stretchZipEqual, lengthExceeds )
import PprCore
import Outputable
import Unique		( foldrIdKey )	-- Temp
\end{code}


The guts of the simplifier is in this module, but the driver
loop for the simplifier is in SimplCore.lhs.


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
    simplIds (bindersOfBinds binds)	$ \ bndrs' -> 
    simpl_binds binds bndrs'		`thenSmpl` \ (binds', _) ->
    freeTick SimplifierDone		`thenSmpl_`
    returnSmpl binds'
  where

	-- We need to track the zapped top-level binders, because
	-- they should have their fragile IdInfo zapped (notably occurrence info)
    simpl_binds []			  bs     = ASSERT( null bs ) returnSmpl ([], panic "simplTopBinds corner")
    simpl_binds (NonRec bndr rhs : binds) (b:bs) = simplLazyBind True bndr  b rhs	(simpl_binds binds bs)
    simpl_binds (Rec pairs       : binds) bs     = simplRecBind  True pairs (take n bs) (simpl_binds binds (drop n bs))
					         where 
						   n = length pairs

simplRecBind :: Bool -> [(InId, InExpr)] -> [OutId]
	     -> SimplM (OutStuff a) -> SimplM (OutStuff a)
simplRecBind top_lvl pairs bndrs' thing_inside
  = go pairs bndrs'		`thenSmpl` \ (binds', stuff) ->
    returnSmpl (addBind (Rec (flattenBinds binds')) stuff)
  where
    go [] _ = thing_inside 	`thenSmpl` \ stuff ->
	      returnSmpl ([], stuff)
	
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

\begin{code}
addBind :: CoreBind -> OutStuff a -> OutStuff a
addBind bind    (binds,  res) = (bind:binds,     res)

addBinds :: [CoreBind] -> OutStuff a -> OutStuff a
addBinds []     stuff	      = stuff
addBinds binds1 (binds2, res) = (binds1++binds2, res)
\end{code}

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
		 simplExprC expr (Stop (substTy subst (coreExprType expr)))
	-- The type in the Stop continuation is usually not used
	-- It's only needed when discarding continuations after finding
	-- a function that returns bottom.
	-- Hence the lazy substitution

simplExprC :: CoreExpr -> SimplCont -> SimplM CoreExpr
	-- Simplify an expression, given a continuation

simplExprC expr cont = simplExprF expr cont	`thenSmpl` \ (floats, (_, body)) ->
  		       returnSmpl (mkLets floats body)

simplExprF :: InExpr -> SimplCont -> SimplM OutExprStuff
	-- Simplify an expression, returning floated binds

simplExprF (Var v) cont
  = simplVar v cont

simplExprF expr@(Con (PrimOp op) args) cont
  = getSubstEnv				`thenSmpl` \ se ->
    prepareArgs (ppr op)
		(primOpType op)
		(primOpStrictness op)
		(pushArgs se args cont)	$ \ args1 cont1 ->

    let
	-- Boring... we may have too many arguments now, so we push them back
	n_args = length args
	args2 = ASSERT( length args1 >= n_args )
		 take n_args args1
	cont2 = pushArgs emptySubstEnv (drop n_args args1) cont1
    in				
     	-- 	Try the prim op simplification
	-- It's really worth trying simplExpr again if it succeeds,
	-- because you can find
	--	case (eqChar# x 'a') of ...
	-- ==>  
	-- 	case (case x of 'a' -> True; other -> False) of ...

    case tryPrimOp op args2 of
	  Just e' -> zapSubstEnv (simplExprF e' cont2)
	  Nothing -> rebuild (Con (PrimOp op) args2) cont2


simplExprF (Con con@(DataCon _) args) cont
  = simplConArgs args		$ \ args' ->
    rebuild (Con con args') cont

simplExprF expr@(Con con@(Literal _) args) cont
  = ASSERT( null args )
    rebuild expr cont

simplExprF (App fun arg) cont
  = getSubstEnv		`thenSmpl` \ se ->
    simplExprF fun (ApplyTo NoDup arg se cont)

simplExprF (Case scrut bndr alts) cont
  = getSubstEnv		`thenSmpl` \ se ->
    simplExprF scrut (Select NoDup bndr alts se cont)


simplExprF (Let (Rec pairs) body) cont
  = simplIds (map fst pairs) 		$ \ bndrs' -> 
	-- NB: bndrs' don't have unfoldings or spec-envs
	-- We add them as we go down, using simplPrags

    simplRecBind False pairs bndrs' (simplExprF body cont)

simplExprF expr@(Lam _ _) cont = simplLam expr cont

simplExprF (Type ty) cont
  = ASSERT( case cont of { Stop _ -> True; ArgOf _ _ _ -> True; other -> False } )
    simplType ty	`thenSmpl` \ ty' ->
    rebuild (Type ty') cont

-- Comments about the Coerce case
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- It's worth checking for a coerce in the continuation,
-- in case we can cancel them.  For example, in the initial form of a worker
-- we may find 	(coerce T (coerce S (\x.e))) y
-- and we'd like it to simplify to e[y/x] in one round of simplification

simplExprF (Note (Coerce to from) e) (CoerceIt outer_to cont)
  = simplType from		`thenSmpl` \ from' ->
    if outer_to == from' then
	-- The coerces cancel out
	simplExprF e cont
    else
	-- They don't cancel, but the inner one is redundant
	simplExprF e (CoerceIt outer_to cont)

simplExprF (Note (Coerce to from) e) cont
  = simplType to		`thenSmpl` \ to' ->
    simplExprF e (CoerceIt to' cont)

-- hack: we only distinguish subsumed cost centre stacks for the purposes of
-- inlining.  All other CCCSs are mapped to currentCCS.
simplExprF (Note (SCC cc) e) cont
  = setEnclosingCC currentCCS $
    simplExpr e 	`thenSmpl` \ e ->
    rebuild (mkNote (SCC cc) e) cont

simplExprF (Note InlineCall e) cont
  = simplExprF e (InlinePlease cont)

-- Comments about the InlineMe case 
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

simplExprF (Note InlineMe e) cont
  = case cont of
	Stop _ -> 	-- Totally boring continuation
			-- Don't inline inside an INLINE expression
		  switchOffInlining (simplExpr e)	`thenSmpl` \ e' ->
		  rebuild (mkNote InlineMe e') cont

	other  -> 	-- Dissolve the InlineMe note if there's
			-- an interesting context of any kind to combine with
			-- (even a type application -- anything except Stop)
		  simplExprF e cont	

-- A non-recursive let is dealt with by simplBeta
simplExprF (Let (NonRec bndr rhs) body) cont
  = getSubstEnv			`thenSmpl` \ se ->
    simplBeta bndr rhs se (contResultType cont)	$
    simplExprF body cont
\end{code}


---------------------------------

\begin{code}
simplLam fun cont
  = go fun cont
  where
    zap_it  = mkLamBndrZapper fun cont
    cont_ty = contResultType cont

      	-- Type-beta reduction
    go (Lam bndr body) (ApplyTo _ (Type ty_arg) arg_se body_cont)
      =	ASSERT( isTyVar bndr )
	tick (BetaReduction bndr)		`thenSmpl_`
	getInScope				`thenSmpl` \ in_scope ->
	let
		ty' = substTy (mkSubst in_scope arg_se) ty_arg
	in
	seqType ty'	`seq`
	extendSubst bndr (DoneTy ty')
	(go body body_cont)

	-- Ordinary beta reduction
    go (Lam bndr body) cont@(ApplyTo _ arg arg_se body_cont)
      = tick (BetaReduction bndr)			`thenSmpl_`
	simplBeta zapped_bndr arg arg_se cont_ty
	(go body body_cont)
      where
	zapped_bndr = zap_it bndr

	-- Not enough args
    go lam@(Lam _ _) cont = completeLam [] lam cont

	-- Exactly enough args
    go expr cont = simplExprF expr cont

-- completeLam deals with the case where a lambda doesn't have an ApplyTo
-- continuation.  
-- We used to try for eta reduction here, but I found that this was
-- eta reducing things like 
--	f = \x -> (coerce (\x -> e))
-- This made f's arity reduce, which is a bad thing, so I removed the
-- eta reduction at this point, and now do it only when binding 
-- (at the call to postInlineUnconditionally

completeLam acc (Lam bndr body) cont
  = simplBinder bndr			$ \ bndr' ->
    completeLam (bndr':acc) body cont

completeLam acc body cont
  = simplExpr body 			`thenSmpl` \ body' ->
    rebuild (foldl (flip Lam) body' acc) cont
		-- Remember, acc is the *reversed* binders

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


---------------------------------
simplConArgs makes sure that the arguments all end up being atomic.
That means it may generate some Lets, hence the strange type

\begin{code}
simplConArgs :: [InArg] -> ([OutArg] -> SimplM OutExprStuff) -> SimplM OutExprStuff
simplConArgs args thing_inside
  = getSubst	`thenSmpl` \ subst ->
    go subst args thing_inside
  where
    go subst [] thing_inside 
	= thing_inside []
    go subst (arg:args) thing_inside 
	| exprIsTrivial arg
	= let
		arg1 = substExpr subst arg
		-- Simplify the RHS with inlining switched off, so that
		-- only absolutely essential things will happen.
		-- If we don't do this, consider:
		--	let x = e in C {x}
		-- We end up inlining x back into C's argument,
		-- and then let-binding it again!
		--
		-- It's important that the substitution *does* deal with case-binder synonyms:
		--	case x of y { True -> (x,1) }
		-- Here we must be sure to substitute y for x when simplifying the args of the pair,
		-- to increase the chances of being able to inline x.  The substituter will do
		-- that because the x->y mapping is held in the in-scope set.
	  in
	  ASSERT( exprIsTrivial arg1 )
	  go subst args				$ \ args1 ->
	  thing_inside (arg1 : args1)

	| otherwise
	= 	-- If the argument ain't trivial, then let-bind it
	  simplExpr arg				`thenSmpl` \ arg1 ->
	  newId (coreExprType arg1)		$ \ arg_id ->
	  go subst args				$ \ args1 ->
	  thing_inside (Var arg_id : args1)	`thenSmpl` \ res ->
	  returnSmpl (addBind (NonRec arg_id arg1) res)
		-- I used to use completeBeta but that was wrong, because
		-- arg_id isn't an InId
\end{code}


---------------------------------
\begin{code}
simplType :: InType -> SimplM OutType
simplType ty
  = getSubst	`thenSmpl` \ subst ->
    let
	new_ty = substTy subst ty
    in
    seqType new_ty `seq`  
    returnSmpl new_ty
\end{code}


%************************************************************************
%*									*
\subsection{Binding}
%*									*
%************************************************************************

@simplBeta@ is used for non-recursive lets in expressions, 
as well as true beta reduction.

Very similar to @simplLazyBind@, but not quite the same.

\begin{code}
simplBeta :: InId 			-- Binder
	  -> InExpr -> SubstEnv		-- Arg, with its subst-env
	  -> OutType			-- Type of thing computed by the context
	  -> SimplM OutExprStuff	-- The body
	  -> SimplM OutExprStuff
#ifdef DEBUG
simplBeta bndr rhs rhs_se cont_ty thing_inside
  | isTyVar bndr
  = pprPanic "simplBeta" (ppr bndr <+> ppr rhs)
#endif

simplBeta bndr rhs rhs_se cont_ty thing_inside
  | preInlineUnconditionally False {- not black listed -} bndr
  = tick (PreInlineUnconditionally bndr)		`thenSmpl_`
    extendSubst bndr (ContEx rhs_se rhs) thing_inside

  | otherwise
  =  	-- Simplify the RHS
    simplBinder bndr					$ \ bndr' ->
    simplArg (idType bndr') (getIdDemandInfo bndr)
	     rhs rhs_se cont_ty				$ \ rhs' ->

	-- Now complete the binding and simplify the body
    completeBeta bndr bndr' rhs' thing_inside

completeBeta bndr bndr' rhs' thing_inside
  | isUnLiftedType (idType bndr') && not (exprOkForSpeculation rhs')
	-- Make a case expression instead of a let
	-- These can arise either from the desugarer,
	-- or from beta reductions: (\x.e) (x +# y)
  = getInScope 			`thenSmpl` \ in_scope ->
    thing_inside		`thenSmpl` \ (floats, (_, body)) ->
    returnSmpl ([], (in_scope, Case rhs' bndr' [(DEFAULT, [], mkLets floats body)]))

  | otherwise
  = completeBinding bndr bndr' False False rhs' thing_inside
\end{code}


\begin{code}
simplArg :: OutType -> Demand
	 -> InExpr -> SubstEnv
	 -> OutType		-- Type of thing computed by the context
	 -> (OutExpr -> SimplM OutExprStuff)
	 -> SimplM OutExprStuff
simplArg arg_ty demand arg arg_se cont_ty thing_inside
  | isStrict demand || 
    isUnLiftedType arg_ty || 
    (opt_DictsStrict && isDictTy arg_ty && isDataType arg_ty)
	-- Return true only for dictionary types where the dictionary
	-- has more than one component (else we risk poking on the component
	-- of a newtype dictionary)
  = transformRhs arg			`thenSmpl` \ t_arg ->
    getEnv 				`thenSmpl` \ env ->
    setSubstEnv arg_se 				$
    simplExprF t_arg (ArgOf NoDup cont_ty 	$ \ rhs' ->
    setAllExceptInScope env			$
    etaFirst thing_inside rhs')

  | otherwise
  = simplRhs False {- Not top level -} 
	     True {- OK to float unboxed -}
	     arg_ty arg arg_se 
	     thing_inside
   
-- Do eta-reduction on the simplified RHS, if eta reduction is on
-- NB: etaCoreExpr only eta-reduces if that results in something trivial
etaFirst | opt_SimplDoEtaReduction = \ thing_inside rhs -> thing_inside (etaCoreExprToTrivial rhs)
	 | otherwise		   = \ thing_inside rhs -> thing_inside rhs

-- Try for eta reduction, but *only* if we get all
-- the way to an exprIsTrivial expression.    We don't want to remove
-- extra lambdas unless we are going to avoid allocating this thing altogether
etaCoreExprToTrivial rhs | exprIsTrivial rhs' = rhs'
			 | otherwise	      = rhs
			 where
			   rhs' = etaCoreExpr rhs
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
  |  (case occ_info of		-- This happens; for example, the case_bndr during case of
	IAmDead -> True		-- known constructor:  case (a,b) of x { (p,q) -> ... }
	other	-> False)	-- Here x isn't mentioned in the RHS, so we don't want to
				-- create the (dead) let-binding  let x = (a,b) in ...
  =  thing_inside

  |  postInlineUnconditionally black_listed occ_info old_bndr new_rhs
	-- Maybe we don't need a let-binding!  Maybe we can just
	-- inline it right away.  Unlike the preInlineUnconditionally case
	-- we are allowed to look at the RHS.
	--
	-- NB: a loop breaker never has postInlineUnconditionally True
	-- and non-loop-breakers only have *forward* references
	-- Hence, it's safe to discard the binding
	--	
	-- NB: You might think that postInlineUnconditionally is an optimisation,
	-- but if we have
	--	let x = f Bool in (x, y)
	-- then because of the constructor, x will not be *inlined* in the pair,
	-- so the trivial binding will stay.  But in this postInlineUnconditionally 
	-- gag we use the *substitution* to substitute (f Bool) for x, and that *will*
	-- happen.
  =  tick (PostInlineUnconditionally old_bndr)	`thenSmpl_`
     extendSubst old_bndr (DoneEx new_rhs)	
     thing_inside

  |  otherwise
  =  getSubst			`thenSmpl` \ subst ->
     let
	-- We make new IdInfo for the new binder by starting from the old binder, 
	-- doing appropriate substitutions.
	-- Then we add arity and unfolding info to get the new binder
	new_bndr_info = substIdInfo subst (idInfo old_bndr) (idInfo new_bndr)
		        `setArityInfo` ArityAtLeast (exprArity new_rhs)
			`setUnfoldingInfo` mkUnfolding top_lvl new_rhs

	final_id = new_bndr `setIdInfo` new_bndr_info
     in
	-- These seqs force the Ids, and hence the IdInfos, and hence any
	-- inner substitutions
     final_id	`seq`

     (modifyInScope new_bndr final_id thing_inside	`thenSmpl` \ stuff ->
      returnSmpl (addBind (NonRec final_id new_rhs) stuff))

  where
    occ_info = getIdOccInfo old_bndr
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
    getSubstEnv 					`thenSmpl` \ rhs_se ->
    simplRhs top_lvl False {- Not ok to float unboxed -}
	     (idType bndr')
	     rhs rhs_se  				$ \ rhs' ->

	-- Now compete the binding and simplify the body
    completeBinding bndr bndr' top_lvl black_listed rhs' thing_inside
\end{code}



\begin{code}
simplRhs :: Bool		-- True <=> Top level
	 -> Bool		-- True <=> OK to float unboxed (speculative) bindings
	 -> OutType -> InExpr -> SubstEnv
	 -> (OutExpr -> SimplM (OutStuff a))
	 -> SimplM (OutStuff a)
simplRhs top_lvl float_ubx rhs_ty rhs rhs_se thing_inside
  =    	-- Swizzle the inner lets past the big lambda (if any)
	-- and try eta expansion
    transformRhs rhs					`thenSmpl` \ t_rhs ->

	-- Simplify it
    setSubstEnv rhs_se (simplExprF t_rhs (Stop rhs_ty))	`thenSmpl` \ (floats, (in_scope', rhs')) ->

	-- Float lets out of RHS
    let
	(floats_out, rhs'') | float_ubx = (floats, rhs')
			    | otherwise	= splitFloats floats rhs' 
    in
    if (top_lvl || exprIsCheap rhs') && 	-- Float lets if (a) we're at the top level
        not (null floats_out)			-- or 		 (b) it exposes a cheap (i.e. duplicatable) expression
    then
	tickLetFloat floats_out				`thenSmpl_`
		-- Do the float
		-- 
		-- There's a subtlety here.  There may be a binding (x* = e) in the
		-- floats, where the '*' means 'will be demanded'.  So is it safe
		-- to float it out?  Answer no, but it won't matter because
		-- we only float if arg' is a WHNF,
		-- and so there can't be any 'will be demanded' bindings in the floats.
		-- Hence the assert
	WARN( any demanded_float floats_out, ppr floats_out )
	setInScope in_scope' (etaFirst thing_inside rhs'') 	`thenSmpl` \ stuff ->
		-- in_scope' may be excessive, but that's OK;
		-- it's a superset of what's in scope
	returnSmpl (addBinds floats_out stuff)
    else	
		-- Don't do the float
	etaFirst thing_inside (mkLets floats rhs')

-- In a let-from-let float, we just tick once, arbitrarily
-- choosing the first floated binder to identify it
tickLetFloat (NonRec b r      : fs) = tick (LetFloatFromLet b)
tickLetFloat (Rec ((b,r):prs) : fs) = tick (LetFloatFromLet b)
	
demanded_float (NonRec b r) = isStrict (getIdDemandInfo b) && not (isUnLiftedType (idType b))
		-- Unlifted-type (cheap-eagerness) lets may well have a demanded flag on them
demanded_float (Rec _)	    = False

-- Don't float any unlifted bindings out, because the context
-- is either a Rec group, or the top level, neither of which
-- can tolerate them.
splitFloats floats rhs
  = go floats
  where
    go []		    = ([], rhs)
    go (f:fs) | must_stay f = ([], mkLets (f:fs) rhs)
	      | otherwise   = case go fs of
				   (out, rhs') -> (f:out, rhs')

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
	DoneId var1 occ -> WARN( not (isInScope var1 subst) && isLocallyDefined var1 && not (mayHaveNoBinding var1),
				 text "simplVar:" <+> ppr var )
					-- The mayHaveNoBinding test accouunts for the fact
					-- that class dictionary constructors dont have top level
					-- bindings and hence aren't in scope.
			   finish_var var1 occ
  where
    finish_var var occ
      = getBlackList		`thenSmpl` \ black_list ->
	getInScope		`thenSmpl` \ in_scope ->
	completeCall black_list in_scope occ var cont

---------------------------------------------------------
--	Dealing with a call

completeCall black_list_fn in_scope occ var cont

	-- Look for an unfolding. There's a binding for the
	-- thing, but perhaps we want to inline it anyway
  | maybeToBool maybe_inline
  = tick (UnfoldingDone var)		`thenSmpl_`
    zapSubstEnv (completeInlining var unf_template discard_inline_cont)
		-- The template is already simplified, so don't re-substitute.
		-- This is VITAL.  Consider
		--	let x = e in
		--	let y = \z -> ...x... in
		--	\ x -> ...y...
		-- We'll clone the inner \x, adding x->x' in the id_subst
		-- Then when we inline y, we must *not* replace x by x' in
		-- the inlined copy!!
    
  | otherwise		-- No inlining
			-- Use prepareArgs to use function strictness
  = prepareArgs (ppr var) (idType var) (get_str var) cont	$ \ args' cont' ->

	-- Look for rules or specialisations that match
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
    getSwitchChecker 						`thenSmpl` \ chkr ->
    if switchIsOn chkr DontApplyRules then
	-- Don't try rules
	rebuild (mkApps (Var var) args') cont'
    else
	-- Try rules first
    case lookupRule in_scope var args' of
	Just (rule_name, rule_rhs) -> 
		tick (RuleFired rule_name)			`thenSmpl_`
		zapSubstEnv (simplExprF rule_rhs cont')
			-- See note above about zapping the substitution here
	
	Nothing -> rebuild (mkApps (Var var) args') cont'

  where
    get_str var = case getIdStrictness var of
			NoStrictnessInfo		  -> (repeat wwLazy, False)
			StrictnessInfo demands result_bot -> (demands, result_bot)

	---------- Unfolding stuff
    (subst_args, result_cont) = contArgs in_scope cont
    val_args    	      = filter isValArg subst_args
    arg_infos  		      = map (interestingArg in_scope) val_args
    inline_call	              = contIsInline result_cont
    interesting_cont          = contIsInteresting result_cont
    discard_inline_cont       | inline_call = discardInline cont
		              | otherwise   = cont

    maybe_inline  = callSiteInline black_listed inline_call occ
				   var arg_infos interesting_cont
    Just unf_template = maybe_inline
    black_listed      = black_list_fn var


-- An argument is interesting if it has *some* structure
-- We are here trying to avoid unfolding a function that
-- is applied only to variables that have no unfolding
-- (i.e. they are probably lambda bound): f x y z
-- There is little point in inlining f here.
interestingArg in_scope (Type _)	  = False
interestingArg in_scope (App fn (Type _)) = interestingArg in_scope fn
interestingArg in_scope (Var v)	          = hasSomeUnfolding (getIdUnfolding v')
					  where
					    v' = case lookupVarSet in_scope v of
							Just v' -> v'
							other   -> v
interestingArg in_scope other	          = True


-- First a special case
-- Don't actually inline the scrutinee when we see
--	case x of y { .... }
-- and x has unfolding (C a b).  Why not?  Because
-- we get a silly binding y = C a b.  If we don't
-- inline knownCon can directly substitute x for y instead.
completeInlining var (Con con con_args) (Select _ bndr alts se cont)
  | conOkForAlt con 
  = knownCon (Var var) con con_args bndr alts se cont

-- Now the normal case
completeInlining var unfolding cont
  = simplExprF unfolding cont

----------- costCentreOk
-- costCentreOk checks that it's ok to inline this thing
-- The time it *isn't* is this:
--
--	f x = let y = E in
--	      scc "foo" (...y...)
--
-- Here y has a "current cost centre", and we can't inline it inside "foo",
-- regardless of whether E is a WHNF or not.
    
costCentreOk ccs_encl cc_rhs
  =  not opt_SccProfilingOn
  || isSubsumedCCS ccs_encl	  -- can unfold anything into a subsumed scope
  || not (isEmptyCC cc_rhs)	  -- otherwise need a cc on the unfolding
\end{code}		   


\begin{code}
---------------------------------------------------------
--	Preparing arguments for a call

prepareArgs :: SDoc 	-- Error message info
	    -> OutType -> ([Demand],Bool) -> SimplCont
	    -> ([OutExpr] -> SimplCont -> SimplM OutExprStuff)
	    -> SimplM OutExprStuff

prepareArgs pp_fun orig_fun_ty (fun_demands, result_bot) orig_cont thing_inside
  = go [] demands orig_fun_ty orig_cont
  where
    not_enough_args = fun_demands `lengthExceeds` countValArgs orig_cont
	-- "No strictness info" is signalled by an infinite list of wwLazy
 
    demands | not_enough_args = repeat wwLazy			-- Not enough args, or no strictness
	    | result_bot      = fun_demands			-- Enough args, and function returns bottom
	    | otherwise	      = fun_demands ++ repeat wwLazy	-- Enough args and function does not return bottom
	-- NB: demands is finite iff enough args and result_bot is True

	-- Main game plan: loop through the arguments, simplifying
	-- each of them in turn.  We carry with us a list of demands,
	-- and the type of the function-applied-to-earlier-args

	-- Type argument
    go acc ds fun_ty (ApplyTo _ arg@(Type ty_arg) se cont)
	= getInScope		`thenSmpl` \ in_scope ->
	  let
		ty_arg' = substTy (mkSubst in_scope se) ty_arg
		res_ty  = applyTy fun_ty ty_arg'
	  in
	  seqType ty_arg'	`seq`
	  go (Type ty_arg' : acc) ds res_ty cont

	-- Value argument
    go acc (d:ds) fun_ty (ApplyTo _ val_arg se cont)
	= case splitFunTy_maybe fun_ty of {
		Nothing -> pprTrace "prepareArgs" (pp_fun $$ ppr orig_fun_ty $$ ppr orig_cont) 
			   (thing_inside (reverse acc) cont) ;
		Just (arg_ty, res_ty) ->
	  simplArg arg_ty d val_arg se (contResultType cont) 	$ \ arg' ->
	  go (arg':acc) ds res_ty cont }

	-- We've run out of demands, which only happens for functions
	-- we *know* now return bottom
	-- This deals with
	--	* case (error "hello") of { ... }
	--	* (error "Hello") arg
	--	* f (error "Hello") where f is strict
	--	etc
    go acc [] fun_ty cont = tick_case_of_error cont		`thenSmpl_`
    			    thing_inside (reverse acc) (discardCont cont)

	-- We're run out of arguments
    go acc ds fun_ty cont = thing_inside (reverse acc) cont

-- Boring: we must only record a tick if there was an interesting
--	   continuation to discard.  If not, we tick forever.
tick_case_of_error (Stop _)		 = returnSmpl ()
tick_case_of_error (CoerceIt _ (Stop _)) = returnSmpl ()
tick_case_of_error other		 = tick BottomFound
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
		   x = f x
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
  | otherwise = case getIdOccInfo bndr of
	  	  OneOcc in_lam once -> not in_lam && once
			-- Not inside a lambda, one occurrence ==> safe!
		  other 	     -> False


postInlineUnconditionally :: Bool  	-- Black listed
			  -> OccInfo
			  -> InId -> OutExpr -> Bool
	-- Examines a (bndr = rhs) binding, AFTER the rhs has been simplified
	-- It returns True if it's ok to discard the binding and inline the
	-- RHS at every use site.

	-- NOTE: This isn't our last opportunity to inline.
	-- We're at the binding site right now, and
	-- we'll get another opportunity when we get to the ocurrence(s)

postInlineUnconditionally black_listed occ_info bndr rhs
  | isExportedId bndr	|| 
    black_listed 	|| 
    loop_breaker	= False			-- Don't inline these
  | otherwise	        = exprIsTrivial rhs	-- Duplicating is free
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
  where
    loop_breaker = case occ_info of
			IAmALoopBreaker -> True
			other		-> False
\end{code}



%************************************************************************
%*									*
\subsection{The main rebuilder}
%*									*
%************************************************************************

\begin{code}
-------------------------------------------------------------------
-- Finish rebuilding
rebuild_done expr
  = getInScope			`thenSmpl` \ in_scope ->
    returnSmpl ([], (in_scope, expr))

---------------------------------------------------------
rebuild :: OutExpr -> SimplCont -> SimplM OutExprStuff

--	Stop continuation
rebuild expr (Stop _) = rebuild_done expr

--	ArgOf continuation
rebuild expr (ArgOf _ _ cont_fn) = cont_fn expr

--	ApplyTo continuation
rebuild expr cont@(ApplyTo _ arg se cont')
  = setSubstEnv se (simplExpr arg)	`thenSmpl` \ arg' ->
    rebuild (App expr arg') cont'

--	Coerce continuation
rebuild expr (CoerceIt to_ty cont)
  = rebuild (mkCoerce to_ty expr) cont

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
-- 	Case of known constructor or literal

rebuild_case scrut@(Con con args) bndr alts se cont
  | conOkForAlt con	-- Knocks out PrimOps and NoRepLits
  = knownCon scrut con args bndr alts se cont

---------------------------------------------------------
-- 	Eliminate the case if possible

rebuild_case scrut bndr alts se cont
  | 	-- Check that the RHSs are all the same, and
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

--    && opt_SimplDoCaseElim
--	[June 99; don't test this flag.  The code generator dies if it sees
--		case (\x.e) of f -> ...  
--	so better to always do it

   	-- Get rid of the case altogether
	-- See the extensive notes on case-elimination above
	-- Remember to bind the binder though!
  = tick (CaseElim bndr)			`thenSmpl_` (
    setSubstEnv se				$			
    simplBinder bndr				$ \ bndr' ->
    completeBinding bndr bndr' False False scrut 	$
    simplExprF rhs1 cont)

  where
    (rhs1:other_rhss)		 = [rhs | (_,_,rhs) <- alts]
    binders_unused (_, bndrs, _) = all isDeadBinder bndrs

    var_demanded_later (Var v) = isStrict (getIdDemandInfo bndr)	-- It's going to be evaluated later
    var_demanded_later other   = False

---------------------------------------------------------
-- 	Case of something else

rebuild_case scrut case_bndr alts se cont
  = 	-- Prepare case alternatives
    prepareCaseAlts case_bndr (splitTyConApp_maybe (idType case_bndr))
		    scrut_cons alts		`thenSmpl` \ better_alts ->
    
	-- Set the new subst-env in place (before dealing with the case binder)
    setSubstEnv se				$

	-- Deal with the case binder, and prepare the continuation;
	-- The new subst_env is in place
    prepareCaseCont better_alts cont		$ \ cont' ->
	

	-- Deal with variable scrutinee
    (	simplCaseBinder scrut case_bndr 	$ \ case_bndr' zap_occ_info ->

	-- Deal with the case alternatives
	simplAlts zap_occ_info scrut_cons 
	          case_bndr' better_alts cont'	`thenSmpl` \ alts' ->

	mkCase scrut case_bndr' alts'
    )						`thenSmpl` \ case_expr ->

	-- Notice that the simplBinder, prepareCaseCont, etc, do *not* scope
	-- over the rebuild_done; rebuild_done returns the in-scope set, and
	-- that should not include these chaps!
    rebuild_done case_expr	
  where
	-- scrut_cons tells what constructors the scrutinee can't possibly match
    scrut_cons = case scrut of
		   Var v -> otherCons (getIdUnfolding v)
		   other -> []


knownCon expr con args bndr alts se cont
  = tick (KnownBranch bndr)	`thenSmpl_`
    setSubstEnv se		(
    simplBinder bndr		$ \ bndr' ->
    case findAlt con alts of
	(DEFAULT, bs, rhs)     -> ASSERT( null bs )
				  completeBinding bndr bndr' False False expr $
					-- Don't use completeBeta here.  The expr might be
					-- an unboxed literal, like 3, or a variable
					-- whose unfolding is an unboxed literal... and
					-- completeBeta will just construct another case
					-- expression!
			          simplExprF rhs cont

	(Literal lit, bs, rhs) -> ASSERT( null bs )
				  extendSubst bndr (DoneEx expr)	$
					-- Unconditionally substitute, because expr must
					-- be a variable or a literal.  It can't be a
					-- NoRep literal because they don't occur in
					-- case patterns.
				  simplExprF rhs cont

	(DataCon dc, bs, rhs)  -> ASSERT( length bs == length real_args )
				  completeBinding bndr bndr' False False expr	$
					-- See note above
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

simplCaseBinder checks whether the scrutinee is a variable, v.
If so, try to eliminate uses of v in the RHSs in favour of case_bndr; 
that way, there's a chance that v will now only be used once, and hence inlined.

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
simplCaseBinder (Var v) case_bndr thing_inside
  = simplBinder (zap case_bndr)					$ \ case_bndr' ->
    modifyInScope v case_bndr'					$
	-- We could extend the substitution instead, but it would be
	-- a hack because then the substitution wouldn't be idempotent
	-- any more (v is an OutId).  And this just just as well.
    thing_inside case_bndr' zap
  where
    zap b = b `setIdOccInfo` NoOccInfo
	    
simplCaseBinder other_scrut case_bndr thing_inside
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
		   in
    		   newIds (dataConArgTys
				data_con
				(inst_tys ++ mkTyVarTys ex_tyvars'))		$ \ bndrs ->
		   returnSmpl ((DataCon data_con, ex_tyvars' ++ bndrs, rhs) : alts_no_deflt)

	other -> returnSmpl filtered_alts
  where
	-- Filter out alternatives that can't possibly match
    filtered_alts = case scrut_cons of
			[]    -> alts
			other -> [alt | alt@(con,_,_) <- alts, not (con `elem` scrut_cons)]

    missing_cons = [data_con | data_con <- tyConDataCons tycon, 
			       not (data_con `elem` handled_data_cons)]
    handled_data_cons = [data_con | DataCon data_con         <- scrut_cons] ++
			[data_con | (DataCon data_con, _, _) <- filtered_alts]

-- The default case
prepareCaseAlts _ _ scrut_cons alts
  = returnSmpl alts			-- Functions


----------------------
simplAlts zap_occ_info scrut_cons case_bndr' alts cont'
  = mapSmpl simpl_alt alts
  where
    inst_tys' = case splitTyConApp_maybe (idType case_bndr') of
			Just (tycon, inst_tys) -> inst_tys

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

		-- Bind the case-binder to (Con args)
	  let
		con_app = Con con (map Type inst_tys' ++ map varToCoreExpr vs')
	  in
	  modifyInScope case_bndr' (case_bndr' `setIdUnfolding` mkUnfolding False con_app)	$
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

    add_evals (DataCon dc) vs = cat_evals vs (dataConRepStrictness dc)
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
    newId join_arg_ty					( \ arg_id ->
	cont_fn (Var arg_id)				`thenSmpl` \ (binds, (_, rhs)) ->
	returnSmpl (Lam (setOneShotLambda arg_id) (mkLets binds rhs))
    )							`thenSmpl` \ join_rhs ->
   
	-- Build the join Id and continuation
    newId (coreExprType join_rhs)		$ \ join_id ->
    let
	new_cont = ArgOf OkToDup cont_ty
			 (\arg' -> rebuild_done (App (Var join_id) arg'))
    in

    tick (CaseOfCase join_id)						`thenSmpl_`
	-- Want to tick here so that we go round again,
	-- and maybe copy or inline the code;
	-- not strictly CaseOf Case
    thing_inside new_cont		`thenSmpl` \ res ->
    returnSmpl (addBind (NonRec join_id join_rhs) res)

mkDupableCont ty (ApplyTo _ arg se cont) thing_inside
  = mkDupableCont (funResultTy ty) cont 		$ \ cont' ->
    setSubstEnv se (simplExpr arg)			`thenSmpl` \ arg' ->
    if exprIsDupable arg' then
	thing_inside (ApplyTo OkToDup arg' emptySubstEnv cont')
    else
    newId (coreExprType arg')						$ \ bndr ->

    tick (CaseOfCase bndr)						`thenSmpl_`
	-- Want to tick here so that we go round again,
	-- and maybe copy or inline the code;
	-- not strictly CaseOf Case
    thing_inside (ApplyTo OkToDup (Var bndr) emptySubstEnv cont')	`thenSmpl` \ res ->
    returnSmpl (addBind (NonRec bndr arg') res)

mkDupableCont ty (Select _ case_bndr alts se cont) thing_inside
  = tick (CaseOfCase case_bndr)						`thenSmpl_`
    setSubstEnv se (
	simplBinder case_bndr						$ \ case_bndr' ->
	prepareCaseCont alts cont					$ \ cont' ->
	mapAndUnzipSmpl (mkDupableAlt case_bndr case_bndr' cont') alts	`thenSmpl` \ (alt_binds_s, alts') ->
	returnSmpl (concat alt_binds_s, alts')
    )					`thenSmpl` \ (alt_binds, alts') ->

    extendInScopes [b | NonRec b _ <- alt_binds]		$

	-- NB that the new alternatives, alts', are still InAlts, using the original
	-- binders.  That means we can keep the case_bndr intact. This is important
	-- because another case-of-case might strike, and so we want to keep the
	-- info that the case_bndr is dead (if it is, which is often the case).
	-- This is VITAL when the type of case_bndr is an unboxed pair (often the
	-- case in I/O rich code.  We aren't allowed a lambda bound
	-- arg of unboxed tuple type, and indeed such a case_bndr is always dead
    thing_inside (Select OkToDup case_bndr alts' se (Stop (contResultType cont)))	`thenSmpl` \ res ->

    returnSmpl (addBinds alt_binds res)


mkDupableAlt :: InId -> OutId -> SimplCont -> InAlt -> SimplM (OutStuff InAlt)
mkDupableAlt case_bndr case_bndr' cont alt@(con, bndrs, rhs)
  = simplBinders bndrs					$ \ bndrs' ->
    simplExprC rhs cont					`thenSmpl` \ rhs' ->

    if (case cont of { Stop _ -> exprIsDupable rhs'; other -> False}) then
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
	returnSmpl ([], alt)

    else
    let
	rhs_ty' = coreExprType rhs'
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

	then newId realWorldStatePrimTy  $ \ rw_id ->
	     returnSmpl ([rw_id], [Var realWorldPrimId])
	else 
	     returnSmpl (used_bndrs', map varToCoreExpr used_bndrs)
    )
	`thenSmpl` \ (final_bndrs', final_args) ->

    newId (foldr (mkFunTy . idType) rhs_ty' final_bndrs')	$ \ join_bndr ->

	-- Notice that we make the lambdas into one-shot-lambdas.  The
	-- join point is sure to be applied at most once, and doing so
	-- prevents the body of the join point being floated out by
	-- the full laziness pass
    returnSmpl ([NonRec join_bndr (mkLams (map setOneShotLambda final_bndrs') rhs')],
		(con, bndrs, mkApps (Var join_bndr) final_args))
\end{code}
