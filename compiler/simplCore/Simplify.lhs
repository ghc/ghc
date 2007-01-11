%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[Simplify]{The main module of the simplifier}

\begin{code}
module Simplify ( simplTopBinds, simplExpr ) where

#include "HsVersions.h"

import DynFlags	( dopt, DynFlag(Opt_D_dump_inlinings),
			  SimplifierSwitch(..)
			)
import SimplMonad
import Type hiding	( substTy, extendTvSubst )
import SimplEnv	
import SimplUtils
import Id
import IdInfo
import Coercion
import TcGadt		( dataConCanMatch )
import DataCon		( dataConTyCon, dataConRepStrictness )
import TyCon		( tyConArity, isAlgTyCon, isNewTyCon, tyConDataCons_maybe )
import CoreSyn
import PprCore		( pprParendExpr, pprCoreExpr )
import CoreUnfold	( mkUnfolding, callSiteInline )
import CoreUtils
import Rules		( lookupRule )
import BasicTypes	( isMarkedStrict )
import CostCentre	( currentCCS )
import TysPrim		( realWorldStatePrimTy )
import PrelInfo		( realWorldPrimId )
import BasicTypes	( TopLevelFlag(..), isTopLevel, 
			  RecFlag(..), isNonRuleLoopBreaker )
import List		( nub )
import Maybes		( orElse )
import Outputable
import Util
\end{code}


The guts of the simplifier is in this module, but the driver loop for
the simplifier is in SimplCore.lhs.


-----------------------------------------
	*** IMPORTANT NOTE ***
-----------------------------------------
The simplifier used to guarantee that the output had no shadowing, but
it does not do so any more.   (Actually, it never did!)  The reason is
documented with simplifyArgs.


-----------------------------------------
	*** IMPORTANT NOTE ***
-----------------------------------------
Many parts of the simplifier return a bunch of "floats" as well as an
expression. This is wrapped as a datatype SimplUtils.FloatsWith.

All "floats" are let-binds, not case-binds, but some non-rec lets may
be unlifted (with RHS ok-for-speculation).



-----------------------------------------
	ORGANISATION OF FUNCTIONS
-----------------------------------------
simplTopBinds
  - simplify all top-level binders
  - for NonRec, call simplRecOrTopPair
  - for Rec,    call simplRecBind

	
	------------------------------
simplExpr (applied lambda)	==> simplNonRecBind
simplExpr (Let (NonRec ...) ..) ==> simplNonRecBind
simplExpr (Let (Rec ...)    ..) ==> simplify binders; simplRecBind

	------------------------------
simplRecBind	[binders already simplfied]
  - use simplRecOrTopPair on each pair in turn

simplRecOrTopPair [binder already simplified]
  Used for: recursive bindings (top level and nested)
	    top-level non-recursive bindings
  Returns: 
  - check for PreInlineUnconditionally
  - simplLazyBind

simplNonRecBind
  Used for: non-top-level non-recursive bindings
	    beta reductions (which amount to the same thing)
  Because it can deal with strict arts, it takes a 
	"thing-inside" and returns an expression

  - check for PreInlineUnconditionally
  - simplify binder, including its IdInfo
  - if strict binding
	simplStrictArg
	mkAtomicArgs
	completeNonRecX
    else
	simplLazyBind
	addFloats

simplNonRecX:	[given a *simplified* RHS, but an *unsimplified* binder]
  Used for: binding case-binder and constr args in a known-constructor case
  - check for PreInLineUnconditionally
  - simplify binder
  - completeNonRecX
 
	------------------------------
simplLazyBind:	[binder already simplified, RHS not]
  Used for: recursive bindings (top level and nested)
	    top-level non-recursive bindings
	    non-top-level, but *lazy* non-recursive bindings
	[must not be strict or unboxed]
  Returns floats + an augmented environment, not an expression
  - substituteIdInfo and add result to in-scope 
	[so that rules are available in rec rhs]
  - simplify rhs
  - mkAtomicArgs
  - float if exposes constructor or PAP
  - completeBind


completeNonRecX:	[binder and rhs both simplified]
  - if the the thing needs case binding (unlifted and not ok-for-spec)
	build a Case
   else
	completeBind
	addFloats

completeBind: 	[given a simplified RHS]
	[used for both rec and non-rec bindings, top level and not]
  - try PostInlineUnconditionally
  - add unfolding [this is the only place we add an unfolding]
  - add arity



Right hand sides and arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In many ways we want to treat 
	(a) the right hand side of a let(rec), and 
	(b) a function argument
in the same way.  But not always!  In particular, we would
like to leave these arguments exactly as they are, so they
will match a RULE more easily.
	
	f (g x, h x)	
	g (+ x)

It's harder to make the rule match if we ANF-ise the constructor,
or eta-expand the PAP:

	f (let { a = g x; b = h x } in (a,b))
	g (\y. + x y)

On the other hand if we see the let-defns

	p = (g x, h x)
	q = + x

then we *do* want to ANF-ise and eta-expand, so that p and q
can be safely inlined.   

Even floating lets out is a bit dubious.  For let RHS's we float lets
out if that exposes a value, so that the value can be inlined more vigorously.
For example

	r = let x = e in (x,x)

Here, if we float the let out we'll expose a nice constructor. We did experiments
that showed this to be a generally good thing.  But it was a bad thing to float
lets out unconditionally, because that meant they got allocated more often.

For function arguments, there's less reason to expose a constructor (it won't
get inlined).  Just possibly it might make a rule match, but I'm pretty skeptical.
So for the moment we don't float lets out of function arguments either.


Eta expansion
~~~~~~~~~~~~~~
For eta expansion, we want to catch things like

	case e of (a,b) -> \x -> case a of (p,q) -> \y -> r

If the \x was on the RHS of a let, we'd eta expand to bring the two
lambdas together.  And in general that's a good thing to do.  Perhaps
we should eta expand wherever we find a (value) lambda?  Then the eta
expansion at a let RHS can concentrate solely on the PAP case.


%************************************************************************
%*									*
\subsection{Bindings}
%*									*
%************************************************************************

\begin{code}
simplTopBinds :: SimplEnv -> [InBind] -> SimplM [OutBind]

simplTopBinds env binds
  = do	{ 	-- Put all the top-level binders into scope at the start
		-- so that if a transformation rule has unexpectedly brought
		-- anything into scope, then we don't get a complaint about that.
		-- It's rather as if the top-level binders were imported.
	; env <- simplRecBndrs env (bindersOfBinds binds)
	; dflags <- getDOptsSmpl
	; let dump_flag = dopt Opt_D_dump_inlinings dflags || 
			  dopt Opt_D_dump_rule_firings dflags
	; env' <- simpl_binds dump_flag env binds
	; freeTick SimplifierDone
	; return (getFloats env') }
  where
	-- We need to track the zapped top-level binders, because
	-- they should have their fragile IdInfo zapped (notably occurrence info)
	-- That's why we run down binds and bndrs' simultaneously.
	--
	-- The dump-flag emits a trace for each top-level binding, which
	-- helps to locate the tracing for inlining and rule firing
    simpl_binds :: Bool -> SimplEnv -> [InBind] -> SimplM SimplEnv
    simpl_binds dump env []	      = return env
    simpl_binds dump env (bind:binds) = do { env' <- trace dump bind $
						     simpl_bind env bind
					   ; simpl_binds dump env' binds }

    trace True  bind = pprTrace "SimplBind" (ppr (bindersOf bind))
    trace False bind = \x -> x

    simpl_bind env (NonRec b r) = simplRecOrTopPair env TopLevel b r
    simpl_bind env (Rec pairs)  = simplRecBind      env TopLevel pairs
\end{code}


%************************************************************************
%*									*
\subsection{Lazy bindings}
%*									*
%************************************************************************

simplRecBind is used for
	* recursive bindings only

\begin{code}
simplRecBind :: SimplEnv -> TopLevelFlag
	     -> [(InId, InExpr)]
	     -> SimplM SimplEnv
simplRecBind env top_lvl pairs
  = do	{ env' <- go (zapFloats env) pairs
	; return (env `addRecFloats` env') }
	-- addFloats adds the floats from env', 
	-- *and* updates env with the in-scope set from env'
  where
    go env [] = return env
	
    go env ((bndr, rhs) : pairs)
	= do { env <- simplRecOrTopPair env top_lvl bndr rhs
	     ; go env pairs }
\end{code}

simplOrTopPair is used for
	* recursive bindings (whether top level or not)
	* top-level non-recursive bindings

It assumes the binder has already been simplified, but not its IdInfo.

\begin{code}
simplRecOrTopPair :: SimplEnv
	     	  -> TopLevelFlag
	     	  -> InId -> InExpr	-- Binder and rhs
	     	  -> SimplM SimplEnv	-- Returns an env that includes the binding

simplRecOrTopPair env top_lvl bndr rhs
  | preInlineUnconditionally env top_lvl bndr rhs  	-- Check for unconditional inline
  = do	{ tick (PreInlineUnconditionally bndr)
	; return (extendIdSubst env bndr (mkContEx env rhs)) }

  | otherwise
  = do	{ let bndr' = lookupRecBndr env bndr
	      (env', bndr'') = addLetIdInfo env bndr bndr'
	; simplLazyBind env' top_lvl Recursive bndr bndr'' rhs env' }
	-- May not actually be recursive, but it doesn't matter
\end{code}


simplLazyBind is used for
  * [simplRecOrTopPair] recursive bindings (whether top level or not)
  * [simplRecOrTopPair] top-level non-recursive bindings
  * [simplNonRecE]	non-top-level *lazy* non-recursive bindings

Nota bene:
    1. It assumes that the binder is *already* simplified, 
       and is in scope, and its IdInfo too, except unfolding

    2. It assumes that the binder type is lifted.

    3. It does not check for pre-inline-unconditionallly;
       that should have been done already.

\begin{code}
simplLazyBind :: SimplEnv
	      -> TopLevelFlag -> RecFlag
	      -> InId -> OutId		-- Binder, both pre-and post simpl
					-- The OutId has IdInfo, except arity, unfolding
	      -> InExpr -> SimplEnv 	-- The RHS and its environment
	      -> SimplM SimplEnv

simplLazyBind env top_lvl is_rec bndr bndr1 rhs rhs_se
  = do	{ let	rhs_env  = rhs_se `setInScope` env
		rhs_cont = mkRhsStop (idType bndr1)

  	-- Simplify the RHS; note the mkRhsStop, which tells 
	-- the simplifier that this is the RHS of a let.
	; (rhs_env1, rhs1) <- simplExprF rhs_env rhs rhs_cont

	-- If any of the floats can't be floated, give up now
	-- (The canFloat predicate says True for empty floats.)
	; if (not (canFloat top_lvl is_rec False rhs_env1))
	  then	completeBind env top_lvl bndr bndr1
				 (wrapFloats rhs_env1 rhs1)
	  else do
	-- ANF-ise a constructor or PAP rhs
	{ (rhs_env2, rhs2) <- prepareRhs rhs_env1 rhs1
	; (env', rhs3) <- chooseRhsFloats top_lvl is_rec False env rhs_env2 rhs2
	; completeBind env' top_lvl bndr bndr1 rhs3 } }

chooseRhsFloats :: TopLevelFlag -> RecFlag -> Bool
	     	-> SimplEnv	-- Env for the let
	     	-> SimplEnv	-- Env for the RHS, with RHS floats in it
	     	-> OutExpr		-- ..and the RHS itself
	     	-> SimplM (SimplEnv, OutExpr)	-- New env for let, and RHS

chooseRhsFloats top_lvl is_rec is_strict env rhs_env rhs
  | not (isEmptyFloats rhs_env) 		-- Something to float
  , canFloat top_lvl is_rec is_strict rhs_env	-- ...that can float
  , (isTopLevel top_lvl  || exprIsCheap rhs)	-- ...and we want to float	
  = do	{ tick LetFloatFromLet	-- Float
	; return (addFloats env rhs_env, rhs) }	-- Add the floats to the main env
  | otherwise			-- Don't float
  = return (env, wrapFloats rhs_env rhs)	-- Wrap the floats around the RHS
\end{code}


%************************************************************************
%*									*
\subsection{simplNonRec}
%*									*
%************************************************************************

A specialised variant of simplNonRec used when the RHS is already simplified, 
notably in knownCon.  It uses case-binding where necessary.

\begin{code}
simplNonRecX :: SimplEnv
	     -> InId 		-- Old binder
	     -> OutExpr		-- Simplified RHS
	     -> SimplM SimplEnv

simplNonRecX env bndr new_rhs
  = do	{ (env, bndr') <- simplBinder env bndr
	; completeNonRecX env NotTopLevel NonRecursive
			  (isStrictBndr bndr) bndr bndr' new_rhs }

completeNonRecX :: SimplEnv
		-> TopLevelFlag -> RecFlag -> Bool
	        -> InId 		-- Old binder
		-> OutId		-- New binder
	     	-> OutExpr		-- Simplified RHS
	     	-> SimplM SimplEnv

completeNonRecX env top_lvl is_rec is_strict old_bndr new_bndr new_rhs
  = do 	{ (env1, rhs1) <- prepareRhs (zapFloats env) new_rhs
	; (env2, rhs2) <- chooseRhsFloats top_lvl is_rec is_strict env env1 rhs1
	; completeBind env2 NotTopLevel old_bndr new_bndr rhs2 }
\end{code}

{- No, no, no!  Do not try preInlineUnconditionally in completeNonRecX
   Doing so risks exponential behaviour, because new_rhs has been simplified once already
   In the cases described by the folowing commment, postInlineUnconditionally will 
   catch many of the relevant cases.
  	-- This happens; for example, the case_bndr during case of
	-- known constructor:  case (a,b) of x { (p,q) -> ... }
	-- Here x isn't mentioned in the RHS, so we don't want to
	-- create the (dead) let-binding  let x = (a,b) in ...
	--
	-- Similarly, single occurrences can be inlined vigourously
	-- e.g.  case (f x, g y) of (a,b) -> ....
	-- If a,b occur once we can avoid constructing the let binding for them.

   Furthermore in the case-binding case preInlineUnconditionally risks extra thunks
	-- Consider 	case I# (quotInt# x y) of 
	--		  I# v -> let w = J# v in ...
	-- If we gaily inline (quotInt# x y) for v, we end up building an
	-- extra thunk:
	--		  let w = J# (quotInt# x y) in ...
	-- because quotInt# can fail.

  | preInlineUnconditionally env NotTopLevel bndr new_rhs
  = thing_inside (extendIdSubst env bndr (DoneEx new_rhs))
-}

prepareRhs takes a putative RHS, checks whether it's a PAP or
constructor application and, if so, converts it to ANF, so that the 
resulting thing can be inlined more easily.  Thus
	x = (f a, g b)
becomes
	t1 = f a
	t2 = g b
	x = (t1,t2)

\begin{code}
prepareRhs :: SimplEnv -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Adds new floats to the env iff that allows us to return a good RHS

prepareRhs env (Cast rhs co)	-- Note [Float coercions]
  = do	{ (env', rhs') <- makeTrivial env rhs
	; return (env', Cast rhs' co) }

prepareRhs env rhs
  | (Var fun, args) <- collectArgs rhs		-- It's an application
  , let n_args = valArgCount args	
  , n_args > 0					-- ...but not a trivial one	
  , isDataConWorkId fun || n_args < idArity fun	-- ...and it's a constructor or PAP
  = go env (Var fun) args
  where
    go env fun [] 	    = return (env, fun)
    go env fun (arg : args) = do { (env', arg') <- makeTrivial env arg
				 ; go env' (App fun arg') args }

prepareRhs env rhs 		-- The default case
  = return (env, rhs)
\end{code}

Note [Float coercions]
~~~~~~~~~~~~~~~~~~~~~~
When we find the binding
	x = e `cast` co
we'd like to transform it to
	x' = e
	x = x `cast` co		-- A trivial binding
There's a chance that e will be a constructor application or function, or something
like that, so moving the coerion to the usage site may well cancel the coersions
and lead to further optimisation.  Example:

     data family T a :: *
     data instance T Int = T Int

     foo :: Int -> Int -> Int
     foo m n = ...
        where
          x = T m
          go 0 = 0
          go n = case x of { T m -> go (n-m) }
		-- This case should optimise


\begin{code}
makeTrivial :: SimplEnv -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Binds the expression to a variable, if it's not trivial, returning the variable
makeTrivial env expr
  | exprIsTrivial expr
  = return (env, expr)
  | otherwise		-- See Note [Take care] below
  = do 	{ var <- newId FSLIT("a") (exprType expr)
	; env <- completeNonRecX env NotTopLevel NonRecursive 
				 False var var expr
	; return (env, substExpr env (Var var)) }
\end{code}


%************************************************************************
%*									*
\subsection{Completing a lazy binding}
%*									*
%************************************************************************

completeBind
  * deals only with Ids, not TyVars
  * takes an already-simplified binder and RHS
  * is used for both recursive and non-recursive bindings
  * is used for both top-level and non-top-level bindings

It does the following:
  - tries discarding a dead binding
  - tries PostInlineUnconditionally
  - add unfolding [this is the only place we add an unfolding]
  - add arity

It does *not* attempt to do let-to-case.  Why?  Because it is used for
  - top-level bindings (when let-to-case is impossible) 
  - many situations where the "rhs" is known to be a WHNF
		(so let-to-case is inappropriate).

Nor does it do the atomic-argument thing

\begin{code}
completeBind :: SimplEnv
	     -> TopLevelFlag		-- Flag stuck into unfolding
	     -> InId 			-- Old binder
	     -> OutId -> OutExpr	-- New binder and RHS
	     -> SimplM SimplEnv
-- completeBind may choose to do its work 
--	* by extending the substitution (e.g. let x = y in ...)
--	* or by adding to the floats in the envt

completeBind env top_lvl old_bndr new_bndr new_rhs
  | postInlineUnconditionally env top_lvl new_bndr occ_info new_rhs unfolding
		-- Inline and discard the binding
  = do	{ tick (PostInlineUnconditionally old_bndr)
	; -- pprTrace "postInlineUnconditionally" (ppr old_bndr <+> ppr new_bndr <+> ppr new_rhs) $
	  return (extendIdSubst env old_bndr (DoneEx new_rhs)) }
	-- Use the substitution to make quite, quite sure that the
	-- substitution will happen, since we are going to discard the binding

  |  otherwise
  = let
	-- 	Arity info
  	new_bndr_info = idInfo new_bndr `setArityInfo` exprArity new_rhs

	-- 	Unfolding info
	-- Add the unfolding *only* for non-loop-breakers
	-- Making loop breakers not have an unfolding at all 
	-- means that we can avoid tests in exprIsConApp, for example.
	-- This is important: if exprIsConApp says 'yes' for a recursive
	-- thing, then we can get into an infinite loop

	-- 	Demand info
	-- If the unfolding is a value, the demand info may
	-- go pear-shaped, so we nuke it.  Example:
	--	let x = (a,b) in
	--	case x of (p,q) -> h p q x
	-- Here x is certainly demanded. But after we've nuked
	-- the case, we'll get just
	--	let x = (a,b) in h a b x
	-- and now x is not demanded (I'm assuming h is lazy)
	-- This really happens.  Similarly
	--	let f = \x -> e in ...f..f...
	-- After inlining f at some of its call sites the original binding may
	-- (for example) be no longer strictly demanded.
	-- The solution here is a bit ad hoc...
 	info_w_unf = new_bndr_info `setUnfoldingInfo` unfolding
        final_info | loop_breaker		= new_bndr_info
		   | isEvaldUnfolding unfolding = zapDemandInfo info_w_unf `orElse` info_w_unf
		   | otherwise			= info_w_unf

	final_id = new_bndr `setIdInfo` final_info
    in
		-- These seqs forces the Id, and hence its IdInfo,
		-- and hence any inner substitutions
    final_id					`seq`
    -- pprTrace "Binding" (ppr final_id <+> ppr unfolding) $
    return (addNonRec env final_id new_rhs)
  where 
    unfolding    = mkUnfolding (isTopLevel top_lvl) new_rhs
    loop_breaker = isNonRuleLoopBreaker occ_info
    old_info     = idInfo old_bndr
    occ_info     = occInfo old_info
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
simplExpr :: SimplEnv -> CoreExpr -> SimplM CoreExpr
simplExpr env expr = simplExprC env expr (mkBoringStop expr_ty')
		   where
		     expr_ty' = substTy env (exprType expr)
	-- The type in the Stop continuation, expr_ty', is usually not used
	-- It's only needed when discarding continuations after finding
	-- a function that returns bottom.
	-- Hence the lazy substitution


simplExprC :: SimplEnv -> CoreExpr -> SimplCont -> SimplM CoreExpr
	-- Simplify an expression, given a continuation
simplExprC env expr cont 
  = -- pprTrace "simplExprC" (ppr expr $$ ppr cont {- $$ ppr (seIdSubst env) -} $$ ppr (seFloats env) ) $
    do	{ (env', expr') <- simplExprF (zapFloats env) expr cont
	; -- pprTrace "simplExprC ret" (ppr expr $$ ppr expr') $
	  -- pprTrace "simplExprC ret3" (ppr (seInScope env')) $
	  -- pprTrace "simplExprC ret4" (ppr (seFloats env')) $
          return (wrapFloats env' expr') }

--------------------------------------------------
simplExprF :: SimplEnv -> InExpr -> SimplCont
	   -> SimplM (SimplEnv, OutExpr)

simplExprF env e cont 
  = -- pprTrace "simplExprF" (ppr e $$ ppr cont $$ ppr (seTvSubst env) $$ ppr (seIdSubst env) {- $$ ppr (seFloats env) -} ) $
    simplExprF' env e cont
    				     
simplExprF' env (Var v)	       cont = simplVar env v cont
simplExprF' env (Lit lit)      cont = rebuild env (Lit lit) cont
simplExprF' env (Note n expr)  cont = simplNote env n expr cont
simplExprF' env (Cast body co) cont = simplCast env body co cont
simplExprF' env (App fun arg)  cont = simplExprF env fun $
				      ApplyTo NoDup arg env cont

simplExprF' env expr@(Lam _ _) cont 
  = simplLam env (map zap bndrs) body cont
	-- The main issue here is under-saturated lambdas
	--   (\x1. \x2. e) arg1
	-- Here x1 might have "occurs-once" occ-info, because occ-info
	-- is computed assuming that a group of lambdas is applied
	-- all at once.  If there are too few args, we must zap the 
	-- occ-info.
  where
    n_args   = countArgs cont
    n_params = length bndrs
    (bndrs, body) = collectBinders expr
    zap | n_args >= n_params = \b -> b	
	| otherwise	     = \b -> if isTyVar b then b
				     else zapLamIdInfo b
	-- NB: we count all the args incl type args
	-- so we must count all the binders (incl type lambdas)

simplExprF' env (Type ty) cont
  = ASSERT( contIsRhsOrArg cont )
    do	{ ty' <- simplType env ty
	; rebuild env (Type ty') cont }

simplExprF' env (Case scrut bndr case_ty alts) cont
  | not (switchIsOn (getSwitchChecker env) NoCaseOfCase)
  = 	-- Simplify the scrutinee with a Select continuation
    simplExprF env scrut (Select NoDup bndr alts env cont)

  | otherwise
  = 	-- If case-of-case is off, simply simplify the case expression
	-- in a vanilla Stop context, and rebuild the result around it
    do	{ case_expr' <- simplExprC env scrut case_cont
	; rebuild env case_expr' cont }
  where
    case_cont = Select NoDup bndr alts env (mkBoringStop case_ty')
    case_ty'  = substTy env case_ty	-- c.f. defn of simplExpr

simplExprF' env (Let (Rec pairs) body) cont
  = do	{ env <- simplRecBndrs env (map fst pairs)
		-- NB: bndrs' don't have unfoldings or rules
		-- We add them as we go down

	; env <- simplRecBind env NotTopLevel pairs
	; simplExprF env body cont }

simplExprF' env (Let (NonRec bndr rhs) body) cont
  = simplNonRecE env bndr (rhs, env) ([], body) cont

---------------------------------
simplType :: SimplEnv -> InType -> SimplM OutType
	-- Kept monadic just so we can do the seqType
simplType env ty
  = -- pprTrace "simplType" (ppr ty $$ ppr (seTvSubst env)) $
    seqType new_ty   `seq`   returnSmpl new_ty
  where
    new_ty = substTy env ty
\end{code}


%************************************************************************
%*									*
\subsection{The main rebuilder}
%*									*
%************************************************************************

\begin{code}
rebuild :: SimplEnv -> OutExpr -> SimplCont -> SimplM (SimplEnv, OutExpr)
-- At this point the substitution in the SimplEnv should be irrelevant
-- only the in-scope set and floats should matter
rebuild env expr cont
  = -- pprTrace "rebuild" (ppr expr $$ ppr cont $$ ppr (seFloats env)) $
    case cont of
      Stop {}		      	   -> return (env, expr)
      CoerceIt co cont	      	   -> rebuild env (mkCoerce co expr) cont
      Select _ bndr alts se cont   -> rebuildCase (se `setFloats` env) expr bndr alts cont
      StrictArg fun ty info cont   -> rebuildCall env (fun `App` expr) (funResultTy ty) info cont
      StrictBind b bs body se cont -> do { env' <- simplNonRecX (se `setFloats` env) b expr
					 ; simplLam env' bs body cont }
      ApplyTo _ arg se cont	   -> do { arg' <- simplExpr (se `setInScope` env) arg
				         ; rebuild env (App expr arg') cont }
\end{code}


%************************************************************************
%*									*
\subsection{Lambdas}
%*									*
%************************************************************************

\begin{code}
simplCast :: SimplEnv -> InExpr -> Coercion -> SimplCont
	  -> SimplM (SimplEnv, OutExpr)
simplCast env body co cont
  = do	{ co' <- simplType env co
	; simplExprF env body (addCoerce co' cont) }
  where
       addCoerce co cont = add_coerce co (coercionKind co) cont

       add_coerce co (s1, k1) cont 	-- co :: ty~ty
         | s1 `coreEqType` k1 = cont	-- is a no-op

       add_coerce co1 (s1, k2) (CoerceIt co2 cont)
         | (l1, t1) <- coercionKind co2
                -- 	coerce T1 S1 (coerce S1 K1 e)
		-- ==>
		--	e, 			if T1=K1
		--	coerce T1 K1 e,		otherwise
		--
		-- For example, in the initial form of a worker
		-- we may find 	(coerce T (coerce S (\x.e))) y
		-- and we'd like it to simplify to e[y/x] in one round 
		-- of simplification
         , s1 `coreEqType` t1  = cont		 -- The coerces cancel out  
         | otherwise           = CoerceIt (mkTransCoercion co1 co2) cont
    
       add_coerce co (s1s2, t1t2) (ApplyTo dup arg arg_se cont)
         | not (isTypeArg arg)  -- This whole case only works for value args
	                        -- Could upgrade to have equiv thing for type apps too	
         , isFunTy s1s2	  -- t1t2 must be a function type, becuase it's applied
                -- co : s1s2 :=: t1t2
		--	(coerce (T1->T2) (S1->S2) F) E
		-- ===> 
		--	coerce T2 S2 (F (coerce S1 T1 E))
		--
		-- t1t2 must be a function type, T1->T2, because it's applied
		-- to something but s1s2 might conceivably not be
		--
		-- When we build the ApplyTo we can't mix the out-types
		-- with the InExpr in the argument, so we simply substitute
		-- to make it all consistent.  It's a bit messy.
		-- But it isn't a common case.
		--
		-- Example of use: Trac #995
         = ApplyTo dup new_arg (zapSubstEnv env) (addCoerce co2 cont)
         where
           -- we split coercion t1->t2 :=: s1->s2 into t1 :=: s1 and 
           -- t2 :=: s2 with left and right on the curried form: 
           --    (->) t1 t2 :=: (->) s1 s2
           [co1, co2] = decomposeCo 2 co
           new_arg    = mkCoerce (mkSymCoercion co1) arg'
	   arg'       = substExpr arg_se arg

       add_coerce co _ cont = CoerceIt co cont
\end{code}


%************************************************************************
%*									*
\subsection{Lambdas}
%*									*
%************************************************************************

\begin{code}
simplLam :: SimplEnv -> [InId] -> InExpr -> SimplCont
	 -> SimplM (SimplEnv, OutExpr)

simplLam env [] body cont = simplExprF env body cont

      	-- Type-beta reduction
simplLam env (bndr:bndrs) body (ApplyTo _ (Type ty_arg) arg_se cont)
  = ASSERT( isTyVar bndr )
    do	{ tick (BetaReduction bndr)
	; ty_arg' <- simplType (arg_se `setInScope` env) ty_arg
	; simplLam (extendTvSubst env bndr ty_arg') bndrs body cont }

	-- Ordinary beta reduction
simplLam env (bndr:bndrs) body (ApplyTo _ arg arg_se cont)
  = do	{ tick (BetaReduction bndr)	
	; simplNonRecE env bndr (arg, arg_se) (bndrs, body) cont }

	-- Not enough args, so there are real lambdas left to put in the result
simplLam env bndrs body cont
  = do	{ (env, bndrs') <- simplLamBndrs env bndrs
	; body' <- simplExpr env body
	; new_lam <- mkLam bndrs' body'
	; rebuild env new_lam cont }

------------------
simplNonRecE :: SimplEnv 
	     -> InId 			-- The binder
	     -> (InExpr, SimplEnv)	-- Rhs of binding (or arg of lambda)
	     -> ([InId], InExpr)	-- Body of the let/lambda
					--	\xs.e
	     -> SimplCont
	     -> SimplM (SimplEnv, OutExpr)

-- simplNonRecE is used for
--  * non-top-level non-recursive lets in expressions
--  * beta reduction
--
-- It deals with strict bindings, via the StrictBind continuation,
-- which may abort the whole process
--
-- The "body" of the binding comes as a pair of ([InId],InExpr)
-- representing a lambda; so we recurse back to simplLam
-- Why?  Because of the binder-occ-info-zapping done before 
-- 	 the call to simplLam in simplExprF (Lam ...)

simplNonRecE env bndr (rhs, rhs_se) (bndrs, body) cont
  | preInlineUnconditionally env NotTopLevel bndr rhs
  = do	{ tick (PreInlineUnconditionally bndr)
	; simplLam (extendIdSubst env bndr (mkContEx rhs_se rhs)) bndrs body cont }

  | isStrictBndr bndr
  = do	{ simplExprF (rhs_se `setFloats` env) rhs 
		     (StrictBind bndr bndrs body env cont) }

  | otherwise
  = do	{ (env, bndr') <- simplBinder env bndr
	; env <- simplLazyBind env NotTopLevel NonRecursive bndr bndr' rhs rhs_se
	; simplLam env bndrs body cont }
\end{code}


%************************************************************************
%*									*
\subsection{Notes}
%*									*
%************************************************************************

\begin{code}
-- Hack alert: we only distinguish subsumed cost centre stacks for the 
-- purposes of inlining.  All other CCCSs are mapped to currentCCS.
simplNote env (SCC cc) e cont
  = do 	{ e' <- simplExpr (setEnclosingCC env currentCCS) e
	; rebuild env (mkSCC cc e') cont }

-- See notes with SimplMonad.inlineMode
simplNote env InlineMe e cont
  | contIsRhsOrArg cont		-- Totally boring continuation; see notes above
  = do	{ 			-- Don't inline inside an INLINE expression
	  e' <- simplExpr (setMode inlineMode env) e
	; rebuild env (mkInlineMe e') cont }

  | otherwise  	-- Dissolve the InlineMe note if there's
		-- an interesting context of any kind to combine with
		-- (even a type application -- anything except Stop)
  = simplExprF env e cont

simplNote env (CoreNote s) e cont
  = simplExpr env e    `thenSmpl` \ e' ->
    rebuild env (Note (CoreNote s) e') cont
\end{code}


%************************************************************************
%*									*
\subsection{Dealing with calls}
%*									*
%************************************************************************

\begin{code}
simplVar env var cont
  = case substId env var of
	DoneEx e	 -> simplExprF (zapSubstEnv env) e cont
	ContEx tvs ids e -> simplExprF (setSubstEnv env tvs ids) e cont
	DoneId var1      -> completeCall (zapSubstEnv env) var1 cont
		-- Note [zapSubstEnv]
		-- The template is already simplified, so don't re-substitute.
		-- This is VITAL.  Consider
		--	let x = e in
		--	let y = \z -> ...x... in
		--	\ x -> ...y...
		-- We'll clone the inner \x, adding x->x' in the id_subst
		-- Then when we inline y, we must *not* replace x by x' in
		-- the inlined copy!!

---------------------------------------------------------
--	Dealing with a call site

completeCall env var cont
  = do	{ dflags <- getDOptsSmpl
	; let	(args,call_cont) = contArgs cont
		-- The args are OutExprs, obtained by *lazily* substituting
		-- in the args found in cont.  These args are only examined
		-- to limited depth (unless a rule fires).  But we must do
		-- the substitution; rule matching on un-simplified args would
		-- be bogus

	------------- First try rules ----------------
	-- Do this before trying inlining.  Some functions have 
	-- rules *and* are strict; in this case, we don't want to 
	-- inline the wrapper of the non-specialised thing; better
	-- to call the specialised thing instead.
	--
	-- We used to use the black-listing mechanism to ensure that inlining of 
	-- the wrapper didn't occur for things that have specialisations till a 
	-- later phase, so but now we just try RULES first
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
	; let	in_scope   = getInScope env
		rules	   = getRules env
		maybe_rule = case activeRule env of
				Nothing     -> Nothing	-- No rules apply
				Just act_fn -> lookupRule act_fn in_scope 
							  rules var args 
	; case maybe_rule of {
	    Just (rule, rule_rhs) -> 
		tick (RuleFired (ru_name rule))			`thenSmpl_`
		(if dopt Opt_D_dump_rule_firings dflags then
		   pprTrace "Rule fired" (vcat [
			text "Rule:" <+> ftext (ru_name rule),
			text "Before:" <+> ppr var <+> sep (map pprParendExpr args),
			text "After: " <+> pprCoreExpr rule_rhs,
			text "Cont:  " <+> ppr call_cont])
		 else
			id)		$
		simplExprF env rule_rhs (dropArgs (ruleArity rule) cont)
		-- The ruleArity says how many args the rule consumed
	
	  ; Nothing -> do	-- No rules

	------------- Next try inlining ----------------
	{ let	arg_infos = [interestingArg arg | arg <- args, isValArg arg]
		n_val_args = length arg_infos
	      	interesting_cont = interestingCallContext (notNull args)
						  	  (notNull arg_infos)
						  	  call_cont
	 	active_inline = activeInline env var
		maybe_inline  = callSiteInline dflags active_inline
				       var arg_infos interesting_cont
	; case maybe_inline of {
	    Just unfolding  	-- There is an inlining!
	      ->  do { tick (UnfoldingDone var)
		     ; (if dopt Opt_D_dump_inlinings dflags then
			   pprTrace "Inlining done" (vcat [
				text "Before:" <+> ppr var <+> sep (map pprParendExpr args),
				text "Inlined fn: " <+> nest 2 (ppr unfolding),
				text "Cont:  " <+> ppr call_cont])
			 else
				id)
		       simplExprF env unfolding cont }

	    ; Nothing -> 		-- No inlining!

	------------- No inlining! ----------------
	-- Next, look for rules or specialisations that match
	--
	rebuildCall env (Var var) (idType var) 
		    (mkArgInfo var n_val_args call_cont) cont
    }}}}

rebuildCall :: SimplEnv
	    -> OutExpr -> OutType	-- Function and its type
	    -> (Bool, [Bool])		-- See SimplUtils.mkArgInfo
	    -> SimplCont
	    -> SimplM (SimplEnv, OutExpr)
rebuildCall env fun fun_ty (has_rules, []) cont
  -- When we run out of strictness args, it means
  -- that the call is definitely bottom; see SimplUtils.mkArgInfo
  -- Then we want to discard the entire strict continuation.  E.g.
  --	* case (error "hello") of { ... }
  --	* (error "Hello") arg
  --	* f (error "Hello") where f is strict
  --	etc
  -- Then, especially in the first of these cases, we'd like to discard
  -- the continuation, leaving just the bottoming expression.  But the
  -- type might not be right, so we may have to add a coerce.
  | not (contIsTrivial cont)	 -- Only do thia if there is a non-trivial
  = return (env, mk_coerce fun)  -- contination to discard, else we do it
  where				 -- again and again!
    cont_ty = contResultType cont
    co      = mkUnsafeCoercion fun_ty cont_ty
    mk_coerce expr | cont_ty `coreEqType` fun_ty = fun
		   | otherwise = mkCoerce co fun

rebuildCall env fun fun_ty info (ApplyTo _ (Type arg_ty) se cont)
  = do	{ ty' <- simplType (se `setInScope` env) arg_ty
	; rebuildCall env (fun `App` Type ty') (applyTy fun_ty ty') info cont }

rebuildCall env fun fun_ty (has_rules, str:strs) (ApplyTo _ arg arg_se cont)
  | str || isStrictType arg_ty		-- Strict argument
  = -- pprTrace "Strict Arg" (ppr arg $$ ppr (seIdSubst env) $$ ppr (seInScope env)) $
    simplExprF (arg_se `setFloats` env) arg
	       (StrictArg fun fun_ty (has_rules, strs) cont)
		-- Note [Shadowing]

  | otherwise				-- Lazy argument
	-- DO NOT float anything outside, hence simplExprC
	-- There is no benefit (unlike in a let-binding), and we'd
	-- have to be very careful about bogus strictness through 
	-- floating a demanded let.
  = do	{ arg' <- simplExprC (arg_se `setInScope` env) arg
			     (mkLazyArgStop arg_ty has_rules)
	; rebuildCall env (fun `App` arg') res_ty (has_rules, strs) cont }
  where
    (arg_ty, res_ty) = splitFunTy fun_ty

rebuildCall env fun fun_ty info cont
  = rebuild env fun cont
\end{code}

Note [Shadowing]
~~~~~~~~~~~~~~~~
This part of the simplifier may break the no-shadowing invariant
Consider
	f (...(\a -> e)...) (case y of (a,b) -> e')
where f is strict in its second arg
If we simplify the innermost one first we get (...(\a -> e)...)
Simplifying the second arg makes us float the case out, so we end up with
	case y of (a,b) -> f (...(\a -> e)...) e'
So the output does not have the no-shadowing invariant.  However, there is
no danger of getting name-capture, because when the first arg was simplified
we used an in-scope set that at least mentioned all the variables free in its
static environment, and that is enough.

We can't just do innermost first, or we'd end up with a dual problem:
	case x of (a,b) -> f e (...(\a -> e')...)

I spent hours trying to recover the no-shadowing invariant, but I just could
not think of an elegant way to do it.  The simplifier is already knee-deep in
continuations.  We have to keep the right in-scope set around; AND we have
to get the effect that finding (error "foo") in a strict arg position will
discard the entire application and replace it with (error "foo").  Getting
all this at once is TOO HARD!

%************************************************************************
%*									*
		Rebuilding a cse expression
%*									*
%************************************************************************

Blob of helper functions for the "case-of-something-else" situation.

\begin{code}
---------------------------------------------------------
-- 	Eliminate the case if possible

rebuildCase :: SimplEnv
	    -> OutExpr		-- Scrutinee
	    -> InId		-- Case binder
	    -> [InAlt]		-- Alternatives (inceasing order)
	    -> SimplCont
	    -> SimplM (SimplEnv, OutExpr)

rebuildCase env scrut case_bndr alts cont
  | Just (con,args) <- exprIsConApp_maybe scrut	
	-- Works when the scrutinee is a variable with a known unfolding
	-- as well as when it's an explicit constructor application
  = knownCon env scrut (DataAlt con) args case_bndr alts cont

  | Lit lit <- scrut	-- No need for same treatment as constructors
			-- because literals are inlined more vigorously
  = knownCon env scrut (LitAlt lit) [] case_bndr alts cont

  | otherwise
  = do	{ 	-- Prepare the continuation;
		-- The new subst_env is in place
	  (env, dup_cont, nodup_cont) <- prepareCaseCont env alts cont

	-- Simplify the alternatives
	; (case_bndr', alts') <- simplAlts env scrut case_bndr alts dup_cont
	; let res_ty' = contResultType dup_cont
	; case_expr <- mkCase scrut case_bndr' res_ty' alts'

	-- Notice that rebuildDone returns the in-scope set from env, not alt_env
	-- The case binder *not* scope over the whole returned case-expression
	; rebuild env case_expr nodup_cont }
\end{code}

simplCaseBinder checks whether the scrutinee is a variable, v.  If so,
try to eliminate uses of v in the RHSs in favour of case_bndr; that
way, there's a chance that v will now only be used once, and hence
inlined.

Note [no-case-of-case]
~~~~~~~~~~~~~~~~~~~~~~
There is a time we *don't* want to do that, namely when
-fno-case-of-case is on.  This happens in the first simplifier pass,
and enhances full laziness.  Here's the bad case:
	f = \ y -> ...(case x of I# v -> ...(case x of ...) ... )
If we eliminate the inner case, we trap it inside the I# v -> arm,
which might prevent some full laziness happening.  I've seen this
in action in spectral/cichelli/Prog.hs:
	 [(m,n) | m <- [1..max], n <- [1..max]]
Hence the check for NoCaseOfCase.

Note [Suppressing the case binder-swap]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is another situation when it might make sense to suppress the
case-expression binde-swap. If we have

    case x of w1 { DEFAULT -> case x of w2 { A -> e1; B -> e2 }
	           ...other cases .... }

We'll perform the binder-swap for the outer case, giving

    case x of w1 { DEFAULT -> case w1 of w2 { A -> e1; B -> e2 } 
	           ...other cases .... }

But there is no point in doing it for the inner case, because w1 can't
be inlined anyway.  Furthermore, doing the case-swapping involves
zapping w2's occurrence info (see paragraphs that follow), and that
forces us to bind w2 when doing case merging.  So we get

    case x of w1 { A -> let w2 = w1 in e1
		   B -> let w2 = w1 in e2
	           ...other cases .... }

This is plain silly in the common case where w2 is dead.

Even so, I can't see a good way to implement this idea.  I tried
not doing the binder-swap if the scrutinee was already evaluated
but that failed big-time:

	data T = MkT !Int

	case v of w  { MkT x ->
	case x of x1 { I# y1 ->
	case x of x2 { I# y2 -> ...

Notice that because MkT is strict, x is marked "evaluated".  But to
eliminate the last case, we must either make sure that x (as well as
x1) has unfolding MkT y1.  THe straightforward thing to do is to do
the binder-swap.  So this whole note is a no-op.

Note [zapOccInfo]
~~~~~~~~~~~~~~~~~
If we replace the scrutinee, v, by tbe case binder, then we have to nuke
any occurrence info (eg IAmDead) in the case binder, because the
case-binder now effectively occurs whenever v does.  AND we have to do
the same for the pattern-bound variables!  Example:

	(case x of { (a,b) -> a }) (case x of { (p,q) -> q })

Here, b and p are dead.  But when we move the argment inside the first
case RHS, and eliminate the second case, we get

	case x of { (a,b) -> a b }

Urk! b is alive!  Reason: the scrutinee was a variable, and case elimination
happened.  

Indeed, this can happen anytime the case binder isn't dead:
	case <any> of x { (a,b) -> 
        case x of { (p,q) -> p } }
Here (a,b) both look dead, but come alive after the inner case is eliminated.
The point is that we bring into the envt a binding
	let x = (a,b) 
after the outer case, and that makes (a,b) alive.  At least we do unless
the case binder is guaranteed dead.

Note [Case of cast]
~~~~~~~~~~~~~~~~~~~
Consider 	case (v `cast` co) of x { I# ->
		... (case (v `cast` co) of {...}) ...
We'd like to eliminate the inner case.  We can get this neatly by 
arranging that inside the outer case we add the unfolding
	v |-> x `cast` (sym co)
to v.  Then we should inline v at the inner case, cancel the casts, and away we go
	
\begin{code}
simplCaseBinder :: SimplEnv -> OutExpr -> InId -> SimplM (SimplEnv, OutId)
simplCaseBinder env scrut case_bndr
  | switchIsOn (getSwitchChecker env) NoCaseOfCase
	-- See Note [no-case-of-case]
  = do	{ (env, case_bndr') <- simplBinder env case_bndr
	; return (env, case_bndr') }

simplCaseBinder env (Var v) case_bndr
-- Failed try [see Note 2 above]
--     not (isEvaldUnfolding (idUnfolding v))
  = do	{ (env, case_bndr') <- simplBinder env (zapOccInfo case_bndr)
	; return (modifyInScope env v case_bndr', case_bndr') }
	-- We could extend the substitution instead, but it would be
	-- a hack because then the substitution wouldn't be idempotent
	-- any more (v is an OutId).  And this does just as well.
	    
simplCaseBinder env (Cast (Var v) co) case_bndr		-- Note [Case of cast]
  = do	{ (env, case_bndr') <- simplBinder env (zapOccInfo case_bndr)
  	; let rhs = Cast (Var case_bndr') (mkSymCoercion co)
	; return (addBinderUnfolding env v rhs, case_bndr') }

simplCaseBinder env other_scrut case_bndr 
  = do	{ (env, case_bndr') <- simplBinder env case_bndr
	; return (env, case_bndr') }

zapOccInfo :: InId -> InId	-- See Note [zapOccInfo]
zapOccInfo b = b `setIdOccInfo` NoOccInfo
\end{code}


simplAlts does two things:

1.  Eliminate alternatives that cannot match, including the
    DEFAULT alternative.

2.  If the DEFAULT alternative can match only one possible constructor,
    then make that constructor explicit.
    e.g.
	case e of x { DEFAULT -> rhs }
     ===>
	case e of x { (a,b) -> rhs }
    where the type is a single constructor type.  This gives better code
    when rhs also scrutinises x or e.

Here "cannot match" includes knowledge from GADTs

It's a good idea do do this stuff before simplifying the alternatives, to
avoid simplifying alternatives we know can't happen, and to come up with
the list of constructors that are handled, to put into the IdInfo of the
case binder, for use when simplifying the alternatives.

Eliminating the default alternative in (1) isn't so obvious, but it can
happen:

data Colour = Red | Green | Blue

f x = case x of
	Red -> ..
	Green -> ..
	DEFAULT -> h x

h y = case y of
	Blue -> ..
	DEFAULT -> [ case y of ... ]

If we inline h into f, the default case of the inlined h can't happen.
If we don't notice this, we may end up filtering out *all* the cases
of the inner case y, which give us nowhere to go!


\begin{code}
simplAlts :: SimplEnv 
	  -> OutExpr
	  -> InId			-- Case binder
	  -> [InAlt] -> SimplCont
	  -> SimplM (OutId, [OutAlt])	-- Includes the continuation
-- Like simplExpr, this just returns the simplified alternatives;
-- it not return an environment

simplAlts env scrut case_bndr alts cont'
  = -- pprTrace "simplAlts" (ppr alts $$ ppr (seIdSubst env)) $
    do	{ let alt_env = zapFloats env
	; (alt_env, case_bndr') <- simplCaseBinder alt_env scrut case_bndr

	; default_alts <- prepareDefault alt_env case_bndr' imposs_deflt_cons cont' maybe_deflt

	; let inst_tys = tyConAppArgs (idType case_bndr')
	      trimmed_alts = filter (is_possible inst_tys) alts_wo_default
	      in_alts      = mergeAlts default_alts trimmed_alts
		-- We need the mergeAlts in case the new default_alt 
		-- has turned into a constructor alternative.

	; alts' <- mapM (simplAlt alt_env imposs_cons case_bndr' cont') in_alts
	; return (case_bndr', alts') }
  where
    (alts_wo_default, maybe_deflt) = findDefault alts
    imposs_cons = case scrut of
		    Var v -> otherCons (idUnfolding v)
		    other -> []

	-- "imposs_deflt_cons" are handled either by the context, 
	-- OR by a branch in this case expression. (Don't include DEFAULT!!)
    imposs_deflt_cons = nub (imposs_cons ++ [con | (con,_,_) <- alts_wo_default])

    is_possible :: [Type] -> CoreAlt -> Bool
    is_possible tys (con, _, _) | con `elem` imposs_cons = False
    is_possible tys (DataAlt con, _, _) = dataConCanMatch tys con
    is_possible tys alt		        = True

------------------------------------
prepareDefault :: SimplEnv
	       -> OutId		-- Case binder; need just for its type. Note that as an
				--   OutId, it has maximum information; this is important.
				--   Test simpl013 is an example
	     -> [AltCon]	-- These cons can't happen when matching the default
	     -> SimplCont
	     -> Maybe InExpr
	     -> SimplM [InAlt]	-- One branch or none; still unsimplified
				-- We use a list because it's what mergeAlts expects

prepareDefault env case_bndr' imposs_cons cont Nothing
  = return []	-- No default branch

prepareDefault env case_bndr' imposs_cons cont (Just rhs)
  | 	-- This branch handles the case where we are 
	-- scrutinisng an algebraic data type
    Just (tycon, inst_tys) <- splitTyConApp_maybe (idType case_bndr'),
    isAlgTyCon tycon,		-- It's a data type, tuple, or unboxed tuples.  
    not (isNewTyCon tycon),	-- We can have a newtype, if we are just doing an eval:
				-- 	case x of { DEFAULT -> e }
				-- and we don't want to fill in a default for them!
    Just all_cons <- tyConDataCons_maybe tycon,
    not (null all_cons),	-- This is a tricky corner case.  If the data type has no constructors,
				-- which GHC allows, then the case expression will have at most a default
				-- alternative.  We don't want to eliminate that alternative, because the
				-- invariant is that there's always one alternative.  It's more convenient
				-- to leave	
				--	case x of { DEFAULT -> e }     
				-- as it is, rather than transform it to
				--	error "case cant match"
				-- which would be quite legitmate.  But it's a really obscure corner, and
				-- not worth wasting code on.

    let imposs_data_cons = [con | DataAlt con <- imposs_cons]	-- We now know it's a data type 
	is_possible con  = not (con `elem` imposs_data_cons)
			   && dataConCanMatch inst_tys con
  = case filter is_possible all_cons of
	[]    -> return []	-- Eliminate the default alternative
				-- altogether if it can't match

	[con] -> 	-- It matches exactly one constructor, so fill it in
		 do { tick (FillInCaseDefault case_bndr')
                    ; us <- getUniquesSmpl
                    ; let (ex_tvs, co_tvs, arg_ids) =
                              dataConRepInstPat us con inst_tys
                    ; return [(DataAlt con, ex_tvs ++ co_tvs ++ arg_ids, rhs)] }

	two_or_more -> return [(DEFAULT, [], rhs)]

  | otherwise 
  = return [(DEFAULT, [], rhs)]

------------------------------------
simplAlt :: SimplEnv
	 -> [AltCon]	-- These constructors can't be present when
			-- matching this alternative
	 -> OutId	-- The case binder
	 -> SimplCont
	 -> InAlt
	 -> SimplM (OutAlt)

-- Simplify an alternative, returning the type refinement for the 
-- alternative, if the alternative does any refinement at all

simplAlt env handled_cons case_bndr' cont' (DEFAULT, bndrs, rhs)
  = ASSERT( null bndrs )
    do	{ let env' = addBinderOtherCon env case_bndr' handled_cons
		-- Record the constructors that the case-binder *can't* be.
	; rhs' <- simplExprC env' rhs cont'
	; return (DEFAULT, [], rhs') }

simplAlt env handled_cons case_bndr' cont' (LitAlt lit, bndrs, rhs)
  = ASSERT( null bndrs )
    do	{ let env' = addBinderUnfolding env case_bndr' (Lit lit)
	; rhs' <- simplExprC env' rhs cont'
	; return (LitAlt lit, [], rhs') }

simplAlt env handled_cons case_bndr' cont' (DataAlt con, vs, rhs)
  = do	{	-- Deal with the pattern-bound variables
		-- Mark the ones that are in ! positions in the data constructor
		-- as certainly-evaluated.
		-- NB: it happens that simplBinders does *not* erase the OtherCon
		--     form of unfolding, so it's ok to add this info before 
		--     doing simplBinders
	  (env, vs') <- simplBinders env (add_evals con vs)

		-- Bind the case-binder to (con args)
	; let inst_tys' = tyConAppArgs (idType case_bndr')
	      con_args  = map Type inst_tys' ++ varsToCoreExprs vs' 
	      env'      = addBinderUnfolding env case_bndr' (mkConApp con con_args)

	; rhs' <- simplExprC env' rhs cont'
	; return (DataAlt con, vs', rhs') }
  where
	-- add_evals records the evaluated-ness of the bound variables of
	-- a case pattern.  This is *important*.  Consider
	--	data T = T !Int !Int
	--
	--	case x of { T a b -> T (a+1) b }
	--
	-- We really must record that b is already evaluated so that we don't
	-- go and re-evaluate it when constructing the result.
	-- See Note [Data-con worker strictness] in MkId.lhs
    add_evals dc vs = cat_evals dc vs (dataConRepStrictness dc)

    cat_evals dc vs strs
	= go vs strs
	where
	  go [] [] = []
	  go (v:vs) strs | isTyVar v = v : go vs strs
	  go (v:vs) (str:strs)
	    | isMarkedStrict str = evald_v  : go vs strs
	    | otherwise          = zapped_v : go vs strs
	    where
	      zapped_v = zap_occ_info v
	      evald_v  = zapped_v `setIdUnfolding` evaldUnfolding
	  go _ _ = pprPanic "cat_evals" (ppr dc $$ ppr vs $$ ppr strs)

	-- If the case binder is alive, then we add the unfolding
	--	case_bndr = C vs
	-- to the envt; so vs are now very much alive
	-- Note [Aug06] I can't see why this actually matters
    zap_occ_info | isDeadBinder case_bndr' = \id -> id
		 | otherwise		   = zapOccInfo

addBinderUnfolding :: SimplEnv -> Id -> CoreExpr -> SimplEnv
addBinderUnfolding env bndr rhs
  = modifyInScope env bndr (bndr `setIdUnfolding` mkUnfolding False rhs)

addBinderOtherCon :: SimplEnv -> Id -> [AltCon] -> SimplEnv
addBinderOtherCon env bndr cons
  = modifyInScope env bndr (bndr `setIdUnfolding` mkOtherCon cons)
\end{code}


%************************************************************************
%*									*
\subsection{Known constructor}
%*									*
%************************************************************************

We are a bit careful with occurrence info.  Here's an example

	(\x* -> case x of (a*, b) -> f a) (h v, e)

where the * means "occurs once".  This effectively becomes
	case (h v, e) of (a*, b) -> f a)
and then
	let a* = h v; b = e in f a
and then
	f (h v)

All this should happen in one sweep.

\begin{code}
knownCon :: SimplEnv -> OutExpr -> AltCon -> [OutExpr]
	 -> InId -> [InAlt] -> SimplCont
	 -> SimplM (SimplEnv, OutExpr)

knownCon env scrut con args bndr alts cont
  = do	{ tick (KnownBranch bndr)
	; knownAlt env scrut args bndr (findAlt con alts) cont }

knownAlt env scrut args bndr (DEFAULT, bs, rhs) cont
  = ASSERT( null bs )
    do	{ env <- simplNonRecX env bndr scrut
		-- This might give rise to a binding with non-atomic args
		-- like x = Node (f x) (g x)
		-- but simplNonRecX will atomic-ify it
	; simplExprF env rhs cont }

knownAlt env scrut args bndr (LitAlt lit, bs, rhs) cont
  = ASSERT( null bs )
    do 	{ env <- simplNonRecX env bndr scrut
	; simplExprF env rhs cont }

knownAlt env scrut args bndr (DataAlt dc, bs, rhs) cont
  = do	{ let dead_bndr  = isDeadBinder bndr
	      n_drop_tys = tyConArity (dataConTyCon dc)
	; env <- bind_args env dead_bndr bs (drop n_drop_tys args)
	; let
		-- It's useful to bind bndr to scrut, rather than to a fresh
		-- binding 	x = Con arg1 .. argn
		-- because very often the scrut is a variable, so we avoid
		-- creating, and then subsequently eliminating, a let-binding
		-- BUT, if scrut is a not a variable, we must be careful
		-- about duplicating the arg redexes; in that case, make
		-- a new con-app from the args
		bndr_rhs  = case scrut of
				Var v -> scrut
				other -> con_app
		con_app = mkConApp dc (take n_drop_tys args ++ con_args)
		con_args = [substExpr env (varToCoreExpr b) | b <- bs]
				-- args are aready OutExprs, but bs are InIds

	; env <- simplNonRecX env bndr bndr_rhs
	; -- pprTrace "knownCon2" (ppr bs $$ ppr rhs $$ ppr (seIdSubst env)) $
	  simplExprF env rhs cont }

-- Ugh!
bind_args env dead_bndr [] _  = return env

bind_args env dead_bndr (b:bs) (Type ty : args)
  = ASSERT( isTyVar b )
    bind_args (extendTvSubst env b ty) dead_bndr bs args
    
bind_args env dead_bndr (b:bs) (arg : args)
  = ASSERT( isId b )
    do	{ let b' = if dead_bndr then b else zapOccInfo b
		-- Note that the binder might be "dead", because it doesn't occur 
		-- in the RHS; and simplNonRecX may therefore discard it via postInlineUnconditionally
		-- Nevertheless we must keep it if the case-binder is alive, because it may
		-- be used in the con_app.  See Note [zapOccInfo]
	; env <- simplNonRecX env b' arg
	; bind_args env dead_bndr bs args }

bind_args _ _ _ _ = panic "bind_args"
\end{code}


%************************************************************************
%*									*
\subsection{Duplicating continuations}
%*									*
%************************************************************************

\begin{code}
prepareCaseCont :: SimplEnv
		-> [InAlt] -> SimplCont
	        -> SimplM (SimplEnv, SimplCont,SimplCont)
			-- Return a duplicatable continuation, a non-duplicable part 
			-- plus some extra bindings (that scope over the entire
			-- continunation)

	-- No need to make it duplicatable if there's only one alternative
prepareCaseCont env [alt] cont = return (env, cont, mkBoringStop (contResultType cont))
prepareCaseCont env alts  cont = mkDupableCont env cont
\end{code}

\begin{code}
mkDupableCont :: SimplEnv -> SimplCont 
	      -> SimplM (SimplEnv, SimplCont, SimplCont)

mkDupableCont env cont
  | contIsDupable cont
  = returnSmpl (env, cont, mkBoringStop (contResultType cont))

mkDupableCont env (Stop {}) = panic "mkDupableCont" 	-- Handled by previous eqn

mkDupableCont env (CoerceIt ty cont)
  = do	{ (env, dup, nodup) <- mkDupableCont env cont
	; return (env, CoerceIt ty dup, nodup) }

mkDupableCont env cont@(StrictBind bndr _ _ se _)
  =  return (env, mkBoringStop (substTy se (idType bndr)), cont)
	-- See Note [Duplicating strict continuations]

mkDupableCont env cont@(StrictArg _ fun_ty _ _)
  =  return (env, mkBoringStop (funArgTy fun_ty), cont)
	-- See Note [Duplicating strict continuations]

mkDupableCont env (ApplyTo _ arg se cont)
  = 	-- e.g. 	[...hole...] (...arg...)
	--	==>
	--		let a = ...arg... 
	--		in [...hole...] a
    do	{ (env, dup_cont, nodup_cont) <- mkDupableCont env cont
	; arg <- simplExpr (se `setInScope` env) arg
	; (env, arg) <- makeTrivial env arg
	; let app_cont = ApplyTo OkToDup arg (zapSubstEnv env) dup_cont
	; return (env, app_cont, nodup_cont) }

mkDupableCont env cont@(Select _ case_bndr [(_,bs,rhs)] se case_cont)
--  See Note [Single-alternative case]
--  | not (exprIsDupable rhs && contIsDupable case_cont)
--  | not (isDeadBinder case_bndr)
  | all isDeadBinder bs
  = return (env, mkBoringStop scrut_ty, cont)
  where
    scrut_ty = substTy se (idType case_bndr)

mkDupableCont env (Select _ case_bndr alts se cont)
  = 	-- e.g.		(case [...hole...] of { pi -> ei })
	--	===>
	--		let ji = \xij -> ei 
	--		in case [...hole...] of { pi -> ji xij }
    do	{ tick (CaseOfCase case_bndr)
	; (env, dup_cont, nodup_cont) <- mkDupableCont env cont
		-- NB: call mkDupableCont here, *not* prepareCaseCont
		-- We must make a duplicable continuation, whereas prepareCaseCont
		-- doesn't when there is a single case branch

	; let alt_env = se `setInScope` env 
	; (alt_env, case_bndr') <- simplBinder alt_env case_bndr
	; alts' <- mapM (simplAlt alt_env [] case_bndr' dup_cont) alts
	-- Safe to say that there are no handled-cons for the DEFAULT case
		-- NB: simplBinder does not zap deadness occ-info, so
		-- a dead case_bndr' will still advertise its deadness
		-- This is really important because in
		--	case e of b { (# a,b #) -> ... }
		-- b is always dead, and indeed we are not allowed to bind b to (# a,b #),
		-- which might happen if e was an explicit unboxed pair and b wasn't marked dead.
		-- In the new alts we build, we have the new case binder, so it must retain
		-- its deadness.
	-- NB: we don't use alt_env further; it has the substEnv for
	--     the alternatives, and we don't want that

	; (env, alts') <- mkDupableAlts env case_bndr' alts'
	; return (env,	-- Note [Duplicated env]
		  Select OkToDup case_bndr' alts' (zapSubstEnv env)
			 (mkBoringStop (contResultType dup_cont)),
		  nodup_cont) }


mkDupableAlts :: SimplEnv -> OutId -> [InAlt]
	      -> SimplM (SimplEnv, [InAlt])
-- Absorbs the continuation into the new alternatives

mkDupableAlts env case_bndr' alts
  = go env alts
  where
    go env [] = return (env, [])
    go env (alt:alts)
	= do { (env, alt') <- mkDupableAlt env case_bndr' alt
     ; (env, alts') <- go env alts
	     ; return (env, alt' : alts' ) }
					
mkDupableAlt env case_bndr' (con, bndrs', rhs')
  | exprIsDupable rhs' 	-- Note [Small alternative rhs]
  = return (env, (con, bndrs', rhs'))
  | otherwise
  = do	{ let rhs_ty'     = exprType rhs'
	      used_bndrs' = filter abstract_over (case_bndr' : bndrs')
	      abstract_over bndr 
		  | isTyVar bndr = True	-- Abstract over all type variables just in case
		  | otherwise	 = not (isDeadBinder bndr)
			-- The deadness info on the new Ids is preserved by simplBinders

	; (final_bndrs', final_args) 	-- Note [Join point abstraction]
		<- if (any isId used_bndrs')
		   then return (used_bndrs', varsToCoreExprs used_bndrs')
		    else do { rw_id <- newId FSLIT("w") realWorldStatePrimTy
			    ; return ([rw_id], [Var realWorldPrimId]) }
	     
	; join_bndr <- newId FSLIT("$j") (mkPiTypes final_bndrs' rhs_ty')
		-- Note [Funky mkPiTypes]
	
	; let 	-- We make the lambdas into one-shot-lambdas.  The
		-- join point is sure to be applied at most once, and doing so
		-- prevents the body of the join point being floated out by
		-- the full laziness pass
		really_final_bndrs     = map one_shot final_bndrs'
		one_shot v | isId v    = setOneShotLambda v
			   | otherwise = v
		join_rhs  = mkLams really_final_bndrs rhs'
		join_call = mkApps (Var join_bndr) final_args

	; return (addNonRec env join_bndr join_rhs, (con, bndrs', join_call)) }
		-- See Note [Duplicated env]
\end{code}

Note [Duplicated env]
~~~~~~~~~~~~~~~~~~~~~
Some of the alternatives are simplified, but have not been turned into a join point
So they *must* have an zapped subst-env.  So we can't use completeNonRecX to
bind the join point, because it might to do PostInlineUnconditionally, and
we'd lose that when zapping the subst-env.  We could have a per-alt subst-env,
but zapping it (as we do in mkDupableCont, the Select case) is safe, and
at worst delays the join-point inlining.

Note [Small alterantive rhs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is worth checking for a small RHS because otherwise we
get extra let bindings that may cause an extra iteration of the simplifier to
inline back in place.  Quite often the rhs is just a variable or constructor.
The Ord instance of Maybe in PrelMaybe.lhs, for example, took several extra
iterations because the version with the let bindings looked big, and so wasn't
inlined, but after the join points had been inlined it looked smaller, and so
was inlined.

NB: we have to check the size of rhs', not rhs. 
Duplicating a small InAlt might invalidate occurrence information
However, if it *is* dupable, we return the *un* simplified alternative,
because otherwise we'd need to pair it up with an empty subst-env....
but we only have one env shared between all the alts.
(Remember we must zap the subst-env before re-simplifying something).
Rather than do this we simply agree to re-simplify the original (small) thing later.

Note [Funky mkPiTypes]
~~~~~~~~~~~~~~~~~~~~~~
Notice the funky mkPiTypes.  If the contructor has existentials
it's possible that the join point will be abstracted over
type varaibles as well as term variables.
 Example:  Suppose we have
	data T = forall t.  C [t]
 Then faced with
	case (case e of ...) of
	    C t xs::[t] -> rhs
 We get the join point
	let j :: forall t. [t] -> ...
	    j = /\t \xs::[t] -> rhs
	in
	case (case e of ...) of
 	    C t xs::[t] -> j t xs

Note [Join point abstaction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we try to lift a primitive-typed something out
for let-binding-purposes, we will *caseify* it (!),
with potentially-disastrous strictness results.  So
instead we turn it into a function: \v -> e
where v::State# RealWorld#.  The value passed to this function
is realworld#, which generates (almost) no code.

There's a slight infelicity here: we pass the overall 
case_bndr to all the join points if it's used in *any* RHS,
because we don't know its usage in each RHS separately

We used to say "&& isUnLiftedType rhs_ty'" here, but now
we make the join point into a function whenever used_bndrs'
is empty.  This makes the join-point more CPR friendly. 
Consider:	let j = if .. then I# 3 else I# 4
		in case .. of { A -> j; B -> j; C -> ... }

Now CPR doesn't w/w j because it's a thunk, so
that means that the enclosing function can't w/w either,
which is a lose.  Here's the example that happened in practice:
	kgmod :: Int -> Int -> Int
	kgmod x y = if x > 0 && y < 0 || x < 0 && y > 0
	            then 78
		    else 5

I have seen a case alternative like this:
	True -> \v -> ...
It's a bit silly to add the realWorld dummy arg in this case, making
	$j = \s v -> ...
	   True -> $j s
(the \v alone is enough to make CPR happy) but I think it's rare

Note [Duplicating strict continuations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Do *not* duplicate StrictBind and StritArg continuations.  We gain
nothing by propagating them into the expressions, and we do lose a
lot.  Here's an example:
	&& (case x of { T -> F; F -> T }) E
Now, && is strict so we end up simplifying the case with
an ArgOf continuation.  If we let-bind it, we get

	let $j = \v -> && v E
	in simplExpr (case x of { T -> F; F -> T })
		     (ArgOf (\r -> $j r)
And after simplifying more we get

	let $j = \v -> && v E
	in case x of { T -> $j F; F -> $j T }
Which is a Very Bad Thing

The desire not to duplicate is the entire reason that
mkDupableCont returns a pair of continuations.

The original plan had:
e.g. 	(...strict-fn...) [...hole...]
	==>
		let $j = \a -> ...strict-fn...
		in $j [...hole...]

Note [Single-alternative cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This case is just like the ArgOf case.  Here's an example:
	data T a = MkT !a
	...(MkT (abs x))...
Then we get
	case (case x of I# x' -> 
	      case x' <# 0# of
		True  -> I# (negate# x')
		False -> I# x') of y {
	  DEFAULT -> MkT y
Because the (case x) has only one alternative, we'll transform to
	case x of I# x' ->
	case (case x' <# 0# of
		True  -> I# (negate# x')
		False -> I# x') of y {
	  DEFAULT -> MkT y
But now we do *NOT* want to make a join point etc, giving 
	case x of I# x' ->
	let $j = \y -> MkT y
	in case x' <# 0# of
		True  -> $j (I# (negate# x'))
		False -> $j (I# x')
In this case the $j will inline again, but suppose there was a big
strict computation enclosing the orginal call to MkT.  Then, it won't
"see" the MkT any more, because it's big and won't get duplicated.
And, what is worse, nothing was gained by the case-of-case transform.

When should use this case of mkDupableCont?  
However, matching on *any* single-alternative case is a *disaster*;
  e.g.	case (case ....) of (a,b) -> (# a,b #)
  We must push the outer case into the inner one!
Other choices:

   * Match [(DEFAULT,_,_)], but in the common case of Int, 
     the alternative-filling-in code turned the outer case into
		case (...) of y { I# _ -> MkT y }

   * Match on single alternative plus (not (isDeadBinder case_bndr))
     Rationale: pushing the case inwards won't eliminate the construction.
     But there's a risk of
		case (...) of y { (a,b) -> let z=(a,b) in ... }
     Now y looks dead, but it'll come alive again.  Still, this
     seems like the best option at the moment.

   * Match on single alternative plus (all (isDeadBinder bndrs))
     Rationale: this is essentially  seq.

   * Match when the rhs is *not* duplicable, and hence would lead to a
     join point.  This catches the disaster-case above.  We can test
     the *un-simplified* rhs, which is fine.  It might get bigger or
     smaller after simplification; if it gets smaller, this case might
     fire next time round.  NB also that we must test contIsDupable
     case_cont *btoo, because case_cont might be big!

     HOWEVER: I found that this version doesn't work well, because
     we can get 	let x = case (...) of { small } in ...case x...
     When x is inlined into its full context, we find that it was a bad
     idea to have pushed the outer case inside the (...) case.

