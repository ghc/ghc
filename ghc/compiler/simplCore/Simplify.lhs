%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[Simplify]{The main module of the simplifier}

\begin{code}
#include "HsVersions.h"

module Simplify ( simplTopBinds, simplExpr, simplBind ) where

IMP_Ubiq(){-uitous-}
IMPORT_DELOOPER(SmplLoop)		-- paranoia checking
IMPORT_1_3(List(partition))

import BinderInfo
import CmdLineOpts	( SimplifierSwitch(..) )
import ConFold		( completePrim )
import CoreUnfold	( Unfolding, SimpleUnfolding, mkFormSummary, FormSummary(..) )
import CostCentre 	( isSccCountCostCentre, cmpCostCentre )
import CoreSyn
import CoreUtils	( coreExprType, nonErrorRHSs, maybeErrorApp,
			  unTagBinders, squashableDictishCcExpr
			)
import Id		( idType, idWantsToBeINLINEd,
			  getIdDemandInfo, addIdDemandInfo,
			  GenId{-instance NamedThing-}
			)
import IdInfo		( willBeDemanded, DemandInfo )
import Literal		( isNoRepLit )
import Maybes		( maybeToBool )
import Name		( isLocallyDefined )
import PprStyle		( PprStyle(..) )
import PprType		( GenType{-instance Outputable-} )
import Pretty		( ppAbove )
import PrimOp		( primOpOkForSpeculation, PrimOp(..) )
import SimplCase	( simplCase, bindLargeRhs )
import SimplEnv
import SimplMonad
import SimplVar		( completeVar )
import SimplUtils
import Type		( mkTyVarTy, mkTyVarTys, mkAppTy,
			  splitFunTy, getFunTy_maybe, eqTy
			)
import TysWiredIn	( realWorldStateTy )
import Util		( isSingleton, zipEqual, panic, pprPanic, assertPanic )
\end{code}

The controlling flags, and what they do
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

passes:
------
-fsimplify		= run the simplifier
-ffloat-inwards		= runs the float lets inwards pass
-ffloat			= runs the full laziness pass
			  (ToDo: rename to -ffull-laziness)
-fupdate-analysis	= runs update analyser
-fstrictness		= runs strictness analyser
-fsaturate-apps		= saturates applications (eta expansion)

options:
-------
-ffloat-past-lambda	= OK to do full laziness.
			  (ToDo: remove, as the full laziness pass is
				 useless without this flag, therefore
				 it is unnecessary. Just -ffull-laziness
				 should be kept.)

-ffloat-lets-ok		= OK to float lets out of lets if the enclosing
			  let is strict or if the floating will expose
			  a WHNF [simplifier].

-ffloat-primops-ok	= OK to float out of lets cases whose scrutinee
			  is a primop that cannot fail [simplifier].

-fcode-duplication-ok	= allows the previous option to work on cases with
			  multiple branches [simplifier].

-flet-to-case		= does let-to-case transformation [simplifier].

-fcase-of-case		= does case of case transformation [simplifier].

-fpedantic-bottoms  	= does not allow:
			     case x of y -> e  ===>  e[x/y]
			  (which may turn bottom into non-bottom)


			NOTES ON INLINING
			~~~~~~~~~~~~~~~~~

Inlining is one of the delicate aspects of the simplifier.  By
``inlining'' we mean replacing an occurrence of a variable ``x'' by
the RHS of x's definition.  Thus

	let x = e in ...x...	===>   let x = e in ...e...

We have two mechanisms for inlining:

1.  Unconditional.  The occurrence analyser has pinned an (OneOcc
FunOcc NoDupDanger NotInsideSCC n) flag on the variable, saying ``it's
certainly safe to inline this variable, and to drop its binding''.
(...Umm... if n <= 1; if n > 1, it is still safe, provided you are
happy to be duplicating code...) When it encounters such a beast, the
simplifer binds the variable to its RHS (in the id_env) and continues.
It doesn't even look at the RHS at that stage.  It also drops the
binding altogether.

2.  Conditional.  In all other situations, the simplifer simplifies
the RHS anyway, and keeps the new binding.  It also binds the new
(cloned) variable to a ``suitable'' Unfolding in the UnfoldEnv.

Here, ``suitable'' might mean NoUnfolding (if the occurrence
info is ManyOcc and the RHS is not a manifest HNF, or UnfoldAlways (if
the variable has an INLINE pragma on it).  The idea is that anything
in the UnfoldEnv is safe to use, but also has an enclosing binding if
you decide not to use it.

Head normal forms
~~~~~~~~~~~~~~~~~
We *never* put a non-HNF unfolding in the UnfoldEnv except in the
INLINE-pragma case.

At one time I thought it would be OK to put non-HNF unfoldings in for
variables which occur only once [if they got inlined at that
occurrence the RHS of the binding would become dead, so no duplication
would occur].   But consider:
@
	let x = <expensive>
	    f = \y -> ...y...y...y...
	in f x
@
Now, it seems that @x@ appears only once, but even so it is NOT safe
to put @x@ in the UnfoldEnv, because @f@ will be inlined, and will
duplicate the references to @x@.

Because of this, the "unconditional-inline" mechanism above is the
only way in which non-HNFs can get inlined.

INLINE pragmas
~~~~~~~~~~~~~~

When a variable has an INLINE pragma on it --- which includes wrappers
produced by the strictness analyser --- we treat it rather carefully.

For a start, we are careful not to substitute into its RHS, because
that might make it BIG, and the user said "inline exactly this", not
"inline whatever you get after inlining other stuff inside me".  For
example

	let f = BIG
	in {-# INLINE y #-} y = f 3
	in ...y...y...

Here we don't want to substitute BIG for the (single) occurrence of f,
because then we'd duplicate BIG when we inline'd y.  (Exception:
things in the UnfoldEnv with UnfoldAlways flags, which originated in
other INLINE pragmas.)

So, we clean out the UnfoldEnv of all SimpleUnfolding inlinings before
going into such an RHS.

What about imports?  They don't really matter much because we only
inline relatively small things via imports.

We augment the the UnfoldEnv with UnfoldAlways guidance if there's an
INLINE pragma.  We also do this for the RHSs of recursive decls,
before looking at the recursive decls. That way we achieve the effect
of inlining a wrapper in the body of its worker, in the case of a
mutually-recursive worker/wrapper split.


%************************************************************************
%*									*
\subsection[Simplify-simplExpr]{The main function: simplExpr}
%*									*
%************************************************************************

At the top level things are a little different.

  * No cloning (not allowed for exported Ids, unnecessary for the others)

  * No floating.   Case floating is obviously out.  Let floating is
	theoretically OK, but dangerous because of space leaks.
	The long-distance let-floater lifts these lets.

\begin{code}
simplTopBinds :: SimplEnv -> [InBinding] -> SmplM [OutBinding]

simplTopBinds env [] = returnSmpl []

-- Dead code is now discarded by the occurrence analyser,

simplTopBinds env (NonRec binder@(in_id,occ_info) rhs : binds)
  = 	-- No cloning necessary at top level
 	-- Process the binding
    simplRhsExpr env binder rhs 	`thenSmpl` \ rhs' ->
    completeNonRec env binder rhs'	`thenSmpl` \ (new_env, binds1') ->

	-- Process the other bindings
    simplTopBinds new_env binds	`thenSmpl` \ binds2' ->

	-- Glue together and return ...
    returnSmpl (binds1' ++ binds2')

simplTopBinds env (Rec pairs : binds)
  = simplRecursiveGroup env ids pairs 	`thenSmpl` \ (bind', new_env) ->

	-- Process the other bindings
    simplTopBinds new_env binds		`thenSmpl` \ binds' ->

	-- Glue together and return
    returnSmpl (bind' : binds')
  where
    ids = [id | (binder@(id,_), rhs) <- pairs]	-- No cloning necessary at top level
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-simplExpr]{The main function: simplExpr}
%*									*
%************************************************************************


\begin{code}
simplExpr :: SimplEnv
	  -> InExpr -> [OutArg]
	  -> SmplM OutExpr
\end{code}

The expression returned has the same meaning as the input expression
applied to the specified arguments.


Variables
~~~~~~~~~
Check if there's a macro-expansion, and if so rattle on.  Otherwise do
the more sophisticated stuff.

\begin{code}
simplExpr env (Var v) args
  = case (lookupId env v) of
      LitArg lit		-- A boring old literal
	-> ASSERT( null args )
	   returnSmpl (Lit lit)

      VarArg var 	-- More interesting!  An id!
	-> completeVar env var args
	 			-- Either Id is in the local envt, or it's a global.
				-- In either case we don't need to apply the type
				-- environment to it.
\end{code}

Literals
~~~~~~~~

\begin{code}
simplExpr env (Lit l) [] = returnSmpl (Lit l)
#ifdef DEBUG
simplExpr env (Lit l) _  = panic "simplExpr:Lit with argument"
#endif
\end{code}

Primitive applications are simple.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

NB: Prim expects an empty argument list! (Because it should be
saturated and not higher-order. ADR)

\begin{code}
simplExpr env (Prim op prim_args) args
  = ASSERT (null args)
    let
	prim_args' = [simplArg env prim_arg | prim_arg <- prim_args]
	op'	   = simpl_op op
    in
    completePrim env op' prim_args'
  where
    -- PrimOps just need any types in them renamed.

    simpl_op (CCallOp label is_asm may_gc arg_tys result_ty)
      = let
	    arg_tys'	= map (simplTy env) arg_tys
	    result_ty'	= simplTy env result_ty
	in
	CCallOp label is_asm may_gc arg_tys' result_ty'

    simpl_op other_op = other_op
\end{code}

Constructor applications
~~~~~~~~~~~~~~~~~~~~~~~~
Nothing to try here.  We only reuse constructors when they appear as the
rhs of a let binding (see completeLetBinding).

\begin{code}
simplExpr env (Con con con_args) args
  = ASSERT( null args )
    returnSmpl (Con con [simplArg env con_arg | con_arg <- con_args])
\end{code}


Applications are easy too:
~~~~~~~~~~~~~~~~~~~~~~~~~~
Just stuff 'em in the arg stack

\begin{code}
simplExpr env (App fun arg) args
  = simplExpr env fun (simplArg env arg : args)
\end{code}

Type lambdas
~~~~~~~~~~~~

We only eta-reduce a type lambda if all type arguments in the body can
be eta-reduced. This requires us to collect up all tyvar parameters so
we can pass them all to @mkTyLamTryingEta@.

\begin{code}
simplExpr env (Lam (TyBinder tyvar) body) (TyArg ty : args)
  = -- ASSERT(not (isPrimType ty))
    tick TyBetaReduction	`thenSmpl_`
    simplExpr (extendTyEnv env tyvar ty) body args

simplExpr env tylam@(Lam (TyBinder tyvar) body) []
  = do_tylambdas env [] tylam
  where
    do_tylambdas env tyvars' (Lam (TyBinder tyvar) body)
      =	  -- Clone the type variable
	cloneTyVarSmpl tyvar		`thenSmpl` \ tyvar' ->
	let
	    new_env = extendTyEnv env tyvar (mkTyVarTy tyvar')
	in
	do_tylambdas new_env (tyvar':tyvars') body

    do_tylambdas env tyvars' body
      =	simplExpr env body []		`thenSmpl` \ body' ->
	returnSmpl (
	   (if switchIsSet env SimplDoEtaReduction
	   then mkTyLamTryingEta
	   else mkTyLam) (reverse tyvars')  body'
	)

#ifdef DEBUG
simplExpr env (Lam (TyBinder _) _) (_ : _)
  = panic "simplExpr:TyLam with non-TyArg"
#endif
\end{code}


Ordinary lambdas
~~~~~~~~~~~~~~~~

There's a complication with lambdas that aren't saturated.
Suppose we have:

	(\x. \y. ...x...)

If we did nothing, x is used inside the \y, so would be marked
as dangerous to dup.  But in the common case where the abstraction
is applied to two arguments this is over-pessimistic.
So instead we don't take account of the \y when dealing with x's usage;
instead, the simplifier is careful when partially applying lambdas.

\begin{code}
simplExpr env expr@(Lam (ValBinder binder) body) orig_args
  = go 0 env expr orig_args
  where
    go n env (Lam (ValBinder binder) body) (val_arg : args)
      | isValArg val_arg		-- The lambda has an argument
      = tick BetaReduction	`thenSmpl_`
        go (n+1) (extendIdEnvWithAtom env binder val_arg) body args

    go n env expr@(Lam (ValBinder binder) body) args
      	-- The lambda is un-saturated, so we must zap the occurrence info
 	-- on the arguments we've already beta-reduced into the body of the lambda
      = ASSERT( null args )	-- Value lambda must match value argument!
        let
	    new_env = markDangerousOccs env (take n orig_args)
        in
        simplValLam new_env expr 0 {- Guaranteed applied to at least 0 args! -}

    go n env non_val_lam_expr args     	-- The lambda had enough arguments
      = simplExpr env non_val_lam_expr args
\end{code}


Let expressions
~~~~~~~~~~~~~~~

\begin{code}
simplExpr env (Let bind body) args
  = simplBind env bind (\env -> simplExpr env body args)
		       (computeResultType env body args)
\end{code}

Case expressions
~~~~~~~~~~~~~~~~

\begin{code}
simplExpr env expr@(Case scrut alts) args
  = simplCase env scrut alts (\env rhs -> simplExpr env rhs args)
			     (computeResultType env expr args)
\end{code}


Coercions
~~~~~~~~~
\begin{code}
simplExpr env (Coerce coercion ty body) args
  = simplCoerce env coercion ty body args 
\end{code}


Set-cost-centre
~~~~~~~~~~~~~~~

1) Eliminating nested sccs ...
We must be careful to maintain the scc counts ...

\begin{code}
simplExpr env (SCC cc1 (SCC cc2 expr)) args
  | not (isSccCountCostCentre cc2) && case cmpCostCentre cc1 cc2 of { EQ_ -> True; _ -> False }
    	-- eliminate inner scc if no call counts and same cc as outer
  = simplExpr env (SCC cc1 expr) args

  | not (isSccCountCostCentre cc2) && not (isSccCountCostCentre cc1)
    	-- eliminate outer scc if no call counts associated with either ccs
  = simplExpr env (SCC cc2 expr) args
\end{code}

2) Moving sccs inside lambdas ...
  
\begin{code}
simplExpr env (SCC cc (Lam binder@(ValBinder _) body)) args
  | not (isSccCountCostCentre cc)
	-- move scc inside lambda only if no call counts
  = simplExpr env (Lam binder (SCC cc body)) args

simplExpr env (SCC cc (Lam binder body)) args
	-- always ok to move scc inside type/usage lambda
  = simplExpr env (Lam binder (SCC cc body)) args
\end{code}

3) Eliminating dict sccs ...

\begin{code}
simplExpr env (SCC cc expr) args
  | squashableDictishCcExpr cc expr
    	-- eliminate dict cc if trivial dict expression
  = simplExpr env expr args
\end{code}

4) Moving arguments inside the body of an scc ...
This moves the cost of doing the application inside the scc
(which may include the cost of extracting methods etc)

\begin{code}
simplExpr env (SCC cost_centre body) args
  = let
	new_env = setEnclosingCC env cost_centre
    in
    simplExpr new_env body args		`thenSmpl` \ body' ->
    returnSmpl (SCC cost_centre body')
\end{code}

%************************************************************************
%*									*
\subsection{Simplify RHS of a Let/Letrec}
%*									*
%************************************************************************

simplRhsExpr does arity-expansion.  That is, given:

	* a right hand side /\ tyvars -> \a1 ... an -> e
	* the information (stored in BinderInfo) that the function will always
	  be applied to at least k arguments

it transforms the rhs to

	/\tyvars -> \a1 ... an b(n+1) ... bk -> (e b(n+1) ... bk)

This is a Very Good Thing!

\begin{code}
simplRhsExpr
	:: SimplEnv
	-> InBinder
	-> InExpr
	-> SmplM OutExpr

simplRhsExpr env binder@(id,occ_info) rhs
  | dont_eta_expand rhs
  = simplExpr rhs_env rhs []

  | otherwise	-- Have a go at eta expansion
  = 	-- Deal with the big lambda part
    ASSERT( null uvars )	-- For now

    mapSmpl cloneTyVarSmpl tyvars			`thenSmpl` \ tyvars' ->
    let
	lam_env  = extendTyEnvList rhs_env (zipEqual "simplRhsExpr" tyvars (mkTyVarTys tyvars'))
    in
	-- Deal with the little lambda part
	-- Note that we call simplLam even if there are no binders,
	-- in case it can do arity expansion.
    simplValLam lam_env body (getBinderInfoArity occ_info)	`thenSmpl` \ lambda' ->

	-- Put it back together
    returnSmpl (
       (if switchIsSet env SimplDoEtaReduction
       then mkTyLamTryingEta
       else mkTyLam) tyvars' lambda'
    )
  where

    rhs_env | not (switchIsSet env IgnoreINLINEPragma) &&
	      idWantsToBeINLINEd id
	    = switchOffInlining env
	    | otherwise	
            = env

	-- Switch off all inlining in the RHS of things that have an INLINE pragma.
	-- They are going to be inlined wherever they are used, and then all the
	-- inlining will take effect.  Meanwhile, there isn't
	-- much point in doing anything to the as-yet-un-INLINEd rhs.
	-- It's very important to switch off inlining!  Consider:
	--
	-- let f = \pq -> BIG
	-- in
	-- let g = \y -> f y y
	--     {-# INLINE g #-}
	-- in ...g...g...g...g...g...
	--
	-- Now, if that's the ONLY occurrence of f, it will be inlined inside g,
	-- and thence copied multiple times when g is inlined.

	-- Andy disagrees! Example:
	--	all xs = foldr (&&) True xs
	--	any p = all . map p  {-# INLINE any #-}
	--
	-- Problem: any won't get deforested, and so if it's exported and
	-- the importer doesn't use the inlining, (eg passes it as an arg)
	-- then we won't get deforestation at all.
	-- We havn't solved this problem yet!

    (uvars, tyvars, body) = collectUsageAndTyBinders rhs

	-- dont_eta_expand prevents eta expansion in silly situations.
	-- For example, consider the defn
	--	x = y
	-- It would be silly to eta expand the "y", because it would just
	-- get eta-reduced back to y.  Furthermore, if this was a top level defn,
	-- and x was exported, then the defn won't be eliminated, so this
	-- silly expand/reduce cycle will happen every time, which makes the
	-- simplifier loop!.
	-- The solution is to not even try eta expansion unless the rhs looks
	-- non-trivial.
    dont_eta_expand (Lit _)     = True
    dont_eta_expand (Var _)     = True
    dont_eta_expand (Con _ _)   = True
    dont_eta_expand (App f a)
      | notValArg    a		= dont_eta_expand f
    dont_eta_expand (Lam x b)
      | notValBinder x		= dont_eta_expand b
    dont_eta_expand _		= False
\end{code}


%************************************************************************
%*									*
\subsection{Simplify a lambda abstraction}
%*									*
%************************************************************************

Simplify (\binders -> body) trying eta expansion and reduction, given that
the abstraction will always be applied to at least min_no_of_args.

\begin{code}
simplValLam env expr min_no_of_args
  | not (switchIsSet env SimplDoLambdaEtaExpansion) ||	-- Bale out if eta expansion off
    null binders				    ||  -- or it's a thunk
    null potential_extra_binder_tys		    ||	-- or ain't a function
    no_of_extra_binders <= 0				-- or no extra binders needed
  = cloneIds env binders		`thenSmpl` \ binders' ->
    let
	new_env = extendIdEnvWithClones env binders binders'
    in
    simplExpr new_env body []		`thenSmpl` \ body' ->
    returnSmpl (
      (if switchIsSet new_env SimplDoEtaReduction
       then mkValLamTryingEta
       else mkValLam) binders' body'
    )

  | otherwise				-- Eta expansion possible
  = tick EtaExpansion			`thenSmpl_`
    cloneIds env binders	 	`thenSmpl` \ binders' ->
    let
	new_env = extendIdEnvWithClones env binders binders'
    in
    newIds extra_binder_tys				`thenSmpl` \ extra_binders' ->
    simplExpr new_env body (map VarArg extra_binders')	`thenSmpl` \ body' ->
    returnSmpl (
      (if switchIsSet new_env SimplDoEtaReduction
       then mkValLamTryingEta
       else mkValLam) (binders' ++ extra_binders') body'
    )

  where
    (binders,body) = collectValBinders expr
    (potential_extra_binder_tys, res_ty)
	= splitFunTy (simplTy env (coreExprType (unTagBinders body)))
	-- Note: it's possible that simplValLam will be applied to something
	-- with a forall type.  Eg when being applied to the rhs of
	--		let x = wurble
	-- where wurble has a forall-type, but no big lambdas at the top.
	-- We could be clever an insert new big lambdas, but we don't bother.

    extra_binder_tys = take no_of_extra_binders potential_extra_binder_tys

    no_of_extra_binders =	-- First, use the info about how many args it's
				-- always applied to in its scope
			   (min_no_of_args - length binders)

				-- Next, try seeing if there's a lambda hidden inside
				-- something cheap
			   `max`
			   etaExpandCount body

				-- Finally, see if it's a state transformer, in which
				-- case we eta-expand on principle! This can waste work,
				-- but usually doesn't
			   `max`
			   case potential_extra_binder_tys of
				[ty] | ty `eqTy` realWorldStateTy -> 1
				other				  -> 0

\end{code}



%************************************************************************
%*									*
\subsection[Simplify-coerce]{Coerce expressions}
%*									*
%************************************************************************

\begin{code}
-- (coerce (case s of p -> r)) args ==> case s of p -> (coerce r) args
simplCoerce env coercion ty expr@(Case scrut alts) args
  = simplCase env scrut alts (\env rhs -> simplCoerce env coercion ty rhs args)
			     (computeResultType env expr args)

-- (coerce (let defns in b)) args  ==> let defns' in (coerce b) args
simplCoerce env coercion ty (Let bind body) args
  = simplBind env bind (\env -> simplCoerce env coercion ty body args)
		       (computeResultType env body args)

-- Default case
simplCoerce env coercion ty expr args
  = simplExpr env expr []	`thenSmpl` \ expr' ->
    returnSmpl (mkGenApp (mkCoerce coercion (simplTy env ty) expr') args)
  where

	-- Try cancellation; we do this "on the way up" because
	-- I think that's where it'll bite best
    mkCoerce (CoerceOut con1) ty1 (Coerce (CoerceIn  con2) ty2 body) | con1 == con2 = body
    mkCoerce coercion ty  body = Coerce coercion ty body
\end{code}


%************************************************************************
%*									*
\subsection[Simplify-let]{Let-expressions}
%*									*
%************************************************************************

\begin{code}
simplBind :: SimplEnv
	  -> InBinding
	  -> (SimplEnv -> SmplM OutExpr)
	  -> OutType
	  -> SmplM OutExpr
\end{code}

When floating cases out of lets, remember this:

	let x* = case e of alts
	in <small expr>

where x* is sure to be demanded or e is a cheap operation that cannot
fail, e.g. unboxed addition.  Here we should be prepared to duplicate
<small expr>.  A good example:

	let x* = case y of
		   p1 -> build e1
		   p2 -> build e2
	in
	foldr c n x*
==>
	case y of
	  p1 -> foldr c n (build e1)
	  p2 -> foldr c n (build e2)

NEW: We use the same machinery that we use for case-of-case to
*always* do case floating from let, that is we let bind and abstract
the original let body, and let the occurrence analyser later decide
whether the new let should be inlined or not. The example above
becomes:

==>
      let join_body x' = foldr c n x'
	in case y of
	p1 -> let x* = build e1
		in join_body x*
	p2 -> let x* = build e2
		in join_body x*

note that join_body is a let-no-escape.
In this particular example join_body will later be inlined,
achieving the same effect.
ToDo: check this is OK with andy



\begin{code}
-- Dead code is now discarded by the occurrence analyser,

simplBind env (NonRec binder@(id,occ_info) rhs) body_c body_ty
  = simpl_bind env rhs
  where
    -- Try let-to-case; see notes below about let-to-case
    simpl_bind env rhs | will_be_demanded &&
		         try_let_to_case &&
		         type_ok_for_let_to_case rhs_ty &&
			 rhs_is_whnf	-- note: WHNF, but not bottom,  (comment below)
      = tick Let2Case				`thenSmpl_`
        mkIdentityAlts rhs_ty			`thenSmpl` \ id_alts ->
        simplCase env rhs id_alts (\env rhs -> simpl_bind env rhs) body_ty

    -- Try let-from-let
    simpl_bind env (Let bind rhs) | let_floating_ok
      = tick LetFloatFromLet                    `thenSmpl_`
	simplBind env (fix_up_demandedness will_be_demanded bind)
		      (\env -> simpl_bind env rhs) body_ty

    -- Try case-from-let; this deals with a strict let of error too
    simpl_bind env (Case scrut alts) | will_be_demanded || 
				       (float_primops && is_cheap_prim_app scrut)
      = tick CaseFloatFromLet				`thenSmpl_`

	-- First, bind large let-body if necessary
	if ok_to_dup || isSingleton (nonErrorRHSs alts)
	then
	    simplCase env scrut alts (\env rhs -> simpl_bind env rhs) body_ty
	else
	    bindLargeRhs env [binder] body_ty body_c	`thenSmpl` \ (extra_binding, new_body) ->
	    let
		body_c' = \env -> simplExpr env new_body []
		case_c  = \env rhs -> simplBind env (NonRec binder rhs) body_c' body_ty
	    in
	    simplCase env scrut alts case_c body_ty	`thenSmpl` \ case_expr ->
	    returnSmpl (Let extra_binding case_expr)

    -- None of the above; simplify rhs and tidy up
    simpl_bind env rhs
      = simplRhsExpr env binder rhs 	`thenSmpl` \ rhs' ->
	completeNonRec env binder rhs'	`thenSmpl` \ (new_env, binds) ->
        body_c new_env			`thenSmpl` \ body' ->
        returnSmpl (mkCoLetsAny binds body')


	-- All this stuff is computed at the start of the simpl_bind loop
    float_lets       	      = switchIsSet env SimplFloatLetsExposingWHNF
    float_primops    	      = switchIsSet env SimplOkToFloatPrimOps
    ok_to_dup	     	      = switchIsSet env SimplOkToDupCode
    always_float_let_from_let = switchIsSet env SimplAlwaysFloatLetsFromLets
    try_let_to_case           = switchIsSet env SimplLetToCase
    no_float		      = switchIsSet env SimplNoLetFromStrictLet

    will_be_demanded = willBeDemanded (getIdDemandInfo id)
    rhs_ty 	     = idType id

    rhs_is_whnf = case mkFormSummary rhs of
			VarForm -> True
			ValueForm -> True
			other -> False

    let_floating_ok  = (will_be_demanded && not no_float) ||
		       always_float_let_from_let ||
		       floatExposesHNF float_lets float_primops ok_to_dup rhs
\end{code}

Let to case
~~~~~~~~~~~
It's important to try let-to-case before floating. Consider

	let a*::Int = case v of {p1->e1; p2->e2}
	in b

(The * means that a is sure to be demanded.)
If we do case-floating first we get this:

	let k = \a* -> b
	in case v of
		p1-> let a*=e1 in k a
		p2-> let a*=e2 in k a

Now watch what happens if we do let-to-case first:

	case (case v of {p1->e1; p2->e2}) of
	  Int a# -> let a*=I# a# in b
===>
	let k = \a# -> let a*=I# a# in b
	in case v of
		p1 -> case e1 of I# a# -> k a#
		p1 -> case e1 of I# a# -> k a#

The latter is clearly better.  (Remember the reboxing let-decl for a
is likely to go away, because after all b is strict in a.)

We do not do let to case for WHNFs, e.g.

	  let x = a:b in ...
	  =/=>
	  case a:b of x in ...

as this is less efficient.  but we don't mind doing let-to-case for
"bottom", as that will allow us to remove more dead code, if anything:

	  let x = error in ...
	  ===>
	  case error  of x -> ...
	  ===>
	  error

Notice that let to case occurs only if x is used strictly in its body
(obviously).


Letrec expressions
~~~~~~~~~~~~~~~~~~

Simplify each RHS, float any let(recs) from the RHSs (if let-floating is
on and it'll expose a HNF), and bang the whole resulting mess together
into a huge letrec.

1. Any "macros" should be expanded.  The main application of this
macro-expansion is:

	letrec
		f = ....g...
		g = ....f...
	in
	....f...

Here we would like the single call to g to be inlined.

We can spot this easily, because g will be tagged as having just one
occurrence.  The "inlineUnconditionally" predicate is just what we want.

A worry: could this lead to non-termination?  For example:

	letrec
		f = ...g...
		g = ...f...
		h = ...h...
	in
	..h..

Here, f and g call each other (just once) and neither is used elsewhere.
But it's OK:

* the occurrence analyser will drop any (sub)-group that isn't used at
  all.

* If the group is used outside itself (ie in the "in" part), then there
  can't be a cyle.

** IMPORTANT: check that NewOccAnal has the property that a group of
   bindings like the above has f&g dropped.! ***


2. We'd also like to pull out any top-level let(rec)s from the
rhs of the defns:

	letrec
		f = let h = ... in \x -> ....h...f...h...
	in
	...f...
====>
	letrec
		h = ...
		f = \x -> ....h...f...h...
	in
	...f...

But floating cases is less easy?  (Don't for now; ToDo?)


3.  We'd like to arrange that the RHSs "know" about members of the
group that are bound to constructors.  For example:

    let rec
       d.Eq      = (==,/=)
       f a b c d = case d.Eq of (h,_) -> let x = (a,b); y = (c,d) in not (h x y)
       /= a b    = unpack tuple a, unpack tuple b, call f
    in d.Eq

here, by knowing about d.Eq in f's rhs, one could get rid of
the case (and break out the recursion completely).
[This occurred with more aggressive inlining threshold (4),
nofib/spectral/knights]

How to do it?
	1: we simplify constructor rhss first.
	2: we record the "known constructors" in the environment
	3: we simplify the other rhss, with the knowledge about the constructors



\begin{code}
simplBind env (Rec pairs) body_c body_ty
  =	-- Do floating, if necessary
    let
        floated_pairs | do_floating = float_pairs pairs
		      | otherwise   = pairs

	ticks	      | do_floating = length floated_pairs - length pairs
		      | otherwise   = 0

	binders       = map fst floated_pairs
    in
    tickN LetFloatFromLet ticks		`thenSmpl_` 
		-- It's important to increment the tick counts if we
		-- do any floating.  A situation where this turns out
		-- to be important is this:
		-- Float in produces:
		-- 	letrec  x = let y = Ey in Ex
		--	in B
		-- Now floating gives this:
		--	letrec x = Ex
		--	       y = Ey
		--	in B
		--- We now want to iterate once more in case Ey doesn't
		-- mention x, in which case the y binding can be pulled
		-- out as an enclosing let(rec), which in turn gives
		-- the strictness analyser more chance.

    cloneIds env binders			`thenSmpl` \ ids' ->
    let
       env_w_clones = extendIdEnvWithClones env binders ids'
    in
    simplRecursiveGroup env ids' floated_pairs	`thenSmpl` \ (binding, new_env) ->

    body_c new_env				`thenSmpl` \ body' ->

    returnSmpl (Let binding body')

  where
    ------------ Floating stuff -------------------

    float_lets		      = switchIsSet env SimplFloatLetsExposingWHNF
    always_float_let_from_let = switchIsSet env SimplAlwaysFloatLetsFromLets
    do_floating 	      = float_lets || always_float_let_from_let

    float_pairs pairs = concat (map float_pair pairs)

    float_pair (binder, rhs)
    	| always_float_let_from_let ||
	  floatExposesHNF True False False rhs
	= (binder,rhs') : pairs'

	| otherwise
	= [(binder,rhs)]
	where
	  (pairs', rhs') = do_float rhs

	-- Float just pulls out any top-level let(rec) bindings
    do_float :: InExpr -> ([(InBinder,InExpr)], InExpr)
    do_float (Let (Rec pairs) body)     = (float_pairs pairs    ++ pairs', body')
					    where
					      (pairs', body') = do_float body
    do_float (Let (NonRec id rhs) body) = (float_pair (id,rhs) ++ pairs', body')
					    where
					      (pairs', body') = do_float body
    do_float other			    = ([], other)

simplRecursiveGroup env new_ids pairs 
  = 	-- Add unfoldings to the new_ids corresponding to their RHS
    let
       occs            = [occ | ((_,occ), _) <- pairs]
       new_ids_w_pairs = zipEqual "simplRecGp" new_ids pairs
       rhs_env         = foldl extendEnvForRecBinding 
		               env new_ids_w_pairs
    in

    mapSmpl (\(binder,rhs) -> simplRhsExpr rhs_env binder rhs) pairs	`thenSmpl` \ new_rhss ->

    let
       new_pairs	   = zipEqual "simplRecGp" new_ids new_rhss
       occs_w_new_pairs = zipEqual "simplRecGp" occs new_pairs
       new_env      	   = foldl (\env (occ_info,(new_id,new_rhs)) -> 
			  	    extendEnvGivenBinding env occ_info new_id new_rhs)
			 	   env occs_w_new_pairs
    in
    returnSmpl (Rec new_pairs, new_env)
\end{code}


@completeLet@ looks at the simplified post-floating RHS of the
let-expression, and decides what to do.  There's one interesting
aspect to this, namely constructor reuse.  Consider
@
	f = \x -> case x of
		    (y:ys) -> y:ys
		    []     -> ...
@
Is it a good idea to replace the rhs @y:ys@ with @x@?  This depends a
bit on the compiler technology, but in general I believe not. For
example, here's some code from a real program:
@
const.Int.max.wrk{-s2516-} =
    \ upk.s3297#  upk.s3298# ->
	let {
	  a.s3299 :: Int
	  _N_ {-# U(P) #-}
	  a.s3299 = I#! upk.s3297#
	} in
	  case (const.Int._tagCmp.wrk{-s2513-} upk.s3297# upk.s3298#) of {
	    _LT -> I#! upk.s3298#
	    _EQ -> a.s3299
	    _GT -> a.s3299
	  }
@
The a.s3299 really isn't doing much good.  We'd be better off inlining
it.  (Actually, let-no-escapery means it isn't as bad as it looks.)

So the current strategy is to inline all known-form constructors, and
only do the reverse (turn a constructor application back into a
variable) when we find a let-expression:
@
	let x = C a1 .. an
	in
	... (let y = C a1 .. an in ...) ...
@
where it is always good to ditch the binding for y, and replace y by
x.  That's just what completeLetBinding does.


\begin{code}
	-- Sigh: rather disgusting case for coercions. We want to 
	-- ensure that all let-bound Coerces have atomic bodies, so
	-- they can freely be inlined.
completeNonRec env binder@(_,occ_info) (Coerce coercion ty rhs)
  = (case rhs of
	Var v -> returnSmpl (env, [], rhs)
	Lit l -> returnSmpl (env, [], rhs)
	other -> newId (coreExprType rhs)			`thenSmpl` \ inner_id ->
		 completeNonRec env 
			(inner_id, dangerousArgOcc) rhs		`thenSmpl` \ (env1, extra_bind) ->
		-- Dangerous occ because, like constructor args,
		-- it can be duplicated easily
		let
		atomic_rhs = case lookupId env1 inner_id of
			  	LitArg l -> Lit l
				VarArg v -> Var v
		in
		returnSmpl (env1, extra_bind, atomic_rhs)
     )				`thenSmpl` \ (env1, extra_bind, atomic_rhs) ->
	-- Tiresome to do all this, but we must treat the lit/var cases specially
	-- or we get a tick for atomic rhs when effectively it's a no-op.

     cloneId env1 binder				  `thenSmpl` \ new_id ->
     let 
	new_rhs = Coerce coercion ty atomic_rhs
	env2    = extendIdEnvWithClone env1 binder new_id
	new_env = extendEnvGivenBinding env2 occ_info new_id new_rhs
     in
     returnSmpl (new_env, extra_bind ++ [NonRec new_id new_rhs])
	
completeNonRec env binder new_rhs
  -- See if RHS is an atom, or a reusable constructor
  | maybeToBool maybe_atomic_rhs
  = let
	new_env = extendIdEnvWithAtom env binder rhs_atom
    in
    tick atom_tick_type			`thenSmpl_`
    returnSmpl (new_env, [])
  where
    maybe_atomic_rhs		    = exprToAtom env new_rhs
    Just (rhs_atom, atom_tick_type) = maybe_atomic_rhs

completeNonRec env binder@(_,occ_info) new_rhs
  = cloneId env binder			`thenSmpl` \ new_id ->
    let
	env1    = extendIdEnvWithClone env binder new_id
	new_env = extendEnvGivenBinding env1 occ_info new_id new_rhs
    in
    returnSmpl (new_env, [NonRec new_id new_rhs])
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-atoms]{Simplifying atoms}
%*									*
%************************************************************************

\begin{code}
simplArg :: SimplEnv -> InArg -> OutArg

simplArg env (LitArg lit) = LitArg lit
simplArg env (TyArg  ty)  = TyArg  (simplTy env ty)
simplArg env (VarArg id)  = lookupId env id
\end{code}


\begin{code}
exprToAtom env (Var var) 
  = Just (VarArg var, AtomicRhs)

exprToAtom env (Lit lit) 
  | not (isNoRepLit lit)
  = Just (LitArg lit, AtomicRhs)

exprToAtom env (Con con con_args)
  | switchIsSet env SimplReuseCon
  -- Look out for
  --	let v = C args
  --	in
  --- ...(let w = C same-args in ...)...
  -- Then use v instead of w.	 This may save
  -- re-constructing an existing constructor.
  = case (lookForConstructor env con con_args) of
		  Nothing  -> Nothing
		  Just var -> Just (VarArg var, ConReused)

exprToAtom env other
  = Nothing
\end{code}

%************************************************************************
%*									*
\subsection[Simplify-quickies]{Some local help functions}
%*									*
%************************************************************************


\begin{code}
-- fix_up_demandedness switches off the willBeDemanded Info field
-- for bindings floated out of a non-demanded let
fix_up_demandedness True {- Will be demanded -} bind
   = bind	-- Simple; no change to demand info needed
fix_up_demandedness False {- May not be demanded -} (NonRec binder rhs)
   = NonRec (un_demandify binder) rhs
fix_up_demandedness False {- May not be demanded -} (Rec pairs)
   = Rec [(un_demandify binder, rhs) | (binder,rhs) <- pairs]

un_demandify (id, occ_info) = (id `addIdDemandInfo` noInfo, occ_info)

is_cheap_prim_app (Prim op _) = primOpOkForSpeculation op
is_cheap_prim_app other	      = False

computeResultType :: SimplEnv -> InExpr -> [OutArg] -> OutType
computeResultType env expr args
  = go expr_ty' args
  where
    expr_ty  = coreExprType (unTagBinders expr)
    expr_ty' = simplTy env expr_ty

    go ty [] = ty
    go ty (TyArg ty_arg : args) = go (mkAppTy ty ty_arg) args
    go ty (a:args) | isValArg a = case (getFunTy_maybe ty) of
				    Just (_, res_ty) -> go res_ty args
				    Nothing	     -> panic "computeResultType"
\end{code}

