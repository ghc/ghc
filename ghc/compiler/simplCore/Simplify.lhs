%
% (c) The AQUA Project, Glasgow University, 1993-1996
%
\section[Simplify]{The main module of the simplifier}

\begin{code}
#include "HsVersions.h"

module Simplify ( simplTopBinds, simplExpr, simplBind ) where

import Ubiq{-uitous-}
import SmplLoop		-- paranoia checking

import BinderInfo
import CmdLineOpts	( SimplifierSwitch(..) )
import ConFold		( completePrim )
import CoreSyn
import CoreUtils	( coreExprType, nonErrorRHSs, maybeErrorApp,
			  unTagBinders, squashableDictishCcExpr,
			  manifestlyWHNF
			)
import Id		( idType, idWantsToBeINLINEd,
			  getIdDemandInfo, addIdDemandInfo,
			  GenId{-instance NamedThing-}
			)
import IdInfo		( willBeDemanded, DemandInfo )
import Literal		( isNoRepLit )
import Maybes		( maybeToBool )
import PprStyle		( PprStyle(..) )
import PprType		( GenType{-instance Outputable-} )
import PrelInfo		( realWorldStateTy )
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
import Util		( isSingleton, panic, pprPanic, assertPanic )
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
(cloned) variable to a ``suitable'' UnfoldingDetails in the UnfoldEnv.

Here, ``suitable'' might mean NoUnfoldingDetails (if the occurrence
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

So, we clean out the UnfoldEnv of all GenForm inlinings before
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

simplTopBinds env (NonRec binder@(in_id, occ_info) rhs : binds)
  | inlineUnconditionally ok_to_dup_code occ_info
  = let
	new_env = extendIdEnvWithInlining env env binder rhs
    in
    simplTopBinds new_env binds
  where
    ok_to_dup_code = switchIsSet env SimplOkToDupCode

simplTopBinds env (NonRec binder@(in_id,occ_info) rhs : binds)
  = 	-- No cloning necessary at top level
 	-- Process the binding
    simplRhsExpr env binder rhs		`thenSmpl` \ rhs' ->
    let
       new_env = case rhs' of
	 Var v			    -> extendIdEnvWithAtom env binder (VarArg v)
	 Lit i | not (isNoRepLit i) -> extendIdEnvWithAtom env binder (LitArg i)
	 other			    -> extendUnfoldEnvGivenRhs env binder in_id rhs'
    in
	-- Process the other bindings
    simplTopBinds new_env binds	`thenSmpl` \ binds' ->

	-- Glue together and return ...
	-- We leave it to susequent occurrence analysis to throw away
	-- an unused atom binding. This localises the decision about
	-- discarding top-level bindings.
    returnSmpl (NonRec in_id rhs' : binds')

simplTopBinds env (Rec pairs : binds)
  = simplRecursiveGroup env triples 	`thenSmpl` \ (bind', new_env) ->

	-- Process the other bindings
    simplTopBinds new_env binds		`thenSmpl` \ binds' ->

	-- Glue together and return
    returnSmpl (bind' : binds')
  where
    triples = [(id, (binder, rhs)) | (binder@(id,_), rhs) <- pairs]
		-- No cloning necessary at top level
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
      Nothing -> let
		    new_v = simplTyInId env v
		 in
		 completeVar env new_v args

      Just info ->
	case info of
    	  ItsAnAtom (LitArg lit) 	-- A boring old literal
			-- Paranoia check for args empty
	    ->	case args of
		  []    -> returnSmpl (Lit lit)
		  other -> panic "simplExpr:coVar"

	  ItsAnAtom (VarArg var) 	-- More interesting!  An id!
					-- No need to substitute the type env here,
					-- because we already have!
	    -> completeVar env var args

	  InlineIt id_env ty_env in_expr 	-- A macro-expansion
	    -> simplExpr (replaceInEnvs env (ty_env, id_env)) in_expr args
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
    let
	new_env = extendTyEnv env tyvar ty
    in
    tick TyBetaReduction	`thenSmpl_`
    simplExpr new_env body args

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

\begin{code}
simplExpr env (Lam (ValBinder binder) body) args
  | null leftover_binders
  = 	-- The lambda is saturated (or over-saturated)
    tick BetaReduction	`thenSmpl_`
    simplExpr env_for_enough_args body leftover_args

  | otherwise
  = 	-- Too few args to saturate the lambda
    ASSERT( null leftover_args )

    (if not (null args) -- ah, we must've gotten rid of some...
     then tick BetaReduction
     else returnSmpl (panic "BetaReduction")
    ) `thenSmpl_`

    simplLam env_for_too_few_args leftover_binders body
	     0 {- Guaranteed applied to at least 0 args! -}

  where
    (binder_args_pairs, leftover_binders, leftover_args) = collect_val_args binder args

    env_for_enough_args  = extendIdEnvWithAtomList env binder_args_pairs

    env_for_too_few_args = extendIdEnvWithAtomList env zapped_binder_args_pairs

	-- Since there aren't enough args the binders we are cancelling with
	-- the args supplied are, in effect, ocurring inside a lambda.
	-- So we modify their occurrence info to reflect this fact.
	-- Example:	(\ x y z -> e) p q
	--	    ==> (\z -> e[p/x, q/y])
	-- 	but we should behave as if x and y are marked "inside lambda".
	-- The occurrence analyser does not mark them so itself because then we
	-- do badly on the very common case of saturated lambdas applications:
	--		(\ x y z -> e) p q r
	--	    ==> e[p/x, q/y, r/z]
	--
    zapped_binder_args_pairs = [ ((id, markDangerousToDup occ_info), arg)
			       | ((id, occ_info), arg) <- binder_args_pairs ]

    collect_val_args :: InBinder	    	-- Binder
 		     -> [OutArg]	    	-- Arguments
		     -> ([(InBinder,OutArg)],	-- Binder,arg pairs (ToDo: a maybe?)
			 [InBinder],	    	-- Leftover binders (ToDo: a maybe)
			 [OutArg])	    	-- Leftover args

	-- collect_val_args strips off the leading ValArgs from
	-- the current arg list, returning them along with the
	-- depleted list
    collect_val_args binder []   = ([], [binder], [])
    collect_val_args binder (arg : args) | isValArg arg
	= ([(binder,arg)], [], args)

#ifdef DEBUG
    collect_val_args _ (other_val_arg : _) = panic "collect_val_args"
		-- TyArg should never meet a Lam
#endif
\end{code}


Let expressions
~~~~~~~~~~~~~~~

\begin{code}
simplExpr env (Let bind body) args
  | not (switchIsSet env SimplNoLetFromApp)		-- The common case
  = simplBind env bind (\env -> simplExpr env body args)
		       (computeResultType env body args)

  | otherwise		-- No float from application
  = simplBind env bind (\env -> simplExpr env body [])
		       (computeResultType env body [])	`thenSmpl` \ let_expr' ->
    returnSmpl (mkGenApp let_expr' args)
\end{code}

Case expressions
~~~~~~~~~~~~~~~~

\begin{code}
simplExpr env expr@(Case scrut alts) args
  = simplCase env scrut alts (\env rhs -> simplExpr env rhs args)
			     (computeResultType env expr args)
\end{code}


Set-cost-centre
~~~~~~~~~~~~~~~

A special case we do:
\begin{verbatim}
	scc "foo" (\x -> e)  ===>   \x -> scc "foo" e
\end{verbatim}
Simon thinks it's OK, at least for lexical scoping; and it makes
interfaces change less (arities).

\begin{code}
simplExpr env (SCC cc (Lam binder body)) args
  = simplExpr env (Lam binder (SCC cc body)) args
\end{code}

Some other slightly turgid SCC tidying-up cases:
\begin{code}
simplExpr env (SCC cc1 expr@(SCC _ _)) args
  = simplExpr env expr args
    -- the outer _scc_ serves no purpose

simplExpr env (SCC cc expr) args
  | squashableDictishCcExpr cc expr
  = simplExpr env expr args
    -- the DICT-ish CC is no longer serving any purpose
\end{code}

NB: for other set-cost-centre we move arguments inside the body.
ToDo: check with Patrick that this is ok.

\begin{code}
simplExpr env (SCC cost_centre body) args
  = let
	new_env = setEnclosingCC env (EnclosingCC cost_centre)
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
    mapSmpl cloneTyVarSmpl tyvars			`thenSmpl` \ tyvars' ->
    let
	lam_env  = extendTyEnvList rhs_env (tyvars `zip` (mkTyVarTys tyvars'))
    in
	-- Deal with the little lambda part
	-- Note that we call simplLam even if there are no binders, in case
	-- it can do arity expansion.
    simplLam lam_env binders body min_no_of_args	`thenSmpl` \ lambda' ->

	-- Put it back together
    returnSmpl (
       (if switchIsSet env SimplDoEtaReduction
       then mkTyLamTryingEta
       else mkTyLam) tyvars' lambda'
    )
  where
	-- Note from ANDY:
	-- If you say {-# INLINE #-} then you get what's coming to you;
	-- you are saying inline the rhs, please.
	-- we might want a {-# INLINE UNSIMPLIFIED #-} option.
    rhs_env | simplIdWantsToBeINLINEd id env = filterUnfoldEnvForInlines env
	    | otherwise			     = env

    (uvars, tyvars, binders, body) = collectBinders rhs

    min_no_of_args | not (null binders)			&& 	-- It's not a thunk
		     switchIsSet env SimplDoArityExpand	 	-- Arity expansion on
		   = getBinderInfoArity occ_info - length binders

		   | otherwise	-- Not a thunk
		   = 0		-- Play safe!

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
simplLam env binders body min_no_of_args
  | not (switchIsSet env SimplDoLambdaEtaExpansion) ||	-- Bale out if eta expansion off
    null potential_extra_binder_tys		    ||	-- or ain't a function
    no_of_extra_binders == 0				-- or no extra binders needed
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
    (potential_extra_binder_tys, res_ty)
	= splitFunTy (simplTy env (coreExprType (unTagBinders body)))
	-- Note: it's possible that simplLam will be applied to something
	-- with a forall type.  Eg when being applied to the rhs of
	--		let x = wurble
	-- where wurble has a forall-type, but no big lambdas at the top.
	-- We could be clever an insert new big lambdas, but we don't bother.

    extra_binder_tys = take no_of_extra_binders potential_extra_binder_tys

    no_of_extra_binders =	-- First, use the info about how many args it's
				-- always applied to in its scope
			   min_no_of_args

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
  |  inlineUnconditionally ok_to_dup occ_info
  = body_c (extendIdEnvWithInlining env env binder rhs)

-- Try let-to-case
-- It's important to try let-to-case before floating. Consider
--
--	let a*::Int = case v of {p1->e1; p2->e2}
--	in b
--
-- (The * means that a is sure to be demanded.)
-- If we do case-floating first we get this:
--
--	let k = \a* -> b
--	in case v of
--		p1-> let a*=e1 in k a
--		p2-> let a*=e2 in k a
--
-- Now watch what happens if we do let-to-case first:
--
--	case (case v of {p1->e1; p2->e2}) of
--	  Int a# -> let a*=I# a# in b
-- ===>
--	let k = \a# -> let a*=I# a# in b
--	in case v of
--		p1 -> case e1 of I# a# -> k a#
--		p1 -> case e1 of I# a# -> k a#
--
-- The latter is clearly better.  (Remember the reboxing let-decl
-- for a is likely to go away, because after all b is strict in a.)

  | will_be_demanded &&
    try_let_to_case &&
    type_ok_for_let_to_case rhs_ty &&
    not (manifestlyWHNF rhs)
	-- note: no "manifestlyBottom rhs" in there... (comment below)
    = tick Let2Case				`thenSmpl_`
      mkIdentityAlts rhs_ty			`thenSmpl` \ id_alts ->
      simplCase env rhs id_alts (\env rhs -> done_float env rhs body_c) body_ty
	{-
	We do not do let to case for WHNFs, e.g.

	  let x = a:b in ...
	  =/=>
	  case a:b of x in ...

	  as this is less efficient.
	  but we don't mind doing let-to-case for "bottom", as that
	  will
	  allow us to remove more dead code, if anything:
	  let x = error in ...
	  ===>
	  case error  of x -> ...
	  ===>
	  error

	  Notice that let to case occurs only if x is used strictly in
	  its body (obviously).
	-}

  | (will_be_demanded && not no_float) ||
    always_float_let_from_let ||
    floatExposesHNF float_lets float_primops ok_to_dup rhs
  = try_float env rhs body_c

  | otherwise
  = done_float env rhs body_c

  where
    will_be_demanded = willBeDemanded (getIdDemandInfo id)
    rhs_ty 	     = idType id

    float_lets       	      = switchIsSet env SimplFloatLetsExposingWHNF
    float_primops    	      = switchIsSet env SimplOkToFloatPrimOps
    ok_to_dup	     	      = switchIsSet env SimplOkToDupCode
    always_float_let_from_let = switchIsSet env SimplAlwaysFloatLetsFromLets
    try_let_to_case           = switchIsSet env SimplLetToCase
    no_float		      = switchIsSet env SimplNoLetFromStrictLet

    -------------------------------------------
    done_float env rhs body_c
	= simplRhsExpr env binder rhs 	`thenSmpl` \ rhs' ->
	  completeLet env binder rhs rhs' body_c body_ty

    ---------------------------------------
    try_float env (Let bind rhs) body_c
      = tick LetFloatFromLet                    `thenSmpl_`
	simplBind env (fix_up_demandedness will_be_demanded bind)
		      (\env -> try_float env rhs body_c) body_ty

    try_float env (Case scrut alts) body_c
      | will_be_demanded || (float_primops && is_cheap_prim_app scrut)
      = tick CaseFloatFromLet				`thenSmpl_`

	-- First, bind large let-body if necessary
	if no_need_to_bind_large_body then
	    simplCase env scrut alts (\env rhs -> try_float env rhs body_c) body_ty
	else
	    bindLargeRhs env [binder] body_ty body_c	`thenSmpl` \ (extra_binding, new_body) ->
	    let
		body_c' = \env -> simplExpr env new_body []
	    in
	    simplCase env scrut alts
		      (\env rhs -> try_float env rhs body_c')
		      body_ty				`thenSmpl` \ case_expr ->

	    returnSmpl (Let extra_binding case_expr)
      where
	no_need_to_bind_large_body
	  = ok_to_dup || isSingleton (nonErrorRHSs alts)

    try_float env other_rhs body_c = done_float env other_rhs body_c
\end{code}

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
    (if float_lets || always_float_let_from_let
     then
	mapSmpl float pairs	`thenSmpl` \ floated_pairs_s ->
	returnSmpl (concat floated_pairs_s)
     else
	returnSmpl pairs
    )					`thenSmpl` \ floated_pairs ->
    let
	binders = map fst floated_pairs
    in
    cloneIds env binders		`thenSmpl` \ ids' ->
    let
	env_w_clones = extendIdEnvWithClones env binders ids'
	triples	     = ids' `zip` floated_pairs
    in

    simplRecursiveGroup env_w_clones triples	`thenSmpl` \ (binding, new_env) ->

    body_c new_env				`thenSmpl` \ body' ->

    returnSmpl (Let binding body')

  where
    ------------ Floating stuff -------------------

    float_lets		      = switchIsSet env SimplFloatLetsExposingWHNF
    always_float_let_from_let = switchIsSet env SimplAlwaysFloatLetsFromLets

    float (binder,rhs)
      = let
	    pairs_s = float_pair (binder,rhs)
	in
	case pairs_s of
	  [_] -> returnSmpl pairs_s
	  more_than_one
	    -> tickN LetFloatFromLet (length pairs_s - 1) `thenSmpl_`
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
		returnSmpl pairs_s

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

simplRecursiveGroup env triples
  =	-- Toss out all the dead pairs?  No, there shouldn't be any!
	-- Dead code is discarded by the occurrence analyser
    let
	    -- Separate the live triples into "inline"able and
	    -- "ordinary" We're paranoid about duplication!
	(inline_triples, ordinary_triples)
	  = partition is_inline_triple triples

	is_inline_triple (_, ((_,occ_info),_))
	  = inlineUnconditionally False {-not ok_to_dup-} occ_info

	    -- Now add in the inline_pairs info (using "env_w_clones"),
	    -- so that we will save away suitably-clone-laden envs
	    -- inside the InlineIts...).

	    -- NOTE ALSO that we tie a knot here, because the
	    -- saved-away envs must also include these very inlinings
	    -- (they aren't stored anywhere else, and a late one might
	    -- be used in an early one).

	env_w_inlinings = foldl add_inline env inline_triples

	add_inline env (id', (binder,rhs))
	  = extendIdEnvWithInlining env env_w_inlinings binder rhs

	    -- Separate the remaining bindings into the ones which
	    -- need to be dealt with first (the "early" ones)
	    -- and the others (the "late" ones)
	(early_triples, late_triples)
	  = partition is_early_triple ordinary_triples

	is_early_triple (_, (_, Con _ _)) = True
	is_early_triple (i, _           ) = idWantsToBeINLINEd i
    in
	-- Process the early bindings first
    mapSmpl (do_one_binding env_w_inlinings) early_triples	`thenSmpl` \ early_triples' ->

	-- Now further extend the environment to record our knowledge
	-- about the form of the binders bound in the constructor bindings
    let
	env_w_early_info = foldr add_early_info env_w_inlinings early_triples'
	add_early_info (binder, (id', rhs')) env = extendUnfoldEnvGivenRhs env binder id' rhs'
    in
	-- Now process the non-constructor bindings
    mapSmpl (do_one_binding env_w_early_info) late_triples	`thenSmpl` \ late_triples' ->

	-- Phew! We're done
    let
	binding = Rec (map snd early_triples' ++ map snd late_triples')
    in
    returnSmpl (binding, env_w_early_info)
  where

    do_one_binding env (id', (binder,rhs))
      = simplRhsExpr env binder rhs `thenSmpl` \ rhs' ->
	returnSmpl (binder, (id', rhs'))
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
completeLet
	:: SimplEnv
	-> InBinder
	-> InExpr		-- Original RHS
	-> OutExpr		-- The simplified RHS
	-> (SimplEnv -> SmplM OutExpr)		-- Body handler
	-> OutType		-- Type of body
	-> SmplM OutExpr

completeLet env binder@(id,binder_info) old_rhs new_rhs body_c body_ty

  -- See if RHS is an atom, or a reusable constructor
  | maybeToBool maybe_atomic_rhs
  = let
	new_env = extendIdEnvWithAtom env binder rhs_atom
    in
    tick atom_tick_type			`thenSmpl_`
    body_c new_env

  -- Maybe the rhs is an application of error, and sure to be demanded
  | will_be_demanded &&
    maybeToBool maybe_error_app
  = tick CaseOfError			`thenSmpl_`
    returnSmpl retyped_error_app

  -- The general case
  | otherwise
  = cloneId env binder			`thenSmpl` \ id' ->
    let
	env1    = extendIdEnvWithClone env binder id'
	new_env = extendUnfoldEnvGivenRhs env1 binder id' new_rhs
    in
    body_c new_env			`thenSmpl` \ body' ->
    returnSmpl (Let (NonRec id' new_rhs) body')

  where
    will_be_demanded = willBeDemanded (getIdDemandInfo id)
    try_to_reuse_constr   = switchIsSet env SimplReuseCon

    Just (rhs_atom, atom_tick_type) = maybe_atomic_rhs

    maybe_atomic_rhs :: Maybe (OutArg, TickType)
	-- If the RHS is atomic, we return Just (atom, tick type)
	-- otherwise Nothing

    maybe_atomic_rhs
      = case new_rhs of
	  Var var -> Just (VarArg var, AtomicRhs)

	  Lit lit | not (isNoRepLit lit)
	    -> Just (LitArg lit, AtomicRhs)

	  Con con con_args
	    | try_to_reuse_constr
		   -- Look out for
		   --	let v = C args
		   --	in
		   --- ...(let w = C same-args in ...)...
		   -- Then use v instead of w.	 This may save
		   -- re-constructing an existing constructor.
	     -> case (lookForConstructor env con con_args) of
		  Nothing  -> Nothing
		  Just var -> Just (VarArg var, ConReused)

	  other -> Nothing

    maybe_error_app        = maybeErrorApp new_rhs (Just body_ty)
    Just retyped_error_app = maybe_error_app
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

simplArg env (VarArg id)
  | isLocallyDefined id
  = case lookupId env id of
	Just (ItsAnAtom atom) -> atom
	Just (InlineIt _ _ _) -> pprPanic "simplArg InLineIt:" (ppAbove (ppr PprDebug id) (pprSimplEnv env))
	Nothing		      -> VarArg id 	-- Must be an uncloned thing

  | otherwise
  = 	-- Not locally defined, so no change
    VarArg id
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

