%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[SimplCase]{Simplification of `case' expression}

Support code for @Simplify@.

\begin{code}
module SimplCase ( simplCase, bindLargeRhs ) where

#include "HsVersions.h"

import {-# SOURCE #-} Simplify ( simplBind, simplExpr )

import BinderInfo	-- too boring to try to select things...
import CmdLineOpts	( SimplifierSwitch(..) )
import CoreSyn
import CoreUnfold	( Unfolding(..) )
import CoreUtils	( coreAltsType, nonErrorRHSs, maybeErrorApp,
			  unTagBindersAlts, unTagBinders, coreExprType
			)
import Id		( idType, isDataCon, getIdDemandInfo, dataConArgTys,
			  DataCon, GenId{-instance Eq-},
			  Id
			)
import IdInfo		( willBeDemanded, DemandInfo )
import Literal		( isNoRepLit, Literal{-instance Eq-} )
import Maybes		( maybeToBool )
import PrelVals		( voidId )
import PrimOp		( primOpOkForSpeculation, PrimOp{-instance Eq-} )
import SimplVar		( simplBinder, simplBinders )
import SimplUtils	( newId, newIds )
import SimplEnv
import SimplMonad
import Type		( isUnpointedType, splitAlgTyConApp_maybe, splitAlgTyConApp, mkFunTy, mkFunTys )
import TyCon		( isDataTyCon )
import TysPrim		( voidTy )
import Util		( Eager, runEager, appEager,
			  isIn, isSingleton, zipEqual, panic, assertPanic )
import Outputable
\end{code}

Float let out of case.

\begin{code}
simplCase :: SimplEnv
	  -> InExpr					-- Scrutinee
	  -> (SubstEnvs, InAlts)			-- Alternatives, and their static environment
	  -> (SimplEnv -> InExpr -> SmplM OutExpr)	-- Rhs handler
	  -> OutType					-- Type of result expression
	  -> SmplM OutExpr

simplCase env (Let bind body) alts rhs_c result_ty
  | not (switchIsSet env SimplNoLetFromCase)
  = 	-- Float the let outside the case scrutinee (if not disabled by flag)
    tick LetFloatFromCase		`thenSmpl_`
    simplBind env bind (\env -> simplCase env body alts rhs_c result_ty) result_ty
\end{code}

OK to do case-of-case if

* we allow arbitrary code duplication

OR

* the inner case has one alternative
	case (case e of (a,b) -> rhs) of
	 ...
	 pi -> rhsi
	 ...
  ===>
	case e of
	  (a,b) -> case rhs of
			...
			pi -> rhsi
			...

IF neither of these two things are the case, we avoid code-duplication
by abstracting the outer rhss wrt the pattern variables.  For example

	case (case e of { p1->rhs1; ...; pn -> rhsn }) of
	  (x,y) -> body
===>
	let b = \ x y -> body
	in
	case e of
	  p1 -> case rhs1 of (x,y) -> b x y
	  ...
	  pn -> case rhsn of (x,y) -> b x y


OK, so outer case expression gets duplicated, but that's all.  Furthermore,
  (a) the binding for "b" will be let-no-escaped, so no heap allocation
	will take place; the "call" to b will simply be a stack adjustment
	and a jump
  (b) very commonly, at least some of the rhsi's will be constructors, which
	makes life even simpler.

All of this works equally well if the outer case has multiple rhss.


\begin{code}
simplCase env (Case inner_scrut inner_alts) (subst_envs, outer_alts) rhs_c result_ty
  | switchIsSet env SimplCaseOfCase
  = 	-- Ha!  Do case-of-case
    tick CaseOfCase	`thenSmpl_`

    if no_need_to_bind_large_alts
    then
	simplCase env inner_scrut (getSubstEnvs env, inner_alts)
		  (\env' rhs -> simplCase env' rhs (subst_envs, outer_alts) rhs_c result_ty)
		  result_ty
    else
	bindLargeAlts env_alts outer_alts rhs_c result_ty	`thenSmpl` \ (extra_bindings, outer_alts') ->
	let
	   rhs_c' = \env rhs -> simplExpr env rhs [] result_ty
	in
	simplCase env inner_scrut (getSubstEnvs env, inner_alts)
		  (\env rhs -> simplCase env rhs (subst_envs, outer_alts') rhs_c' result_ty)
			-- We used to have "emptySubstEnvs" instead of subst_envs here,
			-- but that is *wrong*.  The outer_alts' still have the old
			-- binders from outer_alts, with un-substituted types,
			-- so we must keep their subst_envs with them.  It does
			-- no harm to the freshly-manufactured part of outer_alts',
			-- because it'll have nothing in the domain of subst_envs anyway
		  result_ty
						`thenSmpl` \ case_expr ->
	returnSmpl (mkCoLetsNoUnboxed extra_bindings case_expr)

  where
    env_alts = setSubstEnvs env subst_envs

    no_need_to_bind_large_alts = switchIsSet env SimplOkToDupCode ||
   			         isSingleton (nonErrorRHSs inner_alts)
\end{code}

Case of an application of error.

\begin{code}
simplCase env scrut alts rhs_c result_ty
  | maybeToBool maybe_error_app
  = 	-- Look for an application of an error id
    tick CaseOfError 	`thenSmpl_`
    simplExpr env retyped_error_app [] result_ty
		-- Ignore rhs_c!
		-- We must apply simplExpr because "rhs" isn't yet simplified.
		-- The ice is a little thin because body_ty is an OutType; but it's ok really
  where
    maybe_error_app 	   = maybeErrorApp scrut (Just result_ty)
    Just retyped_error_app = maybe_error_app
\end{code}

Finally the default case

\begin{code}
simplCase env other_scrut (subst_envs, alts) rhs_c result_ty
  = simplTy env scrut_ty				`appEager` \ scrut_ty' ->
    simplExpr env_scrut other_scrut [] scrut_ty'	`thenSmpl` \ scrut' ->
    completeCase env_alts scrut' alts rhs_c
  where
	-- When simplifying the scrutinee of a complete case that
	-- has no default alternative
    env_scrut = case alts of
		AlgAlts _ NoDefault  -> setCaseScrutinee env
		PrimAlts _ NoDefault -> setCaseScrutinee env
		other		     -> env

    env_alts = setSubstEnvs env subst_envs

    scrut_ty = coreExprType (unTagBinders other_scrut)
\end{code}


%************************************************************************
%*									*
\subsection[Simplify-case]{Completing case-expression simplification}
%*									*
%************************************************************************

\begin{code}
completeCase
	:: SimplEnv
	-> OutExpr					-- The already-simplified scrutinee
	-> InAlts					-- The un-simplified alternatives
	-> (SimplEnv -> InExpr -> SmplM OutExpr)	-- Rhs handler
	-> SmplM OutExpr	-- The whole case expression
\end{code}

Scrutinising a literal or constructor.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's an obvious win to do:

	case (C a b) of {...; C p q -> rhs; ...}  ===>   rhs[a/p,b/q]

and the similar thing for primitive case.  If we have

	case x of ...

and x is known to be of constructor form, then we'll already have
inlined the constructor to give (case (C a b) of ...), so we don't
need to check for the variable case separately.

Sanity check: we don't have a good
story to tell about case analysis on NoRep things.  ToDo.

\begin{code}
completeCase env (Lit lit) alts rhs_c
  | not (isNoRepLit lit)
  = 	-- Ha!  Select the appropriate alternative
    tick KnownBranch		`thenSmpl_`
    completePrimCaseWithKnownLit env lit alts rhs_c

completeCase env expr@(Con con con_args) alts rhs_c
  = 	-- Ha! Staring us in the face -- select the appropriate alternative
    tick KnownBranch		`thenSmpl_`
    completeAlgCaseWithKnownCon env con con_args alts rhs_c
\end{code}

Case elimination
~~~~~~~~~~~~~~~~
Start with a simple situation:

	case x# of	===>   e[x#/y#]
	  y# -> e

(when x#, y# are of primitive type, of course).
We can't (in general) do this for algebraic cases, because we might
turn bottom into non-bottom!

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

\begin{code}
completeCase env scrut alts rhs_c
  | switchIsSet env SimplDoCaseElim &&

    binders_unused &&

    all_rhss_same &&

    (not  (switchIsSet env SimplPedanticBottoms) ||
     scrut_is_evald ||
     scrut_is_eliminable_primitive ||
     rhs1_is_scrutinee ||
     scrut_is_var_and_single_strict_default
     )

  = tick CaseElim 	`thenSmpl_`
    rhs_c new_env rhs1
  where
	-- Find the non-excluded rhss of the case; always at least one
    (rhs1:rhss)   = possible_rhss
    all_rhss_same = all (cheap_eq rhs1) rhss

	-- Find the reduced set of possible rhss, along with an indication of
	-- whether none of their binders are used
    (binders_unused, possible_rhss, new_env)
      = case alts of
	  PrimAlts alts deflt -> (deflt_binder_unused, 	-- No binders other than deflt
				    deflt_rhs ++ rhss,
				    new_env)
	    where
	      (deflt_binder_unused, deflt_rhs, new_env) = elim_deflt_binder deflt

		-- Eliminate unused rhss if poss
	      rhss = case scrut_form of
			OtherLit not_these -> [rhs | (alt_lit,rhs) <- alts,
						     not (alt_lit `is_elem` not_these)
					      ]
			other -> [rhs | (_,rhs) <- alts]

	  AlgAlts alts deflt -> (deflt_binder_unused && all alt_binders_unused possible_alts,
				   deflt_rhs ++ [rhs | (_,_,rhs) <- possible_alts],
				   new_env)
	    where
	      (deflt_binder_unused, deflt_rhs, new_env) = elim_deflt_binder deflt

		-- Eliminate unused alts if poss
	      possible_alts = case scrut_form of
				OtherCon not_these ->
					 	-- Remove alts which can't match
					[alt | alt@(alt_con,_,_) <- alts,
					       not (alt_con `is_elem` not_these)]

				other -> alts

	      alt_binders_unused (con, args, rhs) = all is_dead args
	      is_dead (_, DeadCode) = True
	      is_dead other_arg     = False

	-- If the scrutinee is a variable, look it up to see what we know about it
    scrut_form = case scrut of
		  Var v -> lookupUnfolding env v
		  other -> NoUnfolding

	-- If the scrut is already eval'd then there's no worry about
	-- eliminating the case
    scrut_is_evald = isEvaluated scrut_form

    scrut_is_eliminable_primitive
      = case scrut of
	   Prim op _ -> primOpOkForSpeculation op
	   Var _     -> case alts of
			  PrimAlts _ _ -> True	-- Primitive, hence non-bottom
			  AlgAlts _ _  -> False	-- Not primitive
	   other     -> False

	-- case v of w -> e{strict in w}  ===>   e[v/w]
    scrut_is_var_and_single_strict_default
      = case scrut of
	  Var _ -> case alts of
			AlgAlts [] (BindDefault (v,_) _) -> willBeDemanded (getIdDemandInfo v)
			other -> False
	  other -> False

    elim_deflt_binder NoDefault 	    		 -- No Binder
	= (True, [], env)
    elim_deflt_binder (BindDefault (id, DeadCode) rhs) -- Binder unused
	= (True, [rhs], env)
    elim_deflt_binder (BindDefault used_binder rhs) 	 -- Binder used
	= case scrut of
		Var v -> 	-- Binder used, but can be eliminated in favour of scrut
			   (True, [rhs], bindIdToAtom env used_binder (VarArg v))
		non_var -> 	-- Binder used, and can't be elimd
			   (False, [rhs], env)

	-- Check whether the chosen unique rhs (ie rhs1) is the same as
	-- the scrutinee.  Remember that the rhs is as yet unsimplified.
    rhs1_is_scrutinee = case (scrut, rhs1) of
			  (Var scrut_var, Var rhs_var)
				-> case (lookupIdSubst env rhs_var) of
				    Nothing 		     -> rhs_var  == scrut_var
				    Just (SubstVar rhs_var') -> rhs_var' == scrut_var
				    other	             -> False
			  other -> False

    is_elem x ys = isIn "completeCase" x ys
\end{code}

Scrutinising anything else.  If it's a variable, it can't be bound to a
constructor or literal, because that would have been inlined

\begin{code}
completeCase env scrut alts rhs_c
  = simplAlts env scrut alts rhs_c	`thenSmpl` \ alts' ->
    mkCoCase env scrut alts'
\end{code}




\begin{code}
bindLargeAlts :: SimplEnv
	      -> InAlts
	      -> (SimplEnv -> InExpr -> SmplM OutExpr)		-- Old rhs handler
	      -> OutType					-- Result type
	      -> SmplM ([OutBinding],	-- Extra bindings
			InAlts)		-- Modified alts

bindLargeAlts env the_lot@(AlgAlts alts deflt) rhs_c rhs_ty
  = mapAndUnzipSmpl do_alt alts			`thenSmpl` \ (alt_bindings, alts') ->
    bindLargeDefault env deflt rhs_ty rhs_c	`thenSmpl` \ (deflt_bindings, deflt') ->
    returnSmpl (deflt_bindings ++ alt_bindings, AlgAlts alts' deflt')
  where
    do_alt (con,args,rhs) = bindLargeRhs env args rhs_ty
				(\env -> rhs_c env rhs) `thenSmpl` \ (bind,rhs') ->
			    returnSmpl (bind, (con,args,rhs'))

bindLargeAlts env the_lot@(PrimAlts alts deflt) rhs_c rhs_ty
  = mapAndUnzipSmpl do_alt alts			`thenSmpl` \ (alt_bindings, alts') ->
    bindLargeDefault env deflt rhs_ty rhs_c	`thenSmpl` \ (deflt_bindings, deflt') ->
    returnSmpl (deflt_bindings ++ alt_bindings, PrimAlts alts' deflt')
  where
    do_alt (lit,rhs) = bindLargeRhs env [] rhs_ty
				(\env -> rhs_c env rhs) `thenSmpl` \ (bind,rhs') ->
		       returnSmpl (bind, (lit,rhs'))

bindLargeDefault env NoDefault rhs_ty rhs_c
  = returnSmpl ([], NoDefault)
bindLargeDefault env (BindDefault binder rhs) rhs_ty rhs_c
  = bindLargeRhs env [binder] rhs_ty
		 (\env -> rhs_c env rhs) `thenSmpl` \ (bind,rhs') ->
    returnSmpl ([bind], BindDefault binder rhs')
\end{code}

	bindLargeRhs env [x1,..,xn] rhs rhs_ty rhs_c
	 | otherwise        = (rhs_id = \x1..xn -> rhs_c rhs,
			       rhs_id x1 .. xn)

\begin{code}
bindLargeRhs :: SimplEnv
	     -> [InBinder]	-- The args wrt which the rhs should be abstracted
	     -> OutType
	     -> (SimplEnv -> SmplM OutExpr)		-- Rhs handler
	     -> SmplM (OutBinding,	-- New bindings (singleton or empty)
		       InExpr)		-- Modified rhs

bindLargeRhs env args rhs_ty rhs_c
  | null used_args && isUnpointedType rhs_ty
	-- If we try to lift a primitive-typed something out
	-- for let-binding-purposes, we will *caseify* it (!),
	-- with potentially-disastrous strictness results.  So
	-- instead we turn it into a function: \v -> e
	-- where v::Void.  Since arguments of type
	-- VoidPrim don't generate any code, this gives the
	-- desired effect.
	--
	-- The general structure is just the same as for the common "otherwise~ case
  = newId prim_rhs_fun_ty	`thenSmpl` \ prim_rhs_fun_id ->
    newId voidTy		`thenSmpl` \ void_arg_id ->
    rhs_c env 			`thenSmpl` \ prim_new_body ->

    returnSmpl (NonRec prim_rhs_fun_id (mkValLam [void_arg_id] prim_new_body),
		App (Var prim_rhs_fun_id) (VarArg voidId))

  | otherwise
  =	-- Generate the rhs
    simplBinders env used_args	`thenSmpl` \ (new_env, used_args') ->
    let
	rhs_fun_ty :: OutType
	rhs_fun_ty = mkFunTys (map idType used_args') rhs_ty
    in

	-- Make the new binding Id.  NB: it's an OutId
    newId rhs_fun_ty 		`thenSmpl` \ rhs_fun_id ->
    rhs_c new_env		`thenSmpl` \ rhs' ->
    let
	final_rhs = mkValLam used_args' rhs'
    in
    returnSmpl (NonRec rhs_fun_id final_rhs,
		foldl App (Var rhs_fun_id) used_arg_atoms)
	-- This is slightly wierd. We're retuning an OutId as part of the
	-- modified rhs, which is meant to be an InExpr. However, that's ok, because when
	-- it's processed the OutId won't be found in the environment, so it
	-- will be left unmodified.
  where

    used_args      = [arg | arg@(_,usage) <- args, not (dead usage)]
    used_arg_atoms = [VarArg arg_id | (arg_id,_) <- used_args]
    dead DeadCode  = True
    dead other     = False

    prim_rhs_fun_ty = mkFunTy voidTy rhs_ty
\end{code}

Case alternatives when we don't know the scrutinee
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A special case for case default.  If we have
\begin{verbatim}
case x of
  p1 -> e1
  y  -> default_e
\end{verbatim}
it is best to make sure that \tr{default_e} mentions \tr{x} in
preference to \tr{y}.  The code generator can do a cheaper job if it
doesn't have to come up with a binding for \tr{y}.

\begin{code}
simplAlts :: SimplEnv
	  -> OutExpr			-- Simplified scrutinee;
					-- only of interest if its a var,
					-- in which case we record its form
	  -> InAlts
	  -> (SimplEnv -> InExpr -> SmplM OutExpr)	-- Rhs handler
	  -> SmplM OutAlts
-- For single-constructor types
--	case e of y -> b    ===>   case e of (a,b) -> let y = (a,b) in b

simplAlts env scrut (AlgAlts [] (BindDefault bndr@(id,occ_info) rhs)) rhs_c
  | maybeToBool maybe_data_ty && 
    not (null cons)           && -- Not an abstract type (can arise if we're pruning tydecl imports)
    null other_cons           &&
    isDataTyCon tycon  -- doesn't apply to (constructor-less) newtypes
  = newIds inst_con_arg_tys	`thenSmpl` \ new_bindees ->
    let
	new_args = [ (b, bad_occ_info) | b <- new_bindees ]
	con_app  = mkCon con ty_args (map VarArg new_bindees)
	new_rhs  = Let (NonRec bndr con_app) rhs
    in
    simplAlts env scrut (AlgAlts [(con,new_args,new_rhs)] NoDefault) rhs_c
  where
    maybe_data_ty		= splitAlgTyConApp_maybe (idType id)
    Just (tycon, ty_args, cons)	= maybe_data_ty
    (con:other_cons)		= cons
    inst_con_arg_tys 		= dataConArgTys con ty_args
    bad_occ_info     		= ManyOcc 0	-- Non-committal!

simplAlts env scrut (AlgAlts alts deflt) rhs_c
  = mapSmpl do_alt alts					`thenSmpl` \ alts' ->
    simplDefault env scrut deflt deflt_form rhs_c	`thenSmpl` \ deflt' ->
    returnSmpl (AlgAlts alts' deflt')
  where
    deflt_form = OtherCon [con | (con,_,_) <- alts]
    do_alt (con, con_args, rhs)
      = simplBinders env con_args				`thenSmpl` \ (env1, con_args') ->
	let
	    new_env = case scrut of
		       Var v -> extendEnvGivenNewRhs env1 v (Con con args)
			     where
				(_, ty_args, _) = splitAlgTyConApp (idType v)
				args = map TyArg ty_args ++ map VarArg con_args'

		       other -> env1
	in
	rhs_c new_env rhs 				`thenSmpl` \ rhs' ->
	returnSmpl (con, con_args', rhs')

simplAlts env scrut (PrimAlts alts deflt) rhs_c
  = mapSmpl do_alt alts					`thenSmpl` \ alts' ->
    simplDefault env scrut deflt deflt_form rhs_c	`thenSmpl` \ deflt' ->
    returnSmpl (PrimAlts alts' deflt')
  where
    deflt_form = OtherLit [lit | (lit,_) <- alts]
    do_alt (lit, rhs)
      = let
	    new_env = case scrut of
			Var v -> extendEnvGivenNewRhs env v (Lit lit)
			other -> env
	in
	rhs_c new_env rhs 				`thenSmpl` \ rhs' ->
	returnSmpl (lit, rhs')
\end{code}

Use default binder where possible
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There's one complication when simplifying the default clause of
a case expression.  If we see

	case x of
	  x' -> ...x...x'...

we'd like to convert it to

	case x of
	  x' -> ...x'...x'...

Reason 1: then there might be just one occurrence of x, and it can be
inlined as the case scrutinee.  So we spot this case when dealing with
the default clause, and add a binding to the environment mapping x to
x'.

Reason 2: if the body is strict in x' then we can eliminate the
case altogether. By using x' in preference to x we give the max chance
of the strictness analyser finding that the body is strict in x'.

On the other hand, if x does *not* get inlined, then we'll actually
get somewhat better code from the former expression.  So when
doing Core -> STG we convert back!

\begin{code}
simplDefault
	:: SimplEnv
	-> OutExpr			-- Simplified scrutinee
	-> InDefault 			-- Default alternative to be completed
	-> Unfolding			-- Gives form of scrutinee
	-> (SimplEnv -> InExpr -> SmplM OutExpr)		-- Old rhs handler
	-> SmplM OutDefault

simplDefault env scrut NoDefault form rhs_c
  = returnSmpl NoDefault

-- Special case for variable scrutinee; see notes above.
simplDefault env (Var scrut_var) (BindDefault binder@(_,occ_info) rhs) 
	     info_from_this_case rhs_c
  = simplBinder env binder 	`thenSmpl` \ (env1, binder') ->
    let
      env2 = extendEnvGivenNewRhs env1 scrut_var (Var binder')

	-- Add form details for the default binder
      scrut_unf = lookupUnfolding env scrut_var
      new_env   = extendEnvGivenUnfolding env2 binder' noBinderInfo scrut_unf
			-- Use noBinderInfo rather than occ_info because we've
			-- added more occurrences by binding the scrut_var to it
    in
    rhs_c new_env rhs			`thenSmpl` \ rhs' ->
    returnSmpl (BindDefault binder' rhs')

simplDefault env scrut (BindDefault binder@(_,occ_info) rhs) 
	     info_from_this_case rhs_c
  = simplBinder env binder 	`thenSmpl` \ (env1, binder') ->
    let
	new_env = extendEnvGivenUnfolding env1 binder' occ_info info_from_this_case
    in
    rhs_c new_env rhs			`thenSmpl` \ rhs' ->
    returnSmpl (BindDefault binder' rhs')
\end{code}

Case alternatives when we know what the scrutinee is
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
completePrimCaseWithKnownLit
	:: SimplEnv
	-> Literal
	-> InAlts
	-> (SimplEnv -> InExpr -> SmplM OutExpr)	-- Rhs handler
	-> SmplM OutExpr

completePrimCaseWithKnownLit env lit (PrimAlts alts deflt) rhs_c
  = search_alts alts
  where
    search_alts :: [(Literal, InExpr)] -> SmplM OutExpr

    search_alts ((alt_lit, rhs) : _)
      | alt_lit == lit
      = 	-- Matching alternative!
	rhs_c env rhs

    search_alts (_ : other_alts)
      = 	-- This alternative doesn't match; keep looking
	search_alts other_alts

    search_alts []
      = case deflt of
	  NoDefault 	 -> 	-- Blargh!
	    panic "completePrimCaseWithKnownLit: No matching alternative and no default"

	  BindDefault binder rhs ->	-- OK, there's a default case
					-- Just bind the Id to the atom and continue
	    let
		new_env = bindIdToAtom env binder (LitArg lit)
	    in
	    rhs_c new_env rhs
\end{code}

@completeAlgCaseWithKnownCon@: We know the constructor, so we can
select one case alternative (or default).  If we choose the default:
we do different things depending on whether the constructor was
staring us in the face (e.g., \tr{case (p:ps) of {y -> ...}})
[let-bind it] or we just know the \tr{y} is now the same as some other
var [substitute \tr{y} out of existence].

\begin{code}
completeAlgCaseWithKnownCon
	:: SimplEnv
	-> DataCon -> [InArg]
		-- Scrutinee is (con, type, value arguments)
	-> InAlts
	-> (SimplEnv -> InExpr -> SmplM OutExpr)	-- Rhs handler
	-> SmplM OutExpr

completeAlgCaseWithKnownCon env con con_args a@(AlgAlts alts deflt) rhs_c
  = ASSERT(isDataCon con)
    search_alts alts
  where
    search_alts :: [(Id, [InBinder], InExpr)] -> SmplM OutExpr

    search_alts ((alt_con, alt_args, rhs) : _)
      | alt_con == con
      = 	-- Matching alternative!
	let
	    val_args = filter isValArg con_args
	    new_env  = foldr bind env (zipEqual "SimplCase" alt_args val_args)
	    bind (bndr, atom) env = bindIdToAtom env bndr atom
	in
	rhs_c new_env rhs

    search_alts (_ : other_alts)
      = 	-- This alternative doesn't match; keep looking
	search_alts other_alts

    search_alts []
      = 	-- No matching alternative
	case deflt of
	  NoDefault 	 -> 	-- Blargh!
	    pprPanic "completeAlgCaseWithKnownCon: No matching alternative and no default"
	             (ppr con <+> ppr con_args $$ ppr a)

	  BindDefault binder@(_,occ_info) rhs ->	-- OK, there's a default case
			-- let-bind the binder to the constructor
		simplBinder env binder		`thenSmpl` \ (env1, id') ->
		let
		    new_env = extendEnvGivenBinding env1 occ_info id' (Con con con_args)
		in
		rhs_c new_env rhs		`thenSmpl` \ rhs' ->
		returnSmpl (Let (NonRec id' (Con con con_args)) rhs')
\end{code}

Case absorption and identity-case elimination
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
mkCoCase :: SimplEnv -> OutExpr -> OutAlts -> SmplM OutExpr
\end{code}

@mkCoCase@ tries the following transformation (if possible):

case v of                 ==>   case v of
  p1 -> rhs1	                  p1 -> rhs1
  ...	                          ...
  pm -> rhsm                      pm -> rhsm
  d  -> case v of                 pn -> rhsn[v/d]  {or (alg)  let d=v in rhsn}
						   {or (prim) case v of d -> rhsn}
	  pn -> rhsn              ...
	  ...                     po -> rhso[v/d]
	  po -> rhso              d  -> rhsd[d/d'] {or let d'=d in rhsd}
	  d' -> rhsd

which merges two cases in one case when -- the default alternative of
the outer case scrutises the same variable as the outer case This
transformation is called Case Merging.  It avoids that the same
variable is scrutinised multiple times.

There's a closely-related transformation:

case e of                 ==>   case e of
  p1 -> rhs1	                  p1 -> rhs1
  ...	                          ...
  pm -> rhsm                      pm -> rhsm
  d  -> case d of                 pn -> let d = pn in rhsn
	  pn -> rhsn              ...
	  ...                     po -> let d = po in rhso
	  po -> rhso              d  -> rhsd[d/d'] {or let d'=d in rhsd}
	  d' -> rhsd

Here, the let's are essential, because d isn't in scope any more.
Sigh.  Of course, they may be unused, in which case they'll be
eliminated on the next round.  Unfortunately, we can't figure out
whether or not they are used at this juncture.

NB: The binder in a BindDefault USED TO BE guaranteed unused if the
scrutinee is a variable, because it'll be mapped to the scrutinised
variable.  Hence the [v/d] substitions can be omitted.

ALAS, now the default binder is used by preference, so we have to
generate trivial lets to express the substitutions, which will be
eliminated on the next pass.

The following code handles *both* these transformations (one
equation for AlgAlts, one for PrimAlts):

\begin{code}
mkCoCase env scrut (AlgAlts outer_alts
			  (BindDefault deflt_var
					 (Case (Var scrut_var')
						 (AlgAlts inner_alts inner_deflt))))
  |  switchIsSet env SimplCaseMerge &&
     ((scrut_is_var && scrut_var == scrut_var')	||	-- First transformation
      deflt_var == scrut_var')				-- Second transformation
  = 	-- Aha! The default-absorption rule applies
    tick CaseMerge	`thenSmpl_`
    returnSmpl (Case scrut (AlgAlts (outer_alts ++ munged_reduced_inner_alts)
			     (munge_alg_deflt deflt_var inner_deflt)))
	-- NB: see comment in this location for the PrimAlts case
  where
	-- Check scrutinee
    scrut_is_var = case scrut of {Var v -> True; other -> False}
    scrut_var    = case scrut of Var v -> v

	--  Eliminate any inner alts which are shadowed by the outer ones
    reduced_inner_alts = [alt | alt@(con,_,_) <- inner_alts,
				not (con `is_elem` outer_cons)]
    outer_cons = [con | (con,_,_) <- outer_alts]
    is_elem = isIn "mkAlgAlts"

	-- Add the lets if necessary
    munged_reduced_inner_alts = map munge_alt reduced_inner_alts

    munge_alt (con, args, rhs) = (con, args, Let (NonRec deflt_var v) rhs)
       where
	 v | scrut_is_var = Var scrut_var
	   | otherwise    = Con con (map TyArg arg_tys ++ map VarArg args)

    arg_tys = case (splitAlgTyConApp (idType deflt_var)) of
		(_, arg_tys, _) -> arg_tys

mkCoCase env scrut (PrimAlts
		  outer_alts
		  (BindDefault deflt_var (Case
					      (Var scrut_var')
					      (PrimAlts inner_alts inner_deflt))))
  |  switchIsSet env SimplCaseMerge &&
     ((scrut_is_var && scrut_var == scrut_var') ||
      deflt_var == scrut_var')
  = 	-- Aha! The default-absorption rule applies
    tick CaseMerge	`thenSmpl_`
    returnSmpl (Case scrut (PrimAlts (outer_alts ++ munged_reduced_inner_alts)
			     (munge_prim_deflt deflt_var inner_deflt)))

	-- Nota Bene: we don't recurse to mkCoCase again, because the
	-- default will now have a binding in it that prevents
	-- mkCoCase doing anything useful.  Much worse, in this
	-- PrimAlts case the binding in the default branch is another
	-- Case, so if we recurse to mkCoCase we will get into an
	-- infinite loop.
	--
	-- ToDo: think of a better way to do this.  At the moment
	-- there is at most one case merge per round.  That's probably
	-- plenty but it seems unclean somehow.
  where
	-- Check scrutinee
    scrut_is_var = case scrut of {Var v -> True; other -> False}
    scrut_var    = case scrut of Var v -> v

	--  Eliminate any inner alts which are shadowed by the outer ones
    reduced_inner_alts = [alt | alt@(lit,_) <- inner_alts,
				not (lit `is_elem` outer_lits)]
    outer_lits = [lit | (lit,_) <- outer_alts]
    is_elem = isIn "mkPrimAlts"

	-- Add the lets (well cases actually) if necessary
	-- The munged alternative looks like
	--	lit -> case lit of d -> rhs
	-- The next pass will certainly eliminate the inner case, but
	-- it isn't easy to do so right away.
    munged_reduced_inner_alts = map munge_alt reduced_inner_alts

    munge_alt (lit, rhs)
      | scrut_is_var = (lit, Case (Var scrut_var)
				    (PrimAlts [] (BindDefault deflt_var rhs)))
      | otherwise = (lit, Case (Lit lit)
				 (PrimAlts [] (BindDefault deflt_var rhs)))
\end{code}

Now the identity-case transformation:

	case e of 		===> e
		True -> True;
		False -> False

and similar friends.

\begin{code}
mkCoCase env scrut alts
  | identity_alts alts
  = tick CaseIdentity		`thenSmpl_`
    returnSmpl scrut
  where
    identity_alts (AlgAlts alts deflt)  = all identity_alg_alt  alts && identity_deflt deflt
    identity_alts (PrimAlts alts deflt) = all identity_prim_alt alts && identity_deflt deflt

    identity_alg_alt (con, args, Con con' args')
	 = con == con'
	   && and (zipWith eq_arg args args')
	   && length args == length args'
    identity_alg_alt other
	 = False

    identity_prim_alt (lit, Lit lit') = lit == lit'
    identity_prim_alt other	       = False

	 -- For the default case we want to spot both
	 --	x -> x
	 -- and
	 --	case y of { ... ; x -> y }
	 -- as "identity" defaults
    identity_deflt NoDefault = True
    identity_deflt (BindDefault binder (Var x)) = x == binder ||
						      case scrut of
							 Var y -> y == x
							 other   -> False
    identity_deflt _ = False

    eq_arg binder (VarArg x) = binder == x
    eq_arg _      _	       = False
\end{code}

The catch-all case

\begin{code}
mkCoCase env other_scrut other_alts = returnSmpl (Case other_scrut other_alts)
\end{code}

Boring local functions used above.  They simply introduce a trivial binding
for the binder, d', in an inner default; either
	let d' = deflt_var in rhs
or
	case deflt_var of d' -> rhs
depending on whether it's an algebraic or primitive case.

\begin{code}
munge_prim_deflt _ NoDefault = NoDefault

munge_prim_deflt deflt_var (BindDefault d' rhs)
  =   BindDefault deflt_var (Case (Var deflt_var)
			 	      (PrimAlts [] (BindDefault d' rhs)))

munge_alg_deflt _ NoDefault = NoDefault

munge_alg_deflt deflt_var (BindDefault d' rhs)
  =   BindDefault deflt_var (Let (NonRec d' (Var deflt_var)) rhs)

-- This line caused a generic version of munge_deflt (ie one used for
-- both alg and prim) to space leak massively.  No idea why.
--  = BindDefault deflt_var (mkCoLetUnboxedToCase (NonRec d' (Var deflt_var)) rhs)
\end{code}

\begin{code}
cheap_eq :: InExpr -> InExpr -> Bool
	-- A cheap equality test which bales out fast!

cheap_eq (Var v1) (Var v2) = v1==v2
cheap_eq (Lit l1) (Lit l2) = l1==l2
cheap_eq (Con con1 args1) (Con con2 args2)
  = con1 == con2 && args1 `eq_args` args2

cheap_eq (Prim op1 args1) (Prim op2 args2)
  = op1 ==op2 && args1 `eq_args` args2

cheap_eq (App f1 a1) (App f2 a2)
  = f1 `cheap_eq` f2 && a1 `eq_arg` a2

cheap_eq _ _ = False

-- ToDo: make CoreArg an instance of Eq
eq_args (a1:as1) (a2:as2) = a1 `eq_arg` a2 && as1 `eq_args` as2
eq_args []	 []	  = True
eq_args _	 _        = False

eq_arg (LitArg 	 l1) (LitArg   l2) = l1	== l2
eq_arg (VarArg 	 v1) (VarArg   v2) = v1	== v2
eq_arg (TyArg  	 t1) (TyArg    t2) = t1 == t2
eq_arg _	     _		   =  False
\end{code}
