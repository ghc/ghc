%
% (c) The AQUA Project, Glasgow University, 1994-1995
%
\section[SimplCase]{Simplification of `case' expression}

Support code for @Simplify@.

\begin{code}
#include "HsVersions.h"

module SimplCase ( simplCase, bindLargeRhs ) where

IMPORT_Trace
import Pretty		-- these are for debugging only
import Outputable

import SimplMonad
import SimplEnv
import TaggedCore
import PlainCore

import AbsPrel		( getPrimOpResultInfo, PrimOpResultInfo(..), PrimOp,
			  voidPrimTy, voidPrimId, mkFunTy, primOpOkForSpeculation
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
			)
import AbsUniType	( splitType, splitTyArgs, glueTyArgs,
			  getTyConFamilySize, isPrimType,
			  getUniDataTyCon_maybe
			)
import BasicLit		( isNoRepLit, BasicLit, PrimKind )
import CmdLineOpts	( SimplifierSwitch(..) )
import Id
import IdInfo
import Maybes		( catMaybes, maybeToBool, Maybe(..) )
import Simplify
import SimplUtils
import SimplVar		( completeVar )
import Util
\end{code}





Float let out of case.

\begin{code}
simplCase :: SimplEnv
	  -> InExpr	-- Scrutinee
	  -> InAlts	-- Alternatives
	  -> (SimplEnv -> InExpr -> SmplM OutExpr)	-- Rhs handler
	  -> OutUniType				-- Type of result expression
	  -> SmplM OutExpr

simplCase env (CoLet bind body) alts rhs_c result_ty
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
simplCase env (CoCase inner_scrut inner_alts) outer_alts rhs_c result_ty
  | switchIsSet env SimplCaseOfCase
  = 	-- Ha!  Do case-of-case
    tick CaseOfCase	`thenSmpl_`

    if no_need_to_bind_large_alts
    then
	simplCase env inner_scrut inner_alts 
		  (\env rhs -> simplCase env rhs outer_alts rhs_c result_ty) result_ty
    else
	bindLargeAlts env outer_alts rhs_c result_ty	`thenSmpl` \ (extra_bindings, outer_alts') ->
	let
	   rhs_c' = \env rhs -> simplExpr env rhs []
	in
	simplCase env inner_scrut inner_alts 
		  (\env rhs -> simplCase env rhs outer_alts' rhs_c' result_ty)
		  result_ty
						`thenSmpl` \ case_expr ->
	returnSmpl (mkCoLetsNoUnboxed extra_bindings case_expr)

  where
    no_need_to_bind_large_alts = switchIsSet env SimplOkToDupCode || 
   			         isSingleton (nonErrorRHSs inner_alts)
\end{code}

Case of an application of error.

\begin{code}
simplCase env scrut alts rhs_c result_ty
  | maybeToBool maybe_error_app
  = 	-- Look for an application of an error id
    tick CaseOfError 	`thenSmpl_`
    rhs_c env retyped_error_app
  where
    alts_ty 	    	   = typeOfCoreAlts (unTagBindersAlts alts)
    maybe_error_app 	   = maybeErrorApp scrut (Just alts_ty)
    Just retyped_error_app = maybe_error_app
\end{code}

Finally the default case

\begin{code}
simplCase env other_scrut alts rhs_c result_ty
  = 	-- Float the let outside the case scrutinee
    simplExpr env other_scrut []	`thenSmpl` \ scrut' ->
    completeCase env scrut' alts rhs_c
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
completeCase env (CoLit lit) alts rhs_c
  | not (isNoRepLit lit)
  = 	-- Ha!  Select the appropriate alternative
    tick KnownBranch		`thenSmpl_`
    completePrimCaseWithKnownLit env lit alts rhs_c

completeCase env expr@(CoCon con tys con_args) alts rhs_c
  = 	-- Ha! Staring us in the face -- select the appropriate alternative
    tick KnownBranch		`thenSmpl_`
    completeAlgCaseWithKnownCon env con tys con_args alts rhs_c
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
	  CoPrimAlts alts deflt -> (deflt_binder_unused, 	-- No binders other than deflt
				    deflt_rhs ++ rhss,
				    new_env)
	    where
	      (deflt_binder_unused, deflt_rhs, new_env) = elim_deflt_binder deflt

		-- Eliminate unused rhss if poss
	      rhss = case scrut_form of
			OtherLiteralForm not_these -> [rhs | (alt_lit,rhs) <- alts, 
						       not (alt_lit `is_elem` not_these)
						      ]
			other -> [rhs | (_,rhs) <- alts]

	  CoAlgAlts alts deflt -> (deflt_binder_unused && all alt_binders_unused possible_alts,
				   deflt_rhs ++ [rhs | (_,_,rhs) <- possible_alts],
				   new_env)
	    where
	      (deflt_binder_unused, deflt_rhs, new_env) = elim_deflt_binder deflt

		-- Eliminate unused alts if poss
	      possible_alts = case scrut_form of
				OtherConstructorForm not_these ->
					 	-- Remove alts which can't match
					[alt | alt@(alt_con,_,_) <- alts,
					       not (alt_con `is_elem` not_these)]

#ifdef DEBUG
--				ConstructorForm c t v -> pprPanic "completeCase!" (ppAbove (ppCat [ppr PprDebug c, ppr PprDebug t, ppr PprDebug v]) (ppr PprDebug alts))
				  -- ConstructorForm can't happen, since we'd have
				  -- inlined it, and be in completeCaseWithKnownCon by now
#endif
				other -> alts

	      alt_binders_unused (con, args, rhs) = all is_dead args
	      is_dead (_, DeadCode) = True
	      is_dead other_arg     = False

	-- If the scrutinee is a variable, look it up to see what we know about it
    scrut_form = case scrut of
		  CoVar v -> lookupUnfolding env v
		  other   -> NoUnfoldingDetails

	-- If the scrut is already eval'd then there's no worry about 
	-- eliminating the case
    scrut_is_evald = case scrut_form of
			OtherLiteralForm _     -> True
			ConstructorForm _ _ _  -> True
			OtherConstructorForm _ -> True
			other		       -> False


    scrut_is_eliminable_primitive
      = case scrut of
	   CoPrim op _ _ -> primOpOkForSpeculation op
	   CoVar _       -> case alts of
				CoPrimAlts _ _ -> True	-- Primitive, hence non-bottom
				CoAlgAlts _ _  -> False	-- Not primitive
	   other	 -> False
    
	-- case v of w -> e{strict in w}  ===>   e[v/w]
    scrut_is_var_and_single_strict_default
      = case scrut of
	  CoVar _ -> case alts of 
			CoAlgAlts [] (CoBindDefault (v,_) _) -> willBeDemanded (getIdDemandInfo v)
		        other -> False
	  other -> False

    elim_deflt_binder CoNoDefault 	    		 -- No Binder
        = (True, [], env)
    elim_deflt_binder (CoBindDefault (id, DeadCode) rhs) -- Binder unused
	= (True, [rhs], env)
    elim_deflt_binder (CoBindDefault used_binder rhs) 	 -- Binder used
	= case scrut of
		CoVar v -> 	-- Binder used, but can be eliminated in favour of scrut
			   (True, [rhs], extendIdEnvWithAtom env used_binder (CoVarAtom v))
		non_var -> 	-- Binder used, and can't be elimd
			   (False, [rhs], env)

	-- Check whether the chosen unique rhs (ie rhs1) is the same as
	-- the scrutinee.  Remember that the rhs is as yet unsimplified.
    rhs1_is_scrutinee = case (scrut, rhs1) of
			  (CoVar scrut_var, CoVar rhs_var) 
				-> case lookupId env rhs_var of
				    Just (ItsAnAtom (CoVarAtom rhs_var')) 
					-> rhs_var' == scrut_var
				    other -> False
			  other -> False

    is_elem x ys = isIn "completeCase" x ys
\end{code}

Scrutinising anything else.  If it's a variable, it can't be bound to a
constructor or literal, because that would have been inlined

\begin{code}	
completeCase env scrut alts rhs_c
  = simplAlts env scrut alts rhs_c	`thenSmpl` \ alts' ->
    mkCoCase scrut alts'
\end{code}




\begin{code}
bindLargeAlts :: SimplEnv 
	      -> InAlts 
	      -> (SimplEnv -> InExpr -> SmplM OutExpr)		-- Old rhs handler
	      -> OutUniType					-- Result type
	      -> SmplM ([OutBinding],	-- Extra bindings
			InAlts)		-- Modified alts

bindLargeAlts env the_lot@(CoAlgAlts alts deflt) rhs_c rhs_ty
  = mapAndUnzipSmpl do_alt alts			`thenSmpl` \ (alt_bindings, alts') ->
    bindLargeDefault env deflt rhs_ty rhs_c	`thenSmpl` \ (deflt_bindings, deflt') ->
    returnSmpl (deflt_bindings ++ alt_bindings, CoAlgAlts alts' deflt')
  where
    do_alt (con,args,rhs) = bindLargeRhs env args rhs_ty 
				(\env -> rhs_c env rhs) `thenSmpl` \ (bind,rhs') ->
			    returnSmpl (bind, (con,args,rhs'))

bindLargeAlts env the_lot@(CoPrimAlts alts deflt) rhs_c rhs_ty
  = mapAndUnzipSmpl do_alt alts			`thenSmpl` \ (alt_bindings, alts') ->
    bindLargeDefault env deflt rhs_ty rhs_c	`thenSmpl` \ (deflt_bindings, deflt') ->
    returnSmpl (deflt_bindings ++ alt_bindings, CoPrimAlts alts' deflt')
  where
    do_alt (lit,rhs) = bindLargeRhs env [] rhs_ty
				(\env -> rhs_c env rhs) `thenSmpl` \ (bind,rhs') ->
		       returnSmpl (bind, (lit,rhs'))

bindLargeDefault env CoNoDefault rhs_ty rhs_c
  = returnSmpl ([], CoNoDefault)
bindLargeDefault env (CoBindDefault binder rhs) rhs_ty rhs_c
  = bindLargeRhs env [binder] rhs_ty 
		 (\env -> rhs_c env rhs) `thenSmpl` \ (bind,rhs') ->
    returnSmpl ([bind], CoBindDefault binder rhs')
\end{code}

	bindLargeRhs env [x1,..,xn] rhs rhs_ty rhs_c
	 | otherwise        = (rhs_id = \x1..xn -> rhs_c rhs, 
			       rhs_id x1 .. xn)

\begin{code}
bindLargeRhs :: SimplEnv
	     -> [InBinder]	-- The args wrt which the rhs should be abstracted
	     -> OutUniType
	     -> (SimplEnv -> SmplM OutExpr)		-- Rhs handler
	     -> SmplM (OutBinding,	-- New bindings (singleton or empty)
		       InExpr)		-- Modified rhs

bindLargeRhs env args rhs_ty rhs_c
  | null used_args && isPrimType rhs_ty
	-- If we try to lift a primitive-typed something out
	-- for let-binding-purposes, we will *caseify* it (!),
	-- with potentially-disastrous strictness results.  So
	-- instead we turn it into a function: \v -> e
	-- where v::VoidPrim.  Since arguments of type
	-- VoidPrim don't generate any code, this gives the 
	-- desired effect.
	--
	-- The general structure is just the same as for the common "otherwise~ case
  = newId prim_rhs_fun_ty	`thenSmpl` \ prim_rhs_fun_id ->
    newId voidPrimTy		`thenSmpl` \ void_arg_id ->
    rhs_c env 			`thenSmpl` \ prim_new_body ->

    returnSmpl (CoNonRec prim_rhs_fun_id (mkCoLam [void_arg_id] prim_new_body),
	        CoApp (CoVar prim_rhs_fun_id) (CoVarAtom voidPrimId))

  | otherwise
  = 	-- Make the new binding Id.  NB: it's an OutId
    newId rhs_fun_ty 		`thenSmpl` \ rhs_fun_id ->

	-- Generate its rhs
    cloneIds env used_args	`thenSmpl` \ used_args' ->
    let
	new_env = extendIdEnvWithClones env used_args used_args'
    in
    rhs_c new_env		`thenSmpl` \ rhs' ->
    let
	final_rhs
	  = (if switchIsSet new_env SimplDoEtaReduction
	     then mkCoLamTryingEta
	     else mkCoLam) used_args' rhs'
    in
    returnSmpl (CoNonRec rhs_fun_id final_rhs,
		foldl CoApp (CoVar rhs_fun_id) used_arg_atoms)
	-- This is slightly wierd. We're retuning an OutId as part of the
	-- modified rhs, which is meant to be an InExpr. However, that's ok, because when
	-- it's processed the OutId won't be found in the environment, so it
	-- will be left unmodified.
  where
    rhs_fun_ty :: OutUniType
    rhs_fun_ty = glueTyArgs [simplTy env (getIdUniType id) | (id,_) <- used_args] rhs_ty

    used_args      = [arg | arg@(_,usage) <- args, not (dead usage)]
    used_arg_atoms = [CoVarAtom arg_id | (arg_id,_) <- used_args]
    dead DeadCode  = True
    dead other     = False

    prim_rhs_fun_ty = mkFunTy voidPrimTy rhs_ty
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

simplAlts env scrut (CoAlgAlts alts deflt) rhs_c
  = mapSmpl do_alt alts					`thenSmpl` \ alts' ->
    simplDefault env scrut deflt deflt_form rhs_c	`thenSmpl` \ deflt' ->
    returnSmpl (CoAlgAlts alts' deflt')
  where
    deflt_form = OtherConstructorForm [con | (con,_,_) <- alts]
    do_alt (con, con_args, rhs)
      = cloneIds env con_args				`thenSmpl` \ con_args' ->
	let
	    env1    = extendIdEnvWithClones env con_args con_args'
	    new_env = case scrut of 
		       CoVar var -> _scc_ "euegC1" (extendUnfoldEnvGivenConstructor env1 var con con_args')
		       other     -> env1
        in
	rhs_c new_env rhs 				`thenSmpl` \ rhs' ->
	returnSmpl (con, con_args', rhs')

simplAlts env scrut (CoPrimAlts alts deflt) rhs_c
  = mapSmpl do_alt alts					`thenSmpl` \ alts' ->
    simplDefault env scrut deflt deflt_form rhs_c	`thenSmpl` \ deflt' ->
    returnSmpl (CoPrimAlts alts' deflt')
  where
    deflt_form = OtherLiteralForm [lit | (lit,_) <- alts]
    do_alt (lit, rhs)
      = let
	    new_env = case scrut of
			CoVar var -> _scc_ "euegFD1" (extendUnfoldEnvGivenFormDetails env var (LiteralForm lit))
			other	  -> env
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
	-> UnfoldingDetails		-- Gives form of scrutinee
        -> (SimplEnv -> InExpr -> SmplM OutExpr)		-- Old rhs handler
	-> SmplM OutDefault

simplDefault env scrut CoNoDefault form rhs_c
  = returnSmpl CoNoDefault

-- Special case for variable scrutinee; see notes above.
simplDefault env (CoVar scrut_var) (CoBindDefault binder rhs) form_from_this_case rhs_c
  = cloneId env binder 	`thenSmpl` \ binder' ->
    let
      env1    = extendIdEnvWithAtom env binder (CoVarAtom binder')

	-- Add form details for the default binder
      scrut_form = lookupUnfolding env scrut_var
      final_form
        = case (form_from_this_case, scrut_form) of
	    (OtherConstructorForm cs, OtherConstructorForm ds) -> OtherConstructorForm (cs++ds)
	    (OtherLiteralForm cs,     OtherLiteralForm ds)     -> OtherLiteralForm (cs++ds)
			-- ConstructorForm, LiteralForm impossible
			-- (ASSERT?  ASSERT?  Hello? WDP 95/05)
	    other 				               -> form_from_this_case

      env2 = _scc_ "euegFD2" (extendUnfoldEnvGivenFormDetails env1 binder' final_form)

	-- Change unfold details for scrut var.  We now want to unfold it
	-- to binder'
      new_scrut_var_form = GeneralForm True {- OK to dup -} WhnfForm 
				       (CoVar binder') UnfoldAlways
      new_env    = extendUnfoldEnvGivenFormDetails env2 scrut_var new_scrut_var_form
			
    in
    rhs_c new_env rhs			`thenSmpl` \ rhs' ->
    returnSmpl (CoBindDefault binder' rhs')

simplDefault env scrut (CoBindDefault binder rhs) form rhs_c
  = cloneId env binder 	`thenSmpl` \ binder' ->
    let
	env1    = extendIdEnvWithAtom env binder (CoVarAtom binder')
	new_env = _scc_ "euegFD2" (extendUnfoldEnvGivenFormDetails env1 binder' form)
    in
    rhs_c new_env rhs			`thenSmpl` \ rhs' ->
    returnSmpl (CoBindDefault binder' rhs')
\end{code}

Case alternatives when we know what the scrutinee is
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
completePrimCaseWithKnownLit
	:: SimplEnv
	-> BasicLit
	-> InAlts
        -> (SimplEnv -> InExpr -> SmplM OutExpr)	-- Rhs handler
	-> SmplM OutExpr

completePrimCaseWithKnownLit env lit (CoPrimAlts alts deflt) rhs_c
  = search_alts alts
  where
    search_alts :: [(BasicLit, InExpr)] -> SmplM OutExpr

    search_alts ((alt_lit, rhs) : _)
      | alt_lit == lit
      = 	-- Matching alternative!
	rhs_c env rhs

    search_alts (_ : other_alts)
      = 	-- This alternative doesn't match; keep looking
        search_alts other_alts

    search_alts []
      = case deflt of
	  CoNoDefault 	 -> 	-- Blargh!
	    panic "completePrimCaseWithKnownLit: No matching alternative and no default"

	  CoBindDefault binder rhs ->	-- OK, there's a default case
				        -- Just bind the Id to the atom and continue
	    let
		new_env = extendIdEnvWithAtom env binder (CoLitAtom lit)
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
	-> DataCon -> [UniType] -> [InAtom]
		-- Scrutinee is (con, type, value arguments)
	-> InAlts
        -> (SimplEnv -> InExpr -> SmplM OutExpr)	-- Rhs handler
	-> SmplM OutExpr

completeAlgCaseWithKnownCon env con tys con_args (CoAlgAlts alts deflt) rhs_c
  = ASSERT(isDataCon con)
    search_alts alts
  where
    search_alts :: [(Id, [InBinder], InExpr)] -> SmplM OutExpr

    search_alts ((alt_con, alt_args, rhs) : _)
      | alt_con == con
      = 	-- Matching alternative!
	let
	    new_env = extendIdEnvWithAtomList env (zip alt_args con_args)
	in
	rhs_c new_env rhs
	
    search_alts (_ : other_alts)
      = 	-- This alternative doesn't match; keep looking
        search_alts other_alts

    search_alts []
      = 	-- No matching alternative
	case deflt of
	  CoNoDefault 	 -> 	-- Blargh!
	    panic "completeAlgCaseWithKnownCon: No matching alternative and no default"

	  CoBindDefault binder rhs ->	-- OK, there's a default case
			-- let-bind the binder to the constructor
		cloneId env binder		`thenSmpl` \ id' ->
		let
		    env1    = extendIdEnvWithClone env binder id'
		    new_env = _scc_ "euegFD3" (extendUnfoldEnvGivenFormDetails env1 id' 
					(ConstructorForm con tys con_args))
		in
		rhs_c new_env rhs		`thenSmpl` \ rhs' ->
		returnSmpl (CoLet (CoNonRec id' (CoCon con tys con_args)) rhs')
\end{code}
							
Case absorption and identity-case elimination
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
mkCoCase :: OutExpr -> OutAlts -> SmplM OutExpr
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

NB: The binder in a CoBindDefault USED TO BE guaranteed unused if the
scrutinee is a variable, because it'll be mapped to the scrutinised
variable.  Hence the [v/d] substitions can be omitted.

ALAS, now the default binder is used by preference, so we have to
generate trivial lets to express the substitutions, which will be
eliminated on the next pass.

The following code handles *both* these transformations (one
equation for AlgAlts, one for PrimAlts):

\begin{code}
mkCoCase scrut (CoAlgAlts outer_alts 
			  (CoBindDefault deflt_var
					 (CoCase (CoVar scrut_var') 
						 (CoAlgAlts inner_alts inner_deflt))))
  |  (scrut_is_var && scrut_var == scrut_var')	-- First transformation
  || deflt_var == scrut_var'			-- Second transformation
  = 	-- Aha! The default-absorption rule applies
    tick CaseMerge	`thenSmpl_`
    returnSmpl (CoCase scrut (CoAlgAlts (outer_alts ++ munged_reduced_inner_alts)
			     (munge_alg_deflt deflt_var inner_deflt)))
	-- NB: see comment in this location for the CoPrimAlts case
  where
	-- Check scrutinee
    scrut_is_var = case scrut of {CoVar v -> True; other -> False}
    scrut_var    = case scrut of CoVar v -> v

	--  Eliminate any inner alts which are shadowed by the outer ones
    reduced_inner_alts = [alt | alt@(con,_,_) <- inner_alts,
				not (con `is_elem` outer_cons)]
    outer_cons = [con | (con,_,_) <- outer_alts]
    is_elem = isIn "mkAlgAlts"

	-- Add the lets if necessary
    munged_reduced_inner_alts = map munge_alt reduced_inner_alts

    munge_alt (con, args, rhs) = (con, args, CoLet (CoNonRec deflt_var v) rhs)
       where
	 v | scrut_is_var = CoVar scrut_var
	   | otherwise    = CoCon con arg_tys (map CoVarAtom args)

    arg_tys = case getUniDataTyCon_maybe (getIdUniType deflt_var) of
		Just (_, arg_tys, _) -> arg_tys

mkCoCase scrut (CoPrimAlts 
		  outer_alts 
		  (CoBindDefault deflt_var (CoCase 
					      (CoVar scrut_var') 
					      (CoPrimAlts inner_alts inner_deflt))))
  | (scrut_is_var && scrut_var == scrut_var') ||
    deflt_var == scrut_var'
  = 	-- Aha! The default-absorption rule applies
    tick CaseMerge	`thenSmpl_`
    returnSmpl (CoCase scrut (CoPrimAlts (outer_alts ++ munged_reduced_inner_alts)
			     (munge_prim_deflt deflt_var inner_deflt)))

	-- Nota Bene: we don't recurse to mkCoCase again, because the
	-- default will now have a binding in it that prevents
	-- mkCoCase doing anything useful.  Much worse, in this
	-- PrimAlts case the binding in the default branch is another
	-- CoCase, so if we recurse to mkCoCase we will get into an
	-- infinite loop.
	-- 
	-- ToDo: think of a better way to do this.  At the moment
	-- there is at most one case merge per round.  That's probably
	-- plenty but it seems unclean somehow.
  where
	-- Check scrutinee
    scrut_is_var = case scrut of {CoVar v -> True; other -> False}
    scrut_var    = case scrut of CoVar v -> v

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
      | scrut_is_var = (lit, CoCase (CoVar scrut_var)
				    (CoPrimAlts [] (CoBindDefault deflt_var rhs)))
      | otherwise = (lit, CoCase (CoLit lit) 
				 (CoPrimAlts [] (CoBindDefault deflt_var rhs)))
\end{code}

Now the identity-case transformation:

	case e of 		===> e
		True -> True; 
		False -> False

and similar friends.

\begin{code}
mkCoCase scrut alts
  | identity_alts alts
  = tick CaseIdentity		`thenSmpl_`
    returnSmpl scrut
  where
    identity_alts (CoAlgAlts alts deflt)  = all identity_alg_alt  alts && identity_deflt deflt
    identity_alts (CoPrimAlts alts deflt) = all identity_prim_alt alts && identity_deflt deflt

    identity_alg_alt (con, args, CoCon con' _ args') 
	 = con == con' && and (zipWith eq_arg args args')
    identity_alg_alt other
	 = False

    identity_prim_alt (lit, CoLit lit') = lit == lit'
    identity_prim_alt other	       = False

	 -- For the default case we want to spot both
	 --	x -> x
	 -- and
	 --	case y of { ... ; x -> y }
	 -- as "identity" defaults
    identity_deflt CoNoDefault = True
    identity_deflt (CoBindDefault binder (CoVar x)) = x == binder ||
						      case scrut of 
							 CoVar y -> y == x
							 other   -> False
    identity_deflt _ = False

    eq_arg binder (CoVarAtom x) = binder == x
    eq_arg _      _	       = False
\end{code}

The catch-all case

\begin{code}
mkCoCase other_scrut other_alts = returnSmpl (CoCase other_scrut other_alts)
\end{code}

Boring local functions used above.  They simply introduce a trivial binding
for the binder, d', in an inner default; either
	let d' = deflt_var in rhs
or
	case deflt_var of d' -> rhs
depending on whether it's an algebraic or primitive case.

\begin{code}
munge_prim_deflt _ CoNoDefault = CoNoDefault

munge_prim_deflt deflt_var (CoBindDefault d' rhs) 
  =   CoBindDefault deflt_var (CoCase (CoVar deflt_var)
			 	      (CoPrimAlts [] (CoBindDefault d' rhs)))

munge_alg_deflt _ CoNoDefault = CoNoDefault

munge_alg_deflt deflt_var (CoBindDefault d' rhs) 
  =   CoBindDefault deflt_var (CoLet (CoNonRec d' (CoVar deflt_var)) rhs)

-- This line caused a generic version of munge_deflt (ie one used for
-- both alg and prim) to space leak massively.  No idea why.
--  = CoBindDefault deflt_var (mkCoLetUnboxedToCase (CoNonRec d' (CoVar deflt_var)) rhs)
\end{code}

\begin{code}
	-- A cheap equality test which bales out fast!
cheap_eq :: InExpr -> InExpr -> Bool
cheap_eq (CoVar v1) (CoVar v2) = v1==v2
cheap_eq (CoLit l1) (CoLit l2) = l1==l2
cheap_eq (CoCon con1 tys1 args1) (CoCon con2 tys2 args2) = (con1==con2) && 
							   (args1 `eq_args` args2)
							   -- Types bound to be equal
cheap_eq (CoPrim op1 tys1 args1) (CoPrim op2 tys2 args2) = (op1==op2) &&
							   (args1 `eq_args` args2)
							   -- Types bound to be equal
cheap_eq (CoApp   f1 a1) (CoApp   f2 a2) = (f1 `cheap_eq` f2) && (a1 `eq_atom` a2)
cheap_eq (CoTyApp f1 t1) (CoTyApp f2 t2) = (f1 `cheap_eq` f2) && (t1 == t2)
cheap_eq _ _ = False

-- ToDo: make CoreAtom an instance of Eq
eq_args (arg1: args1) (arg2 : args2) = (arg1 `eq_atom` arg2) && (args1 `eq_args` args2)
eq_args []		       []		      = True
eq_args other1		       other2		      = False

eq_atom (CoLitAtom l1) (CoLitAtom l2) =  l1==l2
eq_atom (CoVarAtom v1) (CoVarAtom v2) =  v1==v2
eq_atom other1	       other2	      =  False
\end{code}
