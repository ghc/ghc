%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplUtils]{The simplifier utilities}

\begin{code}
module SimplUtils (
	simplBinder, simplBinders, simplIds,
	mkRhsTyLam,		
	etaCoreExpr, 
	etaExpandCount, 
	mkCase, findAlt, findDefault
    ) where

#include "HsVersions.h"

import BinderInfo
import CmdLineOpts	( opt_DoEtaReduction, switchIsOn, SimplifierSwitch(..) )
import CoreSyn
import CoreUtils	( exprIsCheap, exprIsTrivial, exprFreeVars, cheapEqExpr,
			  FormSummary(..),
			  substId, substIds
			)
import Id		( Id, idType, getIdArity, isId, idName,
			  getInlinePragma, setInlinePragma,
			  getIdDemandInfo
			)
import IdInfo		( arityLowerBound, InlinePragInfo(..) )
import Maybes		( maybeToBool )
import Const		( Con(..) )
import Name		( isLocalName )
import SimplMonad
import Type		( Type, tyVarsOfType, tyVarsOfTypes, mkForAllTys, mkTyVarTys,
			  splitTyConApp_maybe, mkTyVarTy, substTyVar
			)
import Var		( setVarUnique )
import VarSet
import UniqSupply	( splitUniqSupply, uniqFromSupply )
import Util		( zipWithEqual, mapAccumL )
import Outputable
\end{code}


%************************************************************************
%*									*
\section{Dealing with a single binder}
%*									*
%************************************************************************

When we hit a binder we may need to
  (a) apply the the type envt (if non-empty) to its type
  (b) apply the type envt and id envt to its SpecEnv (if it has one)
  (c) give it a new unique to avoid name clashes

\begin{code}
simplBinders :: [InBinder] -> ([OutBinder] -> SimplM a) -> SimplM a
simplBinders bndrs thing_inside
  = getSwitchChecker	`thenSmpl` \ sw_chkr ->
    getSimplBinderStuff `thenSmpl` \ stuff ->
    let
	must_clone       = switchIsOn sw_chkr SimplPleaseClone
	(stuff', bndrs') = mapAccumL (subst_binder must_clone) stuff bndrs
    in
    setSimplBinderStuff stuff' 	$
    thing_inside bndrs'

simplBinder :: InBinder -> (OutBinder -> SimplM a) -> SimplM a
simplBinder bndr thing_inside
  = getSwitchChecker	`thenSmpl` \ sw_chkr ->
    getSimplBinderStuff `thenSmpl` \ stuff ->
    let
	must_clone      = switchIsOn sw_chkr SimplPleaseClone
	(stuff', bndr') = subst_binder must_clone stuff bndr
    in
    setSimplBinderStuff stuff' 	$
    thing_inside bndr'

-- Same semantics as simplBinders, but a little less 
-- plumbing and hence a little more efficient.
-- Maybe not worth the candle?
simplIds :: [InBinder] -> ([OutBinder] -> SimplM a) -> SimplM a
simplIds ids thing_inside
  = getSwitchChecker	`thenSmpl` \ sw_chkr ->
    getSimplBinderStuff `thenSmpl` \ (ty_subst, id_subst, in_scope, us) ->
    let
	must_clone			  = switchIsOn sw_chkr SimplPleaseClone
	(id_subst', in_scope', us', ids') = substIds (simpl_clone_fn must_clone)
						     ty_subst id_subst in_scope us ids
    in
    setSimplBinderStuff (ty_subst, id_subst', in_scope', us') 	$
    thing_inside ids'

subst_binder must_clone (ty_subst, id_subst, in_scope, us) bndr
  | isTyVar bndr
  = case substTyVar ty_subst in_scope bndr of
	(ty_subst', in_scope', bndr') -> ((ty_subst', id_subst, in_scope', us), bndr')

  | otherwise
  = case substId (simpl_clone_fn must_clone) ty_subst id_subst in_scope us bndr of
	(id_subst', in_scope', us', bndr')
		-> ((ty_subst, id_subst', in_scope', us'), bndr')

simpl_clone_fn must_clone in_scope us id 
  |  (must_clone && isLocalName (idName id))
  || id `elemVarSet` in_scope
  = case splitUniqSupply us of
	(us1, us2) -> Just (us1, setVarUnique id (uniqFromSupply us2))

  |  otherwise
  =  Nothing
\end{code}


%************************************************************************
%*									*
\subsection{Local tyvar-lifting}
%*									*
%************************************************************************

mkRhsTyLam tries this transformation, when the big lambda appears as
the RHS of a let(rec) binding:

	/\abc -> let(rec) x = e in b
   ==>
	let(rec) x' = /\abc -> let x = x' a b c in e
	in 
	/\abc -> let x = x' a b c in b

This is good because it can turn things like:

	let f = /\a -> letrec g = ... g ... in g
into
	letrec g' = /\a -> ... g' a ...
	in
	let f = /\ a -> g' a

which is better.  In effect, it means that big lambdas don't impede
let-floating.

This optimisation is CRUCIAL in eliminating the junk introduced by
desugaring mutually recursive definitions.  Don't eliminate it lightly!

So far as the implemtation is concerned:

	Invariant: go F e = /\tvs -> F e
	
	Equalities:
		go F (Let x=e in b)
		= Let x' = /\tvs -> F e 
		  in 
		  go G b
		where
		    G = F . Let x = x' tvs
	
		go F (Letrec xi=ei in b)
		= Letrec {xi' = /\tvs -> G ei} 
		  in
		  go G b
		where
		  G = F . Let {xi = xi' tvs}

\begin{code}
mkRhsTyLam (Lam b e)
 | isTyVar b = case collectTyBinders e of
		  (bs,body) -> mkRhsTyLam_help (b:bs) body

mkRhsTyLam other_expr		-- No-op if not a type lambda
  = returnSmpl other_expr


mkRhsTyLam_help tyvars body
  = go (\x -> x) body
  where
    main_tyvar_set = mkVarSet tyvars

    go fn (Let bind@(NonRec var rhs) body) | exprIsTrivial rhs
      = go (fn . Let bind) body

    go fn (Let bind@(NonRec var rhs) body)
      = mk_poly tyvars_here var				`thenSmpl` \ (var', rhs') ->
	go (fn . Let (mk_silly_bind var rhs')) body	`thenSmpl` \ body' ->
	returnSmpl (Let (NonRec var' (mkLams tyvars_here (fn rhs))) body')
      where
	tyvars_here = tyvars
		-- varSetElems (main_tyvar_set `intersectVarSet` tyVarsOfType var_ty)
		-- tyvars_here was an attempt to reduce the number of tyvars
		-- wrt which the new binding is abstracted.  But the naive
		-- approach of abstract wrt the tyvars free in the Id's type
		-- fails. Consider:
		--	/\ a b -> let t :: (a,b) = (e1, e2)
		--		      x :: a     = fst t
		--		  in ...
		-- Here, b isn't free in a's type, but we must nevertheless
		-- abstract wrt b as well, because t's type mentions b.
		-- Since t is floated too, we'd end up with the bogus:
		--	poly_t = /\ a b -> (e1, e2)
		--	poly_x = /\ a   -> fst (poly_t a *b*)
		-- So for now we adopt the even more naive approach of
		-- abstracting wrt *all* the tyvars.  We'll see if that
		-- gives rise to problems.   SLPJ June 98

	var_ty = idType var

    go fn (Let (Rec prs) body)
       = mapAndUnzipSmpl (mk_poly tyvars_here) vars	`thenSmpl` \ (vars', rhss') ->
	 let
	    gn body = fn $ foldr Let body (zipWith mk_silly_bind vars rhss')
	 in
	 go gn body				`thenSmpl` \ body' ->
	 returnSmpl (Let (Rec (vars' `zip` [mkLams tyvars_here (gn rhs) | rhs <- rhss])) body')
       where
	 (vars,rhss) = unzip prs
	 tyvars_here = tyvars
		-- varSetElems (main_tyvar_set `intersectVarSet` tyVarsOfTypes var_tys)
		-- See notes with tyvars_here above

	 var_tys     = map idType vars

    go fn body = returnSmpl (mkLams tyvars (fn body))

    mk_poly tyvars_here var
      = newId (mkForAllTys tyvars_here (idType var))	$ \ poly_id ->
	let
		-- It's crucial to copy the inline-prag of the original var, because
		-- we're looking at occurrence-analysed but as yet unsimplified code!
		-- In particular, we mustn't lose the loop breakers.
		-- 
		-- *However* we don't want to retain a single-occurrence or dead-var info
		-- because we're adding a load of "silly bindings" of the form
		--	var _U_ = poly_var t1 t2
		-- with a must-inline pragma on the silly binding to prevent the
		-- poly-var from being inlined right back in.  Since poly_var now
		-- occurs inside an INLINE binding, it should be given a ManyOcc,
		-- else it may get inlined unconditionally
	    poly_inline_prag = case getInlinePragma var of
				  ICanSafelyBeINLINEd _ _ -> NoInlinePragInfo
				  IAmDead		  -> NoInlinePragInfo
				  var_inline_prag	  -> var_inline_prag

	    poly_id' = setInlinePragma poly_id poly_inline_prag
	in
	returnSmpl (poly_id', mkTyApps (Var poly_id') (mkTyVarTys tyvars_here))

    mk_silly_bind var rhs = NonRec (setInlinePragma var IWantToBeINLINEd) rhs
		-- The addInlinePragma is really important!  If we don't say 
		-- INLINE on these silly little bindings then look what happens!
		-- Suppose we start with:
		--
		--	x = let g = /\a -> \x -> f x x
		--	    in 
		--	    /\ b -> let g* = g b in E
		--
		-- Then: 	* the binding for g gets floated out
		-- 		* but then it gets inlined into the rhs of g*
		--		* then the binding for g* is floated out of the /\b
		--		* so we're back to square one
		-- The silly binding for g* must be INLINE, so that no inlining
		-- will happen in its RHS.
		-- PS: Jun 98: actually this isn't important any more; 
		--	       inlineUnconditionally will catch the type applicn
		--	       and inline it unconditionally, without ever trying
		-- 	       to simplify the RHS
\end{code}


%************************************************************************
%*									*
\subsection{Eta reduction}
%*									*
%************************************************************************

@etaCoreExpr@ trys an eta reduction at the top level of a Core Expr.

e.g.	\ x y -> f x y	===>  f

It is used
	a) Before constructing an Unfolding, to 
	   try to make the unfolding smaller;
	b) In tidyCoreExpr, which is done just before converting to STG.

But we only do this if 
	i) It gets rid of a whole lambda, not part.
	   The idea is that lambdas are often quite helpful: they indicate
	   head normal forms, so we don't want to chuck them away lightly.

	ii) It exposes a simple variable or a type application; in short
	    it exposes a "trivial" expression. (exprIsTrivial)

\begin{code}
etaCoreExpr :: CoreExpr -> CoreExpr
		-- ToDo: we should really check that we don't turn a non-bottom
		-- lambda into a bottom variable.  Sigh

etaCoreExpr expr@(Lam bndr body)
  | opt_DoEtaReduction
  = check (reverse binders) body
  where
    (binders, body) = collectBinders expr

    check [] body
	| exprIsTrivial body && not (any (`elemVarSet` body_fvs) binders)
	= body			-- Success!
	where
	  body_fvs = exprFreeVars body

    check (b : bs) (App fun arg)
	|  (varToCoreExpr b `cheapEqExpr` arg)
	= check bs fun

    check _ _ = expr	-- Bale out

etaCoreExpr expr = expr		-- The common case
\end{code}
	

%************************************************************************
%*									*
\subsection{Eta expansion}
%*									*
%************************************************************************

@etaExpandCount@ takes an expression, E, and returns an integer n,
such that

	E  ===>   (\x1::t1 x1::t2 ... xn::tn -> E x1 x2 ... xn)

is a safe transformation.  In particular, the transformation should
not cause work to be duplicated, unless it is ``cheap'' (see
@manifestlyCheap@ below).

@etaExpandCount@ errs on the conservative side.  It is always safe to
return 0.

An application of @error@ is special, because it can absorb as many
arguments as you care to give it.  For this special case we return
100, to represent "infinity", which is a bit of a hack.

\begin{code}
etaExpandCount :: CoreExpr
	       -> Int	-- Number of extra args you can safely abstract

etaExpandCount (Lam b body)
  | isId b
  = 1 + etaExpandCount body

etaExpandCount (Let bind body)
  | all exprIsCheap (rhssOfBind bind)
  = etaExpandCount body

etaExpandCount (Case scrut _ alts)
  | exprIsCheap scrut
  = minimum [etaExpandCount rhs | (_,_,rhs) <- alts]

etaExpandCount fun@(Var _)     = eta_fun fun

etaExpandCount (App fun (Type ty))
  = eta_fun fun
etaExpandCount (App fun arg)
  | exprIsCheap arg = case etaExpandCount fun of
				0 -> 0
				n -> n-1	-- Knock off one

etaExpandCount other = 0    -- Give up
	-- Lit, Con, Prim,
	-- non-val Lam,
	-- Scc (pessimistic; ToDo),
	-- Let with non-whnf rhs(s),
	-- Case with non-whnf scrutinee

-----------------------------
eta_fun :: CoreExpr	 -- The function
	-> Int		 -- How many args it can safely be applied to

eta_fun (App fun (Type ty)) = eta_fun fun
eta_fun (Var v) 	    = arityLowerBound (getIdArity v)
eta_fun other 		    = 0		-- Give up
\end{code}


%************************************************************************
%*									*
\subsection{Case absorption and identity-case elimination}
%*									*
%************************************************************************

\begin{code}
mkCase :: SwitchChecker -> OutExpr -> OutId -> [OutAlt] -> SimplM OutExpr
\end{code}

@mkCase@ tries the following transformation (if possible):

case e of b {             ==>   case e of b {
  p1 -> rhs1	                  p1 -> rhs1
  ...	                          ...
  pm -> rhsm                      pm -> rhsm
  _  -> case b of b' {            pn -> rhsn[b/b'] {or (alg)  let b=b' in rhsn}
						   {or (prim) case b of b' { _ -> rhsn}}
	      pn -> rhsn          ...
   	      ...                 po -> rhso[b/b']
	      po -> rhso          _  -> rhsd[b/b'] {or let b'=b in rhsd}
	      _  -> rhsd
}

which merges two cases in one case when -- the default alternative of
the outer case scrutises the same variable as the outer case This
transformation is called Case Merging.  It avoids that the same
variable is scrutinised multiple times.

\begin{code}
mkCase sw_chkr scrut outer_bndr outer_alts
  |  switchIsOn sw_chkr SimplCaseMerge
  && maybeToBool maybe_case_in_default
     
  = tick CaseMerge			`thenSmpl_`
    returnSmpl (Case scrut outer_bndr new_alts)
	-- Warning: don't call mkCase recursively!
	-- Firstly, there's no point, because inner alts have already had
	-- mkCase applied to them, so they won't have a case in their default
	-- Secondly, if you do, you get an infinite loop, because the bindNonRec
	-- in munge_rhs puts a case into the DEFAULT branch!
  where
    new_alts = outer_alts_without_deflt ++ munged_inner_alts
    maybe_case_in_default = case findDefault outer_alts of
				(outer_alts_without_default,
				 Just (Case (Var scrut_var) inner_bndr inner_alts))
				 
				   | outer_bndr == scrut_var
				   -> Just (outer_alts_without_default, inner_bndr, inner_alts)
				other -> Nothing

    Just (outer_alts_without_deflt, inner_bndr, inner_alts) = maybe_case_in_default

		--  Eliminate any inner alts which are shadowed by the outer ones
    outer_cons = [con | (con,_,_) <- outer_alts_without_deflt]

    munged_inner_alts = [ (con, args, munge_rhs rhs) 
		        | (con, args, rhs) <- inner_alts, 
			   not (con `elem` outer_cons)	-- Eliminate shadowed inner alts
		        ]
    munge_rhs rhs = bindNonRec inner_bndr (Var outer_bndr) rhs
\end{code}

Now the identity-case transformation:

	case e of 		===> e
		True -> True;
		False -> False

and similar friends.

\begin{code}
mkCase sw_chkr scrut case_bndr alts
  | all identity_alt alts
  = tick CaseIdentity		`thenSmpl_`
    returnSmpl scrut
  where
    identity_alt (DEFAULT, [], Var v)	     = v == case_bndr
    identity_alt (con, args, Con con' args') = con == con' && 
					       and (zipWithEqual "mkCase" 
							cheapEqExpr 
							(map Type arg_tys ++ map varToCoreExpr args)
							args')
    identity_alt other			     = False

    arg_tys = case splitTyConApp_maybe (idType case_bndr) of
		Just (tycon, arg_tys) -> arg_tys
\end{code}

The catch-all case

\begin{code}
mkCase sw_chkr other_scrut case_bndr other_alts
  = returnSmpl (Case other_scrut case_bndr other_alts)
\end{code}


\begin{code}
findDefault :: [CoreAlt] -> ([CoreAlt], Maybe CoreExpr)
findDefault []				= ([], Nothing)
findDefault ((DEFAULT,args,rhs) : alts) = ASSERT( null alts && null args ) 
					  ([], Just rhs)
findDefault (alt : alts) 	        = case findDefault alts of 
					    (alts', deflt) -> (alt : alts', deflt)

findAlt :: Con -> [CoreAlt] -> CoreAlt
findAlt con alts
  = go alts
  where
    go [] 	    = pprPanic "Missing alternative" (ppr con $$ vcat (map ppr alts))
    go (alt : alts) | matches alt = alt
    		    | otherwise   = go alts

    matches (DEFAULT, _, _) = True
    matches (con1, _, _)    = con == con1
\end{code}
