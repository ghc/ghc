%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplUtils]{The simplifier utilities}

\begin{code}
module SimplUtils (
	simplBinder, simplBinders, simplIds,
	transformRhs,
	etaCoreExpr, 
	mkCase, findAlt, findDefault,
	mkCoerce
    ) where

#include "HsVersions.h"

import BinderInfo
import CmdLineOpts	( opt_SimplDoLambdaEtaExpansion, opt_SimplCaseMerge )
import CoreSyn
import CoreFVs		( exprFreeVars )
import CoreUtils	( exprIsTrivial, cheapEqExpr, coreExprType, exprIsCheap, exprGenerousArity )
import Subst		( substBndrs, substBndr, substIds )
import Id		( Id, idType, getIdArity, isId, idName,
			  getInlinePragma, setInlinePragma,
			  getIdDemandInfo, mkId, idInfo
			)
import IdInfo		( arityLowerBound, InlinePragInfo(..), setInlinePragInfo, vanillaIdInfo )
import Maybes		( maybeToBool, catMaybes )
import Const		( Con(..) )
import Name		( isLocalName, setNameUnique )
import SimplMonad
import Type		( Type, tyVarsOfType, tyVarsOfTypes, mkForAllTys, seqType,
			  splitTyConApp_maybe, mkTyVarTys, applyTys, splitFunTys, mkFunTys
			)
import TysPrim		( statePrimTyCon )
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

\begin{code}
simplBinders :: [InBinder] -> ([OutBinder] -> SimplM a) -> SimplM a
simplBinders bndrs thing_inside
  = getSubst		`thenSmpl` \ subst ->
    let
	(subst', bndrs') = substBndrs subst bndrs
    in
    seqBndrs bndrs'	`seq`
    setSubst subst' (thing_inside bndrs')

simplBinder :: InBinder -> (OutBinder -> SimplM a) -> SimplM a
simplBinder bndr thing_inside
  = getSubst		`thenSmpl` \ subst ->
    let
	(subst', bndr') = substBndr subst bndr
    in
    seqBndr bndr'	`seq`
    setSubst subst' (thing_inside bndr')


-- Same semantics as simplBinders, but a little less 
-- plumbing and hence a little more efficient.
-- Maybe not worth the candle?
simplIds :: [InBinder] -> ([OutBinder] -> SimplM a) -> SimplM a
simplIds ids thing_inside
  = getSubst		`thenSmpl` \ subst ->
    let
	(subst', bndrs') = substIds subst ids
    in
    seqBndrs bndrs'	`seq`
    setSubst subst' (thing_inside bndrs')

seqBndrs [] = ()
seqBndrs (b:bs) = seqBndr b `seq` seqBndrs bs

seqBndr b | isTyVar b = b `seq` ()
	  | otherwise = seqType (idType b)	`seq`
			idInfo b		`seq`
			()
\end{code}


%************************************************************************
%*									*
\subsection{Transform a RHS}
%*									*
%************************************************************************

Try (a) eta expansion
    (b) type-lambda swizzling

\begin{code}
transformRhs :: InExpr -> SimplM InExpr
transformRhs rhs 
  = tryEtaExpansion body		`thenSmpl` \ body' ->
    mkRhsTyLam tyvars body'
  where
    (tyvars, body) = collectTyBinders rhs
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

[May 1999]  If we do this transformation *regardless* then we can
end up with some pretty silly stuff.  For example, 

	let 
	    st = /\ s -> let { x1=r1 ; x2=r2 } in ...
	in ..
becomes
	let y1 = /\s -> r1
	    y2 = /\s -> r2
	    st = /\s -> ...[y1 s/x1, y2 s/x2]
	in ..

Unless the "..." is a WHNF there is really no point in doing this.
Indeed it can make things worse.  Suppose x1 is used strictly,
and is of the form

	x1* = case f y of { (a,b) -> e }

If we abstract this wrt the tyvar we then can't do the case inline
as we would normally do.


\begin{code}
mkRhsTyLam tyvars body			-- Only does something if there's a let
  | null tyvars || not (worth_it body)	-- inside a type lambda, and a WHNF inside that
  = returnSmpl (mkLams tyvars body)
  | otherwise
  = go (\x -> x) body
  where
    worth_it (Let _ e)	     = whnf_in_middle e
    worth_it other     	     = False
    whnf_in_middle (Let _ e) = whnf_in_middle e
    whnf_in_middle e	     = exprIsCheap e

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
		-- Here, b isn't free in x's type, but we must nevertheless
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
      = getUniqueSmpl		`thenSmpl` \ uniq ->
	let
	    poly_name = setNameUnique (idName var) uniq		-- Keep same name
	    poly_ty   = mkForAllTys tyvars_here (idType var)	-- But new type of course

		-- It's crucial to copy the inline-prag of the original var, because
		-- we're looking at occurrence-analysed but as yet unsimplified code!
		-- In particular, we mustn't lose the loop breakers.
		-- 
		-- It's even right to retain single-occurrence or dead-var info:
		-- Suppose we started with  /\a -> let x = E in B
		-- where x occurs once in E. Then we transform to:
		--	let x' = /\a -> E in /\a -> let x* = x' a in B
		-- where x* has an INLINE prag on it.  Now, once x* is inlined,
		-- the occurrences of x' will be just the occurrences originaly
		-- pinned on x.
	    poly_info = vanillaIdInfo `setInlinePragInfo` getInlinePragma var

	    poly_id   = mkId poly_name poly_ty poly_info
	in
	returnSmpl (poly_id, mkTyApps (Var poly_id) (mkTyVarTys tyvars_here))

    mk_silly_bind var rhs = NonRec (setInlinePragma var IMustBeINLINEd) rhs
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
		-- The silly binding for g* must be IMustBeINLINEs, so that
		-- we simply substitute for g* throughout.
\end{code}


%************************************************************************
%*									*
\subsection{Eta expansion}
%*									*
%************************************************************************

	Try eta expansion for RHSs

We go for:
		\x1..xn -> N	==>   \x1..xn y1..ym -> N y1..ym
	AND		
		N E1..En	==>   let z1=E1 .. zn=En in \y1..ym -> N z1..zn y1..ym

where (in both cases) N is a NORMAL FORM (i.e. no redexes anywhere)
wanting a suitable number of extra args.

NB: the Ei may have unlifted type, but the simplifier (which is applied
to the result) deals OK with this.

There is no point in looking for a combination of the two, 
because that would leave use with some lets sandwiched between lambdas;
that's what the final test in the first equation is for.

\begin{code}
tryEtaExpansion :: InExpr -> SimplM InExpr
tryEtaExpansion rhs
  |  not opt_SimplDoLambdaEtaExpansion
  || exprIsTrivial rhs				-- Don't eta-expand a trival RHS
  || null y_tys					-- No useful expansion
  || not (null x_bndrs || and trivial_args)	-- Not (no x-binders or no z-binds)
  = returnSmpl rhs

  | otherwise	-- Consider eta expansion
  = newIds y_tys						$ ( \ y_bndrs ->
    tick (EtaExpansion (head y_bndrs))				`thenSmpl_`
    mapAndUnzipSmpl bind_z_arg (args `zip` trivial_args)	`thenSmpl` (\ (maybe_z_binds, z_args) ->
    returnSmpl (mkLams x_bndrs				$ 
		mkLets (catMaybes maybe_z_binds)	$
 		mkLams y_bndrs				$
		mkApps (mkApps fun z_args) (map Var y_bndrs))))
  where
    (x_bndrs, body) = collectValBinders rhs
    (fun, args)	    = collectArgs body
    trivial_args    = map exprIsTrivial args
    fun_arity	    = exprGenerousArity fun

    bind_z_arg (arg, trivial_arg) 
	| trivial_arg = returnSmpl (Nothing, arg)
        | otherwise   = newId (coreExprType arg)	$ \ z ->
			returnSmpl (Just (NonRec z arg), Var z)

	-- Note: I used to try to avoid the coreExprType call by using
	-- the type of the binder.  But this type doesn't necessarily
	-- belong to the same substitution environment as this rhs;
	-- and we are going to make extra term binders (y_bndrs) from the type
	-- which will be processed with the rhs substitution environment.
	-- This only went wrong in a mind bendingly complicated case.
    (potential_extra_arg_tys, inner_ty) = splitFunTys (coreExprType body)
	
    y_tys :: [InType]
    y_tys  = take no_extras_wanted potential_extra_arg_tys
	
    no_extras_wanted :: Int
    no_extras_wanted = 0 `max`

	-- We used to expand the arity to the previous arity fo the
	-- function; but this is pretty dangerous.  Consdier
	--	f = \xy -> e
	-- so that f has arity 2.  Now float something into f's RHS:
	--	f = let z = BIG in \xy -> e
	-- The last thing we want to do now is to put some lambdas
	-- outside, to get
	--	f = \xy -> let z = BIG in e
	--
	-- (bndr_arity - no_of_xs)		`max`

	-- See if the body could obviously do with more args
	(fun_arity - valArgCount args)

-- This case is now deal with by exprGenerousArity
	-- Finally, see if it's a state transformer, and xs is non-null
	-- (so it's also a function not a thunk) in which
	-- case we eta-expand on principle! This can waste work,
	-- but usually doesn't.
	-- I originally checked for a singleton type [ty] in this case
	-- but then I found a situation in which I had
	--	\ x -> let {..} in \ s -> f (...) s
	-- AND f RETURNED A FUNCTION.  That is, 's' wasn't the only
	-- potential extra arg.
--	case (x_bndrs, potential_extra_arg_tys) of
--	    (_:_, ty:_)  -> case splitTyConApp_maybe ty of
--				  Just (tycon,_) | tycon == statePrimTyCon -> 1
--				  other					   -> 0
--	    other -> 0
\end{code}


%************************************************************************
%*									*
\subsection{Eta reduction}
%*									*
%************************************************************************

@etaCoreExpr@ trys an eta reduction at the top level of a Core Expr.

e.g.	\ x y -> f x y	===>  f

It is used
-- OLD
--	a) Before constructing an Unfolding, to 
--	   try to make the unfolding smaller;
	b) In tidyCoreExpr, which is done just before converting to STG.

But we only do this if 
	i) It gets rid of a whole lambda, not part.
	   The idea is that lambdas are often quite helpful: they indicate
	   head normal forms, so we don't want to chuck them away lightly.

-- OLD: in core2stg we want to do this even if the result isn't trivial
--	ii) It exposes a simple variable or a type application; in short
--	    it exposes a "trivial" expression. (exprIsTrivial)

\begin{code}
etaCoreExpr :: CoreExpr -> CoreExpr
		-- ToDo: we should really check that we don't turn a non-bottom
		-- lambda into a bottom variable.  Sigh

etaCoreExpr expr@(Lam bndr body)
  = check (reverse binders) body
  where
    (binders, body) = collectBinders expr

    check [] body
	| not (any (`elemVarSet` body_fvs) binders)
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
\subsection{Case absorption and identity-case elimination}
%*									*
%************************************************************************

\begin{code}
mkCase :: OutExpr -> OutId -> [OutAlt] -> SimplM OutExpr
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
mkCase scrut outer_bndr outer_alts
  |  opt_SimplCaseMerge
  && maybeToBool maybe_case_in_default
     
  = tick (CaseMerge outer_bndr)		`thenSmpl_`
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
mkCase scrut case_bndr alts
  | all identity_alt alts
  = tick (CaseIdentity case_bndr)		`thenSmpl_`
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
mkCase other_scrut case_bndr other_alts
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


mkCoerce to_ty (Note (Coerce _ from_ty) expr) 
  | to_ty == from_ty = expr
  | otherwise	     = Note (Coerce to_ty from_ty) expr
mkCoerce to_ty expr
  = Note (Coerce to_ty (coreExprType expr)) expr
\end{code}
