%
% (c) The AQUA Project, Glasgow University, 1993-1998
%
\section[SimplUtils]{The simplifier utilities}

\begin{code}
module SimplUtils (
	simplBinder, simplBinders, simplIds,
	transformRhs,
	mkCase, findAlt, findDefault,

	-- The continuation type
	SimplCont(..), DupFlag(..), contIsDupable, contResultType,
	pushArgs, discardCont, countValArgs, countArgs,
	analyseCont, discardInline

    ) where

#include "HsVersions.h"

import BinderInfo
import CmdLineOpts	( opt_SimplDoLambdaEtaExpansion, opt_SimplCaseMerge )
import CoreSyn
import CoreUnfold	( isValueUnfolding )
import CoreFVs		( exprFreeVars )
import CoreUtils	( exprIsTrivial, cheapEqExpr, exprType, exprIsCheap, exprEtaExpandArity )
import Subst		( InScopeSet, mkSubst, substBndrs, substBndr, substIds, lookupIdSubst )
import Id		( Id, idType, isId, idName, 
			  idOccInfo, idUnfolding,
			  idDemandInfo, mkId, idInfo
			)
import IdInfo		( arityLowerBound, setOccInfo, vanillaIdInfo )
import Maybes		( maybeToBool, catMaybes )
import Name		( isLocalName, setNameUnique )
import SimplMonad
import Type		( Type, tyVarsOfType, tyVarsOfTypes, mkForAllTys, seqType,
			  splitTyConApp_maybe, splitAlgTyConApp_maybe, mkTyVarTys, applyTys, splitFunTys, mkFunTys
			)
import DataCon		( dataConRepArity )
import TysPrim		( statePrimTyCon )
import Var		( setVarUnique )
import VarSet
import VarEnv		( SubstEnv, SubstResult(..) )
import UniqSupply	( splitUniqSupply, uniqFromSupply )
import Util		( zipWithEqual, mapAccumL )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{The continuation data type}
%*									*
%************************************************************************

\begin{code}
data SimplCont		-- Strict contexts
  = Stop OutType		-- Type of the result

  | CoerceIt OutType			-- The To-type, simplified
	     SimplCont

  | InlinePlease			-- This continuation makes a function very
	     SimplCont			-- keen to inline itelf

  | ApplyTo  DupFlag 
	     InExpr SubstEnv		-- The argument, as yet unsimplified, 
	     SimplCont			-- and its subst-env

  | Select   DupFlag 
	     InId [InAlt] SubstEnv	-- The case binder, alts, and subst-env
	     SimplCont

  | ArgOf    DupFlag		-- An arbitrary strict context: the argument 
  	     			-- 	of a strict function, or a primitive-arg fn
				-- 	or a PrimOp
	     OutType		-- The type of the expression being sought by the context
				--	f (error "foo") ==> coerce t (error "foo")
				-- when f is strict
				-- We need to know the type t, to which to coerce.
	     (OutExpr -> SimplM OutExprStuff)	-- What to do with the result

instance Outputable SimplCont where
  ppr (Stop _)        		     = ptext SLIT("Stop")
  ppr (ApplyTo dup arg se cont)      = (ptext SLIT("ApplyTo") <+> ppr dup <+> ppr arg) $$ ppr cont
  ppr (ArgOf   dup _ _)   	     = ptext SLIT("ArgOf...") <+> ppr dup
  ppr (Select dup bndr alts se cont) = (ptext SLIT("Select") <+> ppr dup <+> ppr bndr) $$ 
				       (nest 4 (ppr alts)) $$ ppr cont
  ppr (CoerceIt ty cont)	     = (ptext SLIT("CoerceIt") <+> ppr ty) $$ ppr cont
  ppr (InlinePlease cont)	     = ptext SLIT("InlinePlease") $$ ppr cont

data DupFlag = OkToDup | NoDup

instance Outputable DupFlag where
  ppr OkToDup = ptext SLIT("ok")
  ppr NoDup   = ptext SLIT("nodup")

contIsDupable :: SimplCont -> Bool
contIsDupable (Stop _)       		 = True
contIsDupable (ApplyTo  OkToDup _ _ _)   = True
contIsDupable (ArgOf    OkToDup _ _)     = True
contIsDupable (Select   OkToDup _ _ _ _) = True
contIsDupable (CoerceIt _ cont)          = contIsDupable cont
contIsDupable (InlinePlease cont)	 = contIsDupable cont
contIsDupable other			 = False

pushArgs :: SubstEnv -> [InExpr] -> SimplCont -> SimplCont
pushArgs se []         cont = cont
pushArgs se (arg:args) cont = ApplyTo NoDup arg se (pushArgs se args cont)

discardCont :: SimplCont	-- A continuation, expecting
	    -> SimplCont	-- Replace the continuation with a suitable coerce
discardCont (Stop to_ty) = Stop to_ty
discardCont cont	 = CoerceIt to_ty (Stop to_ty)
			 where
			   to_ty = contResultType cont

contResultType :: SimplCont -> OutType
contResultType (Stop to_ty)	     = to_ty
contResultType (ArgOf _ to_ty _)     = to_ty
contResultType (ApplyTo _ _ _ cont)  = contResultType cont
contResultType (CoerceIt _ cont)     = contResultType cont
contResultType (InlinePlease cont)   = contResultType cont
contResultType (Select _ _ _ _ cont) = contResultType cont

countValArgs :: SimplCont -> Int
countValArgs (ApplyTo _ (Type ty) se cont) = countValArgs cont
countValArgs (ApplyTo _ val_arg   se cont) = 1 + countValArgs cont
countValArgs other			   = 0

countArgs :: SimplCont -> Int
countArgs (ApplyTo _ arg se cont) = 1 + countArgs cont
countArgs other			  = 0
\end{code}


Comment about analyseCont
~~~~~~~~~~~~~~~~~~~~~~~~~
We want to avoid inlining an expression where there can't possibly be
any gain, such as in an argument position.  Hence, if the continuation
is interesting (eg. a case scrutinee, application etc.) then we
inline, otherwise we don't.  

Previously some_benefit used to return True only if the variable was
applied to some value arguments.  This didn't work:

	let x = _coerce_ (T Int) Int (I# 3) in
	case _coerce_ Int (T Int) x of
		I# y -> ....

we want to inline x, but can't see that it's a constructor in a case
scrutinee position, and some_benefit is False.

Another example:

dMonadST = _/\_ t -> :Monad (g1 _@_ t, g2 _@_ t, g3 _@_ t)

....  case dMonadST _@_ x0 of (a,b,c) -> ....

we'd really like to inline dMonadST here, but we *don't* want to
inline if the case expression is just

	case x of y { DEFAULT -> ... }

since we can just eliminate this case instead (x is in WHNF).  Similar
applies when x is bound to a lambda expression.  Hence
contIsInteresting looks for case expressions with just a single
default case.

\begin{code}
analyseCont :: InScopeSet -> SimplCont
	    -> ([Bool],		-- Arg-info flags; one for each value argument
		Bool,		-- Context of the result of the call is interesting
		Bool)		-- There was an InlinePlease 

analyseCont in_scope cont 
  = case cont of
	-- The "lone-variable" case is important.  I spent ages
	-- messing about with unsatisfactory varaints, but this is nice.
	-- The idea is that if a variable appear all alone
	--	as an arg of lazy fn, or rhs	Stop
	-- 	as scrutinee of a case		Select
	--	as arg of a strict fn		ArgOf
	-- then we should not inline it (unless there is some other reason,
	-- e.g. is is the sole occurrence).  
	-- Why not?  At least in the case-scrutinee situation, turning
	--	case x of y -> ...
	-- into
	--	let y = (a,b) in ...
	-- is bad if the binding for x will remain.
	--
	-- Another example: I discovered that strings
	-- were getting inlined straight back into applications of 'error'
	-- because the latter is strict.
	--	s = "foo"
	--	f = \x -> ...(error s)...

	-- Fundamentally such contexts should not ecourage inlining becuase
	-- the context can ``see'' the unfolding of the variable (e.g. case or a RULE)
	-- so there's no gain.
	--
	-- However, even a type application isn't a lone variable.  Consider
	--	case $fMonadST @ RealWorld of { :DMonad a b c -> c }
	-- We had better inline that sucker!  The case won't see through it.

      (Stop _)           	  -> boring_result		-- Don't inline a lone variable
      (Select _ _ _ _ _) 	  -> boring_result		-- Ditto
      (ArgOf _ _ _)      	  -> boring_result		-- Ditto
      (ApplyTo _ (Type _) _ cont) -> analyse_ty_app cont
      other		 	  -> analyse_app cont
  where
    boring_result = ([], False, False)

		-- For now, I'm treating not treating a variable applied to types as
		-- "lone". The motivating example was
		--	f = /\a. \x. BIG
		--	g = /\a. \y.  h (f a)
		-- There's no advantage in inlining f here, and perhaps
		-- a significant disadvantage.
    analyse_ty_app (Stop _)			= boring_result
    analyse_ty_app (ArgOf _ _ _)      		= boring_result
    analyse_ty_app (Select _ _ _ _ _) 		= ([], True, False)	-- See the $fMonadST example above
    analyse_ty_app (ApplyTo _ (Type _) _ cont)	= analyse_ty_app cont
    analyse_ty_app cont				= analyse_app cont

    analyse_app (InlinePlease cont)  
	= case analyse_app cont of
		 (infos, icont, inline) -> (infos, icont, True)

    analyse_app (ApplyTo _ arg subst cont) 
	| isValArg arg = case analyse_app cont of
			   (infos, icont, inline) -> (analyse_arg subst arg : infos, icont, inline)
	| otherwise    = analyse_app cont

    analyse_app cont = ([], interesting_call_context cont, False)

	-- An argument is interesting if it has *some* structure
	-- We are here trying to avoid unfolding a function that
	-- is applied only to variables that have no unfolding
	-- (i.e. they are probably lambda bound): f x y z
	-- There is little point in inlining f here.
    analyse_arg :: SubstEnv -> InExpr -> Bool
    analyse_arg subst (Var v)	        = case lookupIdSubst (mkSubst in_scope subst) v of
						DoneId v' _ -> isValueUnfolding (idUnfolding v')
						other	    -> False
    analyse_arg subst (Type _)	        = False
    analyse_arg subst (App fn (Type _)) = analyse_arg subst fn
    analyse_arg subst (Note _ a)	= analyse_arg subst a
    analyse_arg subst other	        = True

    interesting_call_context (Stop ty)	     		 = canUpdateInPlace ty
    interesting_call_context (InlinePlease _)	         = True
    interesting_call_context (Select _ _ _ _ _)          = True
    interesting_call_context (CoerceIt _ cont)           = interesting_call_context cont
    interesting_call_context (ApplyTo _ (Type _) _ cont) = interesting_call_context cont
    interesting_call_context (ApplyTo _ _	 _ _)    = True
    interesting_call_context (ArgOf _ _ _)		 = True
	-- If this call is the arg of a strict function, the context
	-- is a bit interesting.  If we inline here, we may get useful
	-- evaluation information to avoid repeated evals: e.g.
	--	x + (y * z)
	-- Here the contIsInteresting makes the '*' keener to inline,
	-- which in turn exposes a constructor which makes the '+' inline.
	-- Assuming that +,* aren't small enough to inline regardless.
	--
	-- It's also very important to inline in a strict context for things
	-- like
	--		foldr k z (f x)
	-- Here, the context of (f x) is strict, and if f's unfolding is
	-- a build it's *great* to inline it here.  So we must ensure that
	-- the context for (f x) is not totally uninteresting.


discardInline :: SimplCont -> SimplCont
discardInline (InlinePlease cont)  = cont
discardInline (ApplyTo d e s cont) = ApplyTo d e s (discardInline cont)
discardInline cont		   = cont

-- Consider   let x = <wurble> in ...
-- If <wurble> returns an explicit constructor, we might be able
-- to do update in place.  So we treat even a thunk RHS context
-- as interesting if update in place is possible.  We approximate
-- this by seeing if the type has a single constructor with a
-- small arity.  But arity zero isn't good -- we share the single copy
-- for that case, so no point in sharing.

canUpdateInPlace ty = case splitAlgTyConApp_maybe ty of
			Just (_, _, [dc]) -> arity == 1 || arity == 2
					  where
					     arity = dataConRepArity dc
			other -> False
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

		-- It's crucial to copy the occInfo of the original var, because
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
	    poly_info = vanillaIdInfo `setOccInfo` idOccInfo var

	    poly_id   = mkId poly_name poly_ty poly_info
	in
	returnSmpl (poly_id, mkTyApps (Var poly_id) (mkTyVarTys tyvars_here))

    mk_silly_bind var rhs = NonRec var rhs
		-- The Inline note is really important!  If we don't say 
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
		-- The silly binding for g* must be INLINEd, so that
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
    fun_arity	    = exprEtaExpandArity fun

    bind_z_arg (arg, trivial_arg) 
	| trivial_arg = returnSmpl (Nothing, arg)
        | otherwise   = newId (exprType arg)	$ \ z ->
			returnSmpl (Just (NonRec z arg), Var z)

	-- Note: I used to try to avoid the exprType call by using
	-- the type of the binder.  But this type doesn't necessarily
	-- belong to the same substitution environment as this rhs;
	-- and we are going to make extra term binders (y_bndrs) from the type
	-- which will be processed with the rhs substitution environment.
	-- This only went wrong in a mind bendingly complicated case.
    (potential_extra_arg_tys, inner_ty) = splitFunTys (exprType body)
	
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

-- This case is now deal with by exprEtaExpandArity
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
    identity_alt (DEFAULT, [], Var v)     = v == case_bndr
    identity_alt (DataAlt con, args, rhs) = cheapEqExpr rhs
					  		(mkConApp con (map Type arg_tys ++ map varToCoreExpr args))
    identity_alt other		          = False

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

findAlt :: AltCon -> [CoreAlt] -> CoreAlt
findAlt con alts
  = go alts
  where
    go [] 	    = pprPanic "Missing alternative" (ppr con $$ vcat (map ppr alts))
    go (alt : alts) | matches alt = alt
    		    | otherwise   = go alts

    matches (DEFAULT, _, _) = True
    matches (con1, _, _)    = con == con1
\end{code}
