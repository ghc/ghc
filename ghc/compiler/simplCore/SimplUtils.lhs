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
	countValArgs, countArgs, mkRhsStop, mkStop,
	getContArgs, interestingCallContext, interestingArg, isStrictType, discardInline

    ) where

#include "HsVersions.h"

import CmdLineOpts	( switchIsOn, SimplifierSwitch(..),
			  opt_SimplDoLambdaEtaExpansion, opt_SimplCaseMerge, opt_DictsStrict,
			  opt_UF_UpdateInPlace
			)
import CoreSyn
import CoreUnfold	( isValueUnfolding )
import CoreUtils	( exprIsTrivial, cheapEqExpr, exprType, exprIsCheap, exprEtaExpandArity, bindNonRec )
import Subst		( InScopeSet, mkSubst, substBndrs, substBndr, substIds, substExpr )
import Id		( Id, idType, isId, idName, 
			  idOccInfo, idUnfolding, idStrictness,
			  mkId, idInfo
			)
import IdInfo		( StrictnessInfo(..), ArityInfo, atLeastArity, setOccInfo, vanillaIdInfo )
import Maybes		( maybeToBool, catMaybes )
import Name		( isLocalName, setNameUnique )
import Demand		( Demand, isStrict, wwLazy, wwLazy )
import SimplMonad
import Type		( Type, tyVarsOfType, tyVarsOfTypes, mkForAllTys, seqType, repType,
			  splitTyConApp_maybe, mkTyVarTys, applyTys, splitFunTys, mkFunTys,
			  isDictTy, isDataType, applyTy, splitFunTy, isUnLiftedType,
			  splitRepFunTys
			)
import TyCon		( tyConDataConsIfAvailable )
import DataCon		( dataConRepArity )
import VarSet
import VarEnv		( SubstEnv, SubstResult(..) )
import Util		( lengthExceeds )
import BasicTypes	( Arity )
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection{The continuation data type}
%*									*
%************************************************************************

\begin{code}
data SimplCont		-- Strict contexts
  = Stop     OutType		-- Type of the result
	     Bool		-- True => This is the RHS of a thunk whose type suggests
				--	   that update-in-place would be possible
				--	   (This makes the inliner a little keener.)

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
	     OutType		-- cont_ty: the type of the expression being sought by the context
				--	f (error "foo") ==> coerce t (error "foo")
				-- when f is strict
				-- We need to know the type t, to which to coerce.
	     (OutExpr -> SimplM OutExprStuff)	-- What to do with the result
				-- The result expression in the OutExprStuff has type cont_ty

instance Outputable SimplCont where
  ppr (Stop _ _)       		     = ptext SLIT("Stop")
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


-------------------
mkRhsStop, mkStop :: OutType -> SimplCont
mkStop    ty = Stop ty False
mkRhsStop ty = Stop ty (canUpdateInPlace ty)


-------------------
contIsDupable :: SimplCont -> Bool
contIsDupable (Stop _ _)       		 = True
contIsDupable (ApplyTo  OkToDup _ _ _)   = True
contIsDupable (ArgOf    OkToDup _ _)     = True
contIsDupable (Select   OkToDup _ _ _ _) = True
contIsDupable (CoerceIt _ cont)          = contIsDupable cont
contIsDupable (InlinePlease cont)	 = contIsDupable cont
contIsDupable other			 = False

-------------------
discardInline :: SimplCont -> SimplCont
discardInline (InlinePlease cont)  = cont
discardInline (ApplyTo d e s cont) = ApplyTo d e s (discardInline cont)
discardInline cont		   = cont

-------------------
discardableCont :: SimplCont -> Bool
discardableCont (Stop _ _)	    = False
discardableCont (CoerceIt _ cont)   = discardableCont cont
discardableCont (InlinePlease cont) = discardableCont cont
discardableCont other		    = True

discardCont :: SimplCont	-- A continuation, expecting
	    -> SimplCont	-- Replace the continuation with a suitable coerce
discardCont cont = case cont of
		     Stop to_ty _ -> cont
		     other        -> CoerceIt to_ty (mkStop to_ty)
		 where
		   to_ty = contResultType cont

-------------------
contResultType :: SimplCont -> OutType
contResultType (Stop to_ty _)	     = to_ty
contResultType (ArgOf _ to_ty _)     = to_ty
contResultType (ApplyTo _ _ _ cont)  = contResultType cont
contResultType (CoerceIt _ cont)     = contResultType cont
contResultType (InlinePlease cont)   = contResultType cont
contResultType (Select _ _ _ _ cont) = contResultType cont

-------------------
countValArgs :: SimplCont -> Int
countValArgs (ApplyTo _ (Type ty) se cont) = countValArgs cont
countValArgs (ApplyTo _ val_arg   se cont) = 1 + countValArgs cont
countValArgs other			   = 0

countArgs :: SimplCont -> Int
countArgs (ApplyTo _ arg se cont) = 1 + countArgs cont
countArgs other			  = 0
\end{code}


\begin{code}
getContArgs :: OutId -> SimplCont 
	    -> SimplM ([(InExpr, SubstEnv, Bool)],	-- Arguments; the Bool is true for strict args
			SimplCont,			-- Remaining continuation
			Bool)				-- Whether we came across an InlineCall
-- getContArgs id k = (args, k', inl)
-- 	args are the leading ApplyTo items in k
--	(i.e. outermost comes first)
--	augmented with demand info from the functionn
getContArgs fun orig_cont
  = getSwitchChecker 	`thenSmpl` \ chkr ->
    let
		-- Ignore strictness info if the no-case-of-case
		-- flag is on.  Strictness changes evaluation order
		-- and that can change full laziness
	stricts | switchIsOn chkr NoCaseOfCase = vanilla_stricts
		| otherwise		       = computed_stricts
    in
    go [] stricts False orig_cont
  where
    ----------------------------

	-- Type argument
    go acc ss inl (ApplyTo _ arg@(Type _) se cont)
	= go ((arg,se,False) : acc) ss inl cont
		-- NB: don't bother to instantiate the function type

	-- Value argument
    go acc (s:ss) inl (ApplyTo _ arg se cont)
	= go ((arg,se,s) : acc) ss inl cont

	-- An Inline continuation
    go acc ss inl (InlinePlease cont)
	= go acc ss True cont

	-- We're run out of arguments, or else we've run out of demands
	-- The latter only happens if the result is guaranteed bottom
	-- This is the case for
	--	* case (error "hello") of { ... }
	--	* (error "Hello") arg
	--	* f (error "Hello") where f is strict
	--	etc
    go acc ss inl cont 
	| null ss && discardableCont cont = tick BottomFound	`thenSmpl_`
					    returnSmpl (reverse acc, discardCont cont, inl)
	| otherwise			  = returnSmpl (reverse acc, cont, 	       inl)

    ----------------------------
    vanilla_stricts, computed_stricts :: [Bool]
    vanilla_stricts  = repeat False
    computed_stricts = zipWith (||) fun_stricts arg_stricts

    ----------------------------
    (val_arg_tys, _) = splitRepFunTys (idType fun)
    arg_stricts      = map isStrictType val_arg_tys ++ repeat False
	-- These argument types are used as a cheap and cheerful way to find
	-- unboxed arguments, which must be strict.  But it's an InType
	-- and so there might be a type variable where we expect a function
	-- type (the substitution hasn't happened yet).  And we don't bother
	-- doing the type applications for a polymorphic function.
	-- Hence the split*Rep*FunTys

    ----------------------------
	-- If fun_stricts is finite, it means the function returns bottom
	-- after that number of value args have been consumed
	-- Otherwise it's infinite, extended with False
    fun_stricts
      = case idStrictness fun of
	  StrictnessInfo demands result_bot 
		| not (demands `lengthExceeds` countValArgs orig_cont)
		-> 	-- Enough args, use the strictness given.
			-- For bottoming functions we used to pretend that the arg
			-- is lazy, so that we don't treat the arg as an
			-- interesting context.  This avoids substituting
			-- top-level bindings for (say) strings into 
			-- calls to error.  But now we are more careful about
			-- inlining lone variables, so its ok (see SimplUtils.analyseCont)
		   if result_bot then
			map isStrict demands		-- Finite => result is bottom
		   else
			map isStrict demands ++ vanilla_stricts

	  other -> vanilla_stricts	-- Not enough args, or no strictness


-------------------
isStrictType :: Type -> Bool
	-- isStrictType computes whether an argument (or let RHS) should
	-- be computed strictly or lazily, based only on its type
isStrictType ty
  | isUnLiftedType ty				    = True
  | opt_DictsStrict && isDictTy ty && isDataType ty = True
  | otherwise					    = False 
	-- Return true only for dictionary types where the dictionary
	-- has more than one component (else we risk poking on the component
	-- of a newtype dictionary)

-------------------
interestingArg :: InScopeSet -> InExpr -> SubstEnv -> Bool
	-- An argument is interesting if it has *some* structure
	-- We are here trying to avoid unfolding a function that
	-- is applied only to variables that have no unfolding
	-- (i.e. they are probably lambda bound): f x y z
	-- There is little point in inlining f here.
interestingArg in_scope arg subst
  = analyse (substExpr (mkSubst in_scope subst) arg)
	-- 'analyse' only looks at the top part of the result
	-- and substExpr is lazy, so this isn't nearly as brutal
	-- as it looks.
  where
    analyse (Var v)	      = hasSomeUnfolding (idUnfolding v)
				-- Was: isValueUnfolding (idUnfolding v')
				-- But that seems over-pessimistic
    analyse (Type _)	      = False
    analyse (App fn (Type _)) = analyse fn
    analyse (Note _ a)	      = analyse a
    analyse other	      = True
	-- Consider 	let x = 3 in f x
	-- The substitution will contain (x -> ContEx 3), and we want to
	-- to say that x is an interesting argument.
	-- But consider also (\x. f x y) y
	-- The substitution will contain (x -> ContEx y), and we want to say
	-- that x is not interesting (assuming y has no unfolding)
\end{code}

Comment about interestingCallContext
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
interestingCallContext :: Bool 		-- False <=> no args at all
		       -> Bool		-- False <=> no value args
		       -> SimplCont -> Bool
	-- The "lone-variable" case is important.  I spent ages
	-- messing about with unsatisfactory varaints, but this is nice.
	-- The idea is that if a variable appear all alone
	--	as an arg of lazy fn, or rhs	Stop
	-- 	as scrutinee of a case		Select
	--	as arg of a strict fn		ArgOf
	-- then we should not inline it (unless there is some other reason,
	-- e.g. is is the sole occurrence).  We achieve this by making
	-- interestingCallContext return False for a lone variable.
	--
	-- Why?  At least in the case-scrutinee situation, turning
	--	let x = (a,b) in case x of y -> ...
	-- into
	--	let x = (a,b) in case (a,b) of y -> ...
	-- and thence to 
	--	let x = (a,b) in let y = (a,b) in ...
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
	-- However, even a type application or coercion isn't a lone variable.
	-- Consider
	--	case $fMonadST @ RealWorld of { :DMonad a b c -> c }
	-- We had better inline that sucker!  The case won't see through it.
	--
	-- For now, I'm treating treating a variable applied to types 
	-- in a *lazy* context "lone". The motivating example was
	--	f = /\a. \x. BIG
	--	g = /\a. \y.  h (f a)
	-- There's no advantage in inlining f here, and perhaps
	-- a significant disadvantage.  Hence some_val_args in the Stop case

interestingCallContext some_args some_val_args cont
  = interesting cont
  where
    interesting (InlinePlease _)       = True
    interesting (Select _ _ _ _ _)     = some_args
    interesting (ApplyTo _ _ _ _)      = some_args	-- Can happen if we have (coerce t (f x)) y
    interesting (ArgOf _ _ _)	       = some_val_args
    interesting (Stop ty upd_in_place) = some_val_args && upd_in_place
    interesting (CoerceIt _ cont)      = interesting cont
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


-------------------
canUpdateInPlace :: Type -> Bool
-- Consider   let x = <wurble> in ...
-- If <wurble> returns an explicit constructor, we might be able
-- to do update in place.  So we treat even a thunk RHS context
-- as interesting if update in place is possible.  We approximate
-- this by seeing if the type has a single constructor with a
-- small arity.  But arity zero isn't good -- we share the single copy
-- for that case, so no point in sharing.

-- Note the repType: we want to look through newtypes for this purpose

canUpdateInPlace ty 
  | not opt_UF_UpdateInPlace = False
  | otherwise
  = case splitTyConApp_maybe (repType ty) of {
			Nothing		-> False ;
			Just (tycon, _) -> 

		      case tyConDataConsIfAvailable tycon of
			[dc]  -> arity == 1 || arity == 2
			      where
				 arity = dataConRepArity dc
			other -> False
		      }
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
transformRhs :: OutExpr 
	     -> (ArityInfo -> OutExpr -> SimplM (OutStuff a))
	     -> SimplM (OutStuff a)

transformRhs rhs thing_inside 
  = tryRhsTyLam rhs			$ \ rhs1 ->
    tryEtaExpansion rhs1 thing_inside
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

So far as the implementation is concerned:

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
tryRhsTyLam rhs thing_inside		-- Only does something if there's a let
  | null tyvars || not (worth_it body)	-- inside a type lambda, and a WHNF inside that
  = thing_inside rhs
  | otherwise
  = go (\x -> x) body		$ \ body' ->
    thing_inside (mkLams tyvars body')

  where
    (tyvars, body) = collectTyBinders rhs

    worth_it (Let _ e)	     = whnf_in_middle e
    worth_it other     	     = False
    whnf_in_middle (Let _ e) = whnf_in_middle e
    whnf_in_middle e	     = exprIsCheap e


    go fn (Let bind@(NonRec var rhs) body) thing_inside
      | exprIsTrivial rhs
      = go (fn . Let bind) body thing_inside

    go fn (Let bind@(NonRec var rhs) body) thing_inside
      = mk_poly tyvars_here var						`thenSmpl` \ (var', rhs') ->
	addAuxiliaryBind (NonRec var' (mkLams tyvars_here (fn rhs))) 	$
	go (fn . Let (mk_silly_bind var rhs')) body thing_inside

      where
	tyvars_here = tyvars
		--	main_tyvar_set = mkVarSet tyvars
		--	var_ty = idType var
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

    go fn (Let (Rec prs) body) thing_inside
       = mapAndUnzipSmpl (mk_poly tyvars_here) vars	`thenSmpl` \ (vars', rhss') ->
	 let
	    gn body = fn (foldr Let body (zipWith mk_silly_bind vars rhss'))
	 in
	 addAuxiliaryBind (Rec (vars' `zip` [mkLams tyvars_here (gn rhs) | rhs <- rhss]))	$
	 go gn body thing_inside
       where
	 (vars,rhss) = unzip prs
	 tyvars_here = tyvars
		-- varSetElems (main_tyvar_set `intersectVarSet` tyVarsOfTypes var_tys)
		-- 	 var_tys     = map idType vars
		-- See notes with tyvars_here above


    go fn body thing_inside = thing_inside (fn body)

    mk_poly tyvars_here var
      = getUniqueSmpl		`thenSmpl` \ uniq ->
	let
	    poly_name = setNameUnique (idName var) uniq		-- Keep same name
	    poly_ty   = mkForAllTys tyvars_here (idType var)	-- But new type of course
	    poly_id   = mkId poly_name poly_ty vanillaIdInfo

		-- In the olden days, it was crucial to copy the occInfo of the original var, 
		-- because we were looking at occurrence-analysed but as yet unsimplified code!
		-- In particular, we mustn't lose the loop breakers.  BUT NOW we are looking
		-- at already simplified code, so it doesn't matter
		-- 
		-- It's even right to retain single-occurrence or dead-var info:
		-- Suppose we started with  /\a -> let x = E in B
		-- where x occurs once in B. Then we transform to:
		--	let x' = /\a -> E in /\a -> let x* = x' a in B
		-- where x* has an INLINE prag on it.  Now, once x* is inlined,
		-- the occurrences of x' will be just the occurrences originally
		-- pinned on x.
		-- 	   poly_info = vanillaIdInfo `setOccInfo` idOccInfo var
	in
	returnSmpl (poly_id, mkTyApps (Var poly_id) (mkTyVarTys tyvars_here))

    mk_silly_bind var rhs = NonRec var rhs
		-- Suppose we start with:
		--
		--	x = let g = /\a -> \x -> f x x
		--	    in 
		--	    /\ b -> let g* = g b in E
		--
		-- Then: 	* the binding for g gets floated out
		-- 		* but then it MIGHT get inlined into the rhs of g*
		--		* then the binding for g* is floated out of the /\b
		--		* so we're back to square one
		-- We rely on the simplifier not to inline g into the RHS of g*,
		-- because it's a "lone" occurrence, and there is no benefit in
		-- inlining.  But it's a slightly delicate property; hence this comment
\end{code}


%************************************************************************
%*									*
\subsection{Eta expansion}
%*									*
%************************************************************************

	Try eta expansion for RHSs

We go for:
   Case 1    f = \x1..xn -> N  ==>   f = \x1..xn y1..ym -> N y1..ym
		 (n >= 0)
     OR		
   Case 2    f = N E1..En      ==>   z1=E1
		 (n > 0)		 .. 
				     zn=En
				     f = \y1..ym -> N z1..zn y1..ym

where (in both cases) 

	* The xi can include type variables

	* The yi are all value variables

	* N is a NORMAL FORM (i.e. no redexes anywhere)
	  wanting a suitable number of extra args.

	* the Ei must not have unlifted type

There is no point in looking for a combination of the two, because
that would leave use with some lets sandwiched between lambdas; that's
what the final test in the first equation is for.

\begin{code}
tryEtaExpansion :: OutExpr 
		-> (ArityInfo -> OutExpr -> SimplM (OutStuff a))
		-> SimplM (OutStuff a)
tryEtaExpansion rhs thing_inside
  |  not opt_SimplDoLambdaEtaExpansion
  || null y_tys				-- No useful expansion
  || not (is_case1 || is_case2)		-- Neither case matches
  = thing_inside final_arity rhs	-- So, no eta expansion, but
					-- return a good arity

  | is_case1
  = make_y_bndrs			$ \ y_bndrs ->
    thing_inside final_arity
		 (mkLams x_bndrs $ mkLams y_bndrs $
		  mkApps body (map Var y_bndrs))

  | otherwise	-- Must be case 2
  = mapAndUnzipSmpl bind_z_arg arg_infos		`thenSmpl` \ (maybe_z_binds, z_args) ->
    addAuxiliaryBinds (catMaybes maybe_z_binds)		$
    make_y_bndrs					$  \ y_bndrs ->
    thing_inside final_arity
	         (mkLams y_bndrs $
		  mkApps (mkApps fun z_args) (map Var y_bndrs))
  where
    all_trivial_args = all is_trivial arg_infos
    is_case1	     = all_trivial_args
    is_case2	     = null x_bndrs && not (any unlifted_non_trivial arg_infos)

    (x_bndrs, body)  = collectBinders rhs	-- NB: x_bndrs can include type variables
    x_arity	     = valBndrCount x_bndrs

    (fun, args)	     = collectArgs body
    arg_infos        = [(arg, exprType arg, exprIsTrivial arg) | arg <- args]

    is_trivial		 (_, _,  triv) = triv
    unlifted_non_trivial (_, ty, triv) = not triv && isUnLiftedType ty

    fun_arity	     = exprEtaExpandArity fun

    final_arity | all_trivial_args = atLeastArity (x_arity + extra_args_wanted)
	        | otherwise	   = atLeastArity x_arity
	-- Arity can be more than the number of lambdas
	-- because of coerces. E.g.  \x -> coerce t (\y -> e) 
	-- will have arity at least 2
	-- The worker/wrapper pass will bring the coerce out to the top

    bind_z_arg (arg, arg_ty, trivial_arg) 
	| trivial_arg = returnSmpl (Nothing, arg)
        | otherwise   = newId SLIT("z") arg_ty	$ \ z ->
			returnSmpl (Just (NonRec z arg), Var z)

    make_y_bndrs thing_inside 
	= ASSERT( not (exprIsTrivial rhs) )
    	  newIds SLIT("y") y_tys			$ \ y_bndrs ->
	  tick (EtaExpansion (head y_bndrs))		`thenSmpl_`
	  thing_inside y_bndrs

    (potential_extra_arg_tys, _) = splitFunTys (exprType body)
	
    y_tys :: [InType]
    y_tys  = take extra_args_wanted potential_extra_arg_tys
	
    extra_args_wanted :: Int	-- Number of extra args we want
    extra_args_wanted = 0 `max` (fun_arity - valArgCount args)

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
